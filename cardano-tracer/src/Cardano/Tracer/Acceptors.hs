{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE PackageImports #-}

module Cardano.Tracer.Acceptors
  ( runAcceptors
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import           Control.Concurrent.Async (async, asyncThreadId, wait, waitAnyCancel)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVarIO)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, forM_, void)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.IORef (IORef, newIORef, readIORef)
import           Data.HashMap.Strict ((!))
import           Data.Time.Clock (secondsToNominalDiffTime)
import           Data.Void (Void)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..), ConnectionId (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, connectToNode,
                                           newNetworkMutableState, nullNetworkServerTracers,
                                           nullNetworkConnectTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion,
                                                               simpleSingletonVersions)
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Logging (TraceObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF
import           Trace.Forward.Network.Acceptor (acceptTraceObjects, acceptTraceObjectsInit)

import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF
import           System.Metrics.Network.Acceptor (acceptEKGMetrics, acceptEKGMetricsInit)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems, TraceObjects, Metrics,
                                       addressToNodeId, prepareAcceptedItems)

runAcceptors
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runAcceptors config acceptedItems = do
  runAcceptors' config acceptedItems
  waitForever
 where
  waitForever = forever $ threadDelay 1000000000

runAcceptors'
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runAcceptors' config@TracerConfig{..} acceptedItems =
  forM_ acceptAt $ \localSocket -> void . async . forever $ do
    runAcceptorForOneNode localSocket
    threadDelay 1000000
 where
  runAcceptorForOneNode localSocket = do
    stopEKG <- newIORef False
    stopTF  <- newIORef False

    -- Temporary fill 'tidVar' using current 'ThreadId'. Later it will be
    -- replaced by the real 'ThreadId' from 'serverAsync' (see below).
    tmpTId <- myThreadId
    tidVar :: TVar ThreadId <- newTVarIO tmpTId

    let configs = mkAcceptorsConfigs config localSocket stopEKG stopTF
    try (runAcceptor connectMode localSocket configs tidVar acceptedItems) >>= \case
      Left (e :: SomeException) -> do
        -- There is some problem (probably the connection was dropped).
        putStrLn $ "cardano-tracer, runAcceptor problem: " <> show e
        -- Explicitly stop 'serverAsync'.
        killThread =<< readTVarIO tidVar
      Right _ -> return ()
    
mkAcceptorsConfigs
  :: TracerConfig
  -> Address
  -> IORef Bool
  -> IORef Bool
  -> ( EKGF.AcceptorConfiguration
     , TF.AcceptorConfiguration TraceObject
     )
mkAcceptorsConfigs TracerConfig{..} localSocket stopEKG stopTF = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.AcceptorConfiguration
      { EKGF.acceptorTracer    = nullTracer
      , EKGF.forwarderEndpoint = forEKGF localSocket
      , EKGF.requestFrequency  = secondsToNominalDiffTime ekgRequestFreq
      , EKGF.whatToRequest     = EKGF.GetAllMetrics
      , EKGF.actionOnResponse  = print
      , EKGF.shouldWeStop      = stopEKG
      , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
      }

  tfConfig :: TF.AcceptorConfiguration TraceObject
  tfConfig =
    TF.AcceptorConfiguration
      { TF.acceptorTracer    = nullTracer
      , TF.forwarderEndpoint = forTF localSocket
      , TF.whatToRequest     = TF.GetTraceObjects loRequestNum
      , TF.actionOnReply     = print
      , TF.shouldWeStop      = stopTF
      , TF.actionOnDone      = putStrLn "TF: we are done!"
      }

  forTF (LocalSocket p)   = TF.LocalPipe p
  forEKGF (LocalSocket p) = EKGF.LocalPipe p

runAcceptor
  :: ConnectMode
  -> Address
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> AcceptedItems
  -> IO ()
runAcceptor mode (LocalSocket localSock) (ekgConfig, tfConfig) tidVar acceptedItems = withIOManager $ \iocp -> do
  let snock = localSnocket iocp localSock
      addr  = localAddressFromPath localSock
  case mode of
    Initiator ->
      doConnectToAcceptor snock addr noTimeLimitsHandshake $
        appInitiator
          [ (runEKGAcceptorInit          ekgConfig acceptedItems, 1)
          , (runTraceObjectsAcceptorInit tfConfig  acceptedItems, 2)
          ]
    Responder ->
      doListenToForwarder snock addr noTimeLimitsHandshake tidVar $
        appResponder
          [ (runEKGAcceptor          ekgConfig acceptedItems, 1)
          , (runTraceObjectsAcceptor tfConfig  acceptedItems, 2)
          ]
 where
  appResponder protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

  appInitiator protocols =
    OuroborosApplication $ \connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = protocol connectionId
         }
      | (protocol, num) <- protocols
      ]

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  -> IO ()
doConnectToAcceptor snocket address timeLimits app =
  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData app
    )
    Nothing
    address

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> TVar ThreadId
  -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  -> IO ()
doListenToForwarder snocket address timeLimits tidVar app = do
  networkState <- newNetworkMutableState
  nsAsync <- async $ cleanNetworkMutableState networkState
  clAsync <- async . void $
    withServerNode
      snocket
      nullNetworkServerTracers
      networkState
      (AcceptedConnectionsLimit maxBound maxBound 0)
      address
      unversionedHandshakeCodec
      timeLimits
      (cborTermVersionDataCodec unversionedProtocolDataCodec)
      acceptableVersion
      (simpleSingletonVersions
        UnversionedProtocol
        UnversionedProtocolData
        (SomeResponderApplication app)
      )
      nullErrorPolicies
      $ \_ serverAsync -> do
        -- Store 'serverAsync' to be able to kill it later.
        atomically $ modifyTVar' tidVar $ const (asyncThreadId serverAsync)
        wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]

runEKGAcceptor
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runEKGAcceptor ekgConfig acceptedItems connId = do
  let (_, _, (ekgStore, localStore)) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptEKGMetrics ekgConfig ekgStore localStore

runTraceObjectsAcceptor
  :: Show addr
  => TF.AcceptorConfiguration TraceObject
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'ResponderMode LBS.ByteString IO Void ()
runTraceObjectsAcceptor tfConfig acceptedItems connId = do
  let (niStore, trObQueue, _) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptTraceObjects tfConfig trObQueue niStore

runEKGAcceptorInit
  :: Show addr
  => EKGF.AcceptorConfiguration
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runEKGAcceptorInit ekgConfig acceptedItems connId = do
  let (_, _, (ekgStore, localStore)) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptEKGMetricsInit ekgConfig ekgStore localStore

runTraceObjectsAcceptorInit
  :: Show addr
  => TF.AcceptorConfiguration TraceObject
  -> AcceptedItems
  -> ConnectionId addr
  -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
runTraceObjectsAcceptorInit tfConfig acceptedItems connId = do
  let (niStore, trObQueue, _) =
        unsafePerformIO $ prepareStores acceptedItems connId
  acceptTraceObjectsInit tfConfig trObQueue niStore

prepareStores
  :: Show addr
  => AcceptedItems
  -> ConnectionId addr
  -> IO (TF.NodeInfoStore, TraceObjects, Metrics)
prepareStores acceptedItems ConnectionId{..} = do
  -- Remote address of the node is unique identifier, from the tracer's point of view.
  let nodeId = addressToNodeId $ show remoteAddress
  prepareAcceptedItems nodeId acceptedItems
  items <- readIORef acceptedItems
  return $ items ! nodeId
