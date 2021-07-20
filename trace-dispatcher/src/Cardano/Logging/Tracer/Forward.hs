{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Logging.Tracer.Forward
  (
    forwardTracer
  ) where

import           Codec.CBOR.Term (Term)
import           Codec.Serialise (Serialise (..))
import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (async, wait, waitAnyCancel)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO,
                     writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           GHC.Generics (Generic)

-- Temporary solution, to avoid conflicts with trace-dispatcher.
import qualified Control.Tracer as T
import           "contra-tracer" Control.Tracer (contramap, stdoutTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Void (Void)
import           Data.Word (Word16)

import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..),
                     MiniProtocolLimits (..), MiniProtocolNum (..),
                     MuxMode (..), OuroborosApplication (..),
                     RunMiniProtocol (..), miniProtocolLimits, miniProtocolNum,
                     miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (cborTermVersionDataCodec, noTimeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned
                     (UnversionedProtocol (..), UnversionedProtocolData (..),
                     unversionedHandshakeCodec, unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version
                     (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath,
                     localSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                     SomeResponderApplication (..), cleanNetworkMutableState,
                     newNetworkMutableState, nullNetworkServerTracers,
                     withServerNode)
import           Ouroboros.Network.Util.ShowProxy (ShowProxy (..))

import qualified System.Metrics as EKG
import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetricsResp)
import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder (forwardTraceObjectsResp)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

-- Instances for 'TraceObject' to forward it using 'trace-forward' library.

deriving instance Generic Privacy
deriving instance Generic SeverityS
deriving instance Generic LoggingContext
deriving instance Generic TraceObject

instance Serialise DetailLevel
instance Serialise Privacy
instance Serialise SeverityS
instance Serialise LoggingContext
instance Serialise TraceObject

instance ShowProxy TraceObject

---------------------------------------------------------------------------

-- newtype ForwardTracerState = ForwardTracerState {
--     ftQueue   :: TBQueue TraceObject
--   }

forwardTracer :: forall m. (MonadIO m)
  => TraceConfig
  -> m (Trace m FormattedMessage)
forwardTracer config = do
    tbQueue <- liftIO $ newTBQueueIO
                          (fromIntegral (tcForwarderQueueSize config))
    store <- liftIO $ EKG.newStore
    liftIO $ EKG.registerGcMetrics store
    liftIO $ launchForwardersSimple (tcForwarder config) tbQueue store
--    stateRef <- liftIO $ newIORef (ForwardTracerState tbQueue)
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output tbQueue)
  where
    output ::
         TBQueue TraceObject
      -> LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output tbQueue LoggingContext {} Nothing (FormattedForwarder lo) = liftIO $ do
      atomically $ writeTBQueue tbQueue lo
    output _tbQueue LoggingContext {} (Just Reset) _msg = liftIO $ do
      pure ()
    output _tbQueue lk (Just c@Document {}) (FormattedForwarder lo) = do
      docIt Forwarder (FormattedHuman False "") (lk, Just c, lo)
    output _tbQueue LoggingContext {} _ _a = pure ()

launchForwardersSimple :: RemoteAddr -> TBQueue TraceObject -> EKG.Store -> IO ()
launchForwardersSimple endpoint tbQueue store =
  void . forkIO $ launchForwardersSimple'
 where
  launchForwardersSimple' =
    try (launchForwarders' endpoint (ekgConfig, tfConfig) tbQueue store) >>= \case
      Left (_e :: SomeException) ->
        -- There is some problem with the connection with the acceptor, try it again.
        -- TODO JNF What if it runs in an infinite loop?
        launchForwardersSimple'
      Right _ ->
        pure () -- Actually, the function 'connectToNode' never returns.

  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = forEKGF endpoint
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const (return ())
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer  = contramap show stdoutTracer
      , TF.acceptorEndpoint = forTF endpoint
      , TF.nodeBasicInfo    = return
                                [ ("NodeName",      "core-1")
                                , ("NodeProtocol",  "Shelley")
                                , ("NodeRelease",   "1.27.0")
                                , ("NodeCommit",    "abcdefg")
                                , ("NodeStartTime", "2021 06 04 14:34:07 UTC")
                                ]
      , TF.actionOnRequest  = const (return ())
      }

  forTF (LocalSocket p)   = TF.LocalPipe p
  forEKGF (LocalSocket p) = EKGF.LocalPipe p

launchForwarders'
  :: RemoteAddr
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> TBQueue TraceObject
  -> EKG.Store
  -> IO ()
launchForwarders' (LocalSocket localSock) configs tbQueue store = withIOManager $ \iocp -> do
  let snocket = localSnocket iocp localSock
      address = localAddressFromPath localSock
  doListenToAcceptor snocket address noTimeLimitsHandshake configs tbQueue store

doListenToAcceptor
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> TBQueue TraceObject
  -> EKG.Store
  -> IO ()
doListenToAcceptor snocket address timeLimits (ekgConfig, tfConfig) tbQueue store = do
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
        (SomeResponderApplication $
          forwarderApp [ (forwardEKGMetricsResp   ekgConfig store,  1)
                       , (forwardTraceObjectsResp tfConfig tbQueue, 2)
                       ]
        )
      )
      nullErrorPolicies
      $ \_ serverAsync ->
        wait serverAsync -- Block until async exception.
  void $ waitAnyCancel [nsAsync, clAsync]
 where
  forwarderApp
    :: [(RunMiniProtocol 'ResponderMode LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  forwarderApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f ~(a,b,c) = f a b c
