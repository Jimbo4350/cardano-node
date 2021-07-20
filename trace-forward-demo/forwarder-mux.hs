{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, withAsync)
import           Control.Monad (forever)
import "contra-tracer" Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Text (pack)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, secondsToNominalDiffTime)
import           Data.Void (Void)
import           Data.Word (Word16)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad.STM (atomically)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as Socket
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import qualified System.Metrics as EKG

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))

import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder (forwardTraceObjects)

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetrics)

main :: IO ()
main = do
  (howToConnect, freq, benchFillFreq, reConnectTest) <- do
    args <- getArgs
    if "--dc" `elem` args
      then
        case args of
          [host, port, "--dc", freq] ->
            return ( RemoteSocket host port
                   , 0.5
                   , Nothing
                   , Just (read freq :: Pico) -- This is how often the client will be shut down.
                   )
          _ -> die "Usage: demo-forwarder-mux host port --dc freqInSecs"
      else
        case args of
          [path, freq] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , Nothing
                   , Nothing
                   )
          [host, port, freq] ->
            return ( RemoteSocket host port
                   , read freq :: Pico
                   , Nothing
                   , Nothing
                   )
          [path, freq, "-b", ff] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , Just (read ff :: Pico)
                   , Nothing
                   )
          _ ->
            die "Usage: demo-forwarder-mux (pathToLocalPipe | host port) freqInSecs [-b fillFreqInSecs]"
  let configs = mkConfigs howToConnect freq benchFillFreq
  
  case reConnectTest of
    Nothing -> launchForwarders howToConnect benchFillFreq configs
    Just rcFreq -> runReConnector (launchForwarders howToConnect benchFillFreq configs) rcFreq

mkConfigs
  :: HowToConnect
  -> Pico
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
mkConfigs howToConnect freq benchFillFreq = (ekgConfig, tfConfig)
 where
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = if benchMode then nullTracer else contramap show stdoutTracer
      , EKGF.acceptorEndpoint   = forEKGF howToConnect
      , EKGF.reConnectFrequency = secondsToNominalDiffTime freq
      , EKGF.actionOnRequest    = const (return ())
      }
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer  = if benchMode then nullTracer else contramap show stdoutTracer
      , TF.acceptorEndpoint = forTF howToConnect
      , TF.nodeBasicInfo    = return [("NodeName", "node-1")]
      , TF.actionOnRequest  = const (return ())
      }

  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)

  benchMode = isJust benchFillFreq

toMicroSecs :: NominalDiffTime -> Int
toMicroSecs dt = fromEnum dt `div` 1000000

runReConnector :: IO () -> Pico -> IO ()
runReConnector forwarder rcFreq = forever $ do
  putStrLn "ReConnect test, start forwarder..."
  withAsync forwarder $ \_ -> do
    threadDelay . toMicroSecs . secondsToNominalDiffTime $ rcFreq
    putStrLn "ReConnect test, stop forwarder..."

-- Network part

data HowToConnect
  = LocalPipe !FilePath
  | RemoteSocket !String !String

launchForwarders
  :: HowToConnect
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
launchForwarders endpoint benchFillFreq configs =
  try (launchForwarders' endpoint benchFillFreq configs) >>= \case
    Left (_e :: SomeException) ->
      launchForwarders endpoint benchFillFreq configs
    Right _ -> return ()

launchForwarders'
  :: HowToConnect
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
launchForwarders' endpoint benchFillFreq configs = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket address noTimeLimitsHandshake benchFillFreq configs
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket address timeLimitsHandshake benchFillFreq configs

doConnectToAcceptor
  :: Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
doConnectToAcceptor snocket address timeLimits benchFillFreq (ekgConfig, tfConfig) = do
  tfQueue <- newTBQueueIO 1000000
  _ <- async $ traceObjectsWriter tfQueue benchFillFreq
  store <- EKG.newStore
  EKG.registerGcMetrics store

  connectToNode
    snocket
    unversionedHandshakeCodec
    timeLimits
    (cborTermVersionDataCodec unversionedProtocolDataCodec)
    nullNetworkConnectTracers
    acceptableVersion
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData $
         forwarderApp [ (forwardEKGMetrics ekgConfig store,    1)
                      , (forwardTraceObjects tfConfig tfQueue, 2)
                      ]
    )
    Nothing
    address
 where
  forwarderApp
    :: [(RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void, Word16)]
    -> OuroborosApplication 'InitiatorMode addr LBS.ByteString IO () Void
  forwarderApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]

traceObjectsWriter :: TBQueue TraceObject -> Maybe Pico -> IO ()
traceObjectsWriter queue benchFillFreq = forever $ do
  now <- getCurrentTime
  atomically $ writeTBQueue queue (mkTraceObject now)
  threadDelay fillPause
 where
  mkTraceObject now' = TraceObject
    { toHuman     = Just "Human Message 1"
    , toMachine   = Nothing
    , toNamespace = ["demoNamespace"]
    , toSeverity  = Info
    , toDetails   = DRegular
    , toTimestamp = now'
    , toHostname  = "linux"
    , toThreadId  = "1"
    }

  fillPause = case benchFillFreq of
                Just ff -> toMicroSecs . secondsToNominalDiffTime $ ff
                Nothing -> 500000
