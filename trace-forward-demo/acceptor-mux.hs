{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (ThreadId, killThread, myThreadId, threadDelay)
import           Control.Concurrent.Async (async, asyncThreadId, wait, withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import           Control.Concurrent.STM.TVar
import "contra-tracer" Control.Tracer (contramap, nullTracer, stdoutTracer)
import           Control.Monad (forever, void, when)
import           Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import           Data.Fixed (Pico)
import           Data.Maybe (isJust)
import           Data.Text (pack)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime,
                                  diffUTCTime, secondsToNominalDiffTime)
import           Data.Void (Void)
import           Data.Word (Word16, Word64)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket as Socket
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Snocket (Snocket, localAddressFromPath, localSnocket, socketSnocket)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import qualified System.Metrics as EKG

import           Cardano.Logging (TraceObject)

import qualified Trace.Forward.Configuration as TF
import qualified Trace.Forward.Protocol.Type as TF
import           Trace.Forward.Network.Acceptor (acceptTraceObjects)

import           System.Metrics.Network.Acceptor (acceptEKGMetrics)
import           System.Metrics.Store.Acceptor (emptyMetricsLocalStore)
import qualified System.Metrics.Configuration as EKGF
import qualified System.Metrics.ReqResp as EKGF

main :: IO ()
main = do
  (listenIt, freq, itemsNum, benchSpeedFreq, totalObjs, reConnectTest) <- do
    args <- getArgs
    if "--dc" `elem` args
      then
        case args of
          [host, port, "--dc", freq] ->
            return ( RemoteSocket host port
                   , 1   -- This is disconnect test, so the frequency of requests doesn't matter.
                   , 100 -- This is disconnect test, so the number of requested TraceObjects doesn't matter.
                   , Nothing
                   , Nothing
                   , Just (read freq :: Pico) -- This is how often the server will be shut down.
                   )
          _ -> die "Usage: demo-acceptor-mux host port --dc freqInSecs"
      else
        case args of
          [path, freq, n] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Nothing
                   , Nothing
                   , Nothing
                   )
          [host, port, freq, n] ->
            return ( RemoteSocket host port
                   , read freq :: Pico
                   , read n :: Word16
                   , Nothing
                   , Nothing
                   , Nothing
                   )
          [path, freq, n, "-b", sp] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Just (read sp :: Pico)
                   , Nothing
                   , Nothing
                   )
          [path, freq, n, "-b", sp, "-t", tn] ->
            return ( LocalPipe path
                   , read freq :: Pico
                   , read n :: Word16
                   , Just (read sp :: Pico)
                   , Just (read tn :: Word64)
                   , Nothing
                   )
          _ ->
            die "Usage: demo-acceptor-mux (pathToPipe | host port) freqInSecs itemsNum [-b freqInSecs] [-t totalObjs]"

  configs <- mkConfigs listenIt freq itemsNum benchSpeedFreq totalObjs

  tidVar <- newTVarIO =<< myThreadId -- Just for filling TVar, it will be replaced anyway.

  case reConnectTest of
    Nothing -> launchAcceptors listenIt configs tidVar
    Just rcFreq -> runReConnector (launchAcceptors listenIt configs tidVar) rcFreq tidVar

mkConfigs
  :: HowToConnect
  -> Pico
  -> Word16
  -> Maybe Pico
  -> Maybe Word64
  -> IO (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
mkConfigs listenIt freq itemsNum benchSpeedFreq totalObjs = do
  stopEKGF <- newIORef False
  stopTF <- newIORef False
  loCounter <- newIORef (0 :: Word64)

  when benchMode $
    void . async $ runSpeedPrinter loCounter 0 stopTF

  case totalObjs of
    Nothing -> return ()
    Just tn -> do
      startTime <- getCurrentTime
      putStrLn $ "Start time: " <> show startTime
      void . async $ stopWhenTotalReached startTime loCounter tn stopTF stopEKGF

  let ekgConfig =
        EKGF.AcceptorConfiguration
          { EKGF.acceptorTracer    = if benchMode then nullTracer else contramap show stdoutTracer
          , EKGF.forwarderEndpoint = forEKGF listenIt
          , EKGF.requestFrequency  = secondsToNominalDiffTime freq
          , EKGF.whatToRequest     = EKGF.GetAllMetrics
            -- Currently, only TF works in bench mode.
          , EKGF.actionOnResponse  = if benchMode then (\_ -> return ()) else print
          , EKGF.shouldWeStop      = stopEKGF
          , EKGF.actionOnDone      = putStrLn "EKGF: we are done!"
          }
      tfConfig :: TF.AcceptorConfiguration TraceObject
      tfConfig =
        TF.AcceptorConfiguration
          { TF.acceptorTracer    = if benchMode then nullTracer else contramap show stdoutTracer
          , TF.forwarderEndpoint = forTF listenIt
          , TF.whatToRequest     = TF.GetTraceObjects itemsNum
            -- Currently, only TF works in bench mode.
          , TF.actionOnReply     = if benchMode then count loCounter else print
          , TF.shouldWeStop      = stopTF
          , TF.actionOnDone      = putStrLn "TF: we are done!"
          }
  return (ekgConfig, tfConfig)
 where
  forTF (LocalPipe p)      = TF.LocalPipe p
  forTF (RemoteSocket h p) = TF.RemoteSocket (pack h) (read p :: TF.Port)

  forEKGF (LocalPipe p)      = EKGF.LocalPipe p
  forEKGF (RemoteSocket h p) = EKGF.RemoteSocket (pack h) (read p :: EKGF.Port)

  benchMode = isJust benchSpeedFreq

  count :: IORef Word64 -> [TraceObject] -> IO ()
  count loCounter los =
    atomicModifyIORef' loCounter $ \cnt -> (cnt + fromIntegral (length los), ())

  runSpeedPrinter loCounter diff stopTF =
    case benchSpeedFreq of
      Nothing -> return ()
      Just sp -> do
        let waitInMicroSecs = toMicroSecs . secondsToNominalDiffTime $ sp
        threadDelay waitInMicroSecs
        --Check should we stop...
        shouldIStop <- readIORef stopTF
        if shouldIStop
          then return ()
          else do
            n <- readIORef loCounter
            let newObjsNum = n - diff
            putStrLn $ "Bench mode: " <> show newObjsNum
                       <> " new TraceObjects were received during last "
                       <> show waitInMicroSecs <> " mks."
            runSpeedPrinter loCounter n stopTF

  stopWhenTotalReached startTime loCounter totalObjsNum stopTF stopEKGF = do
    n <- readIORef loCounter
    if n < totalObjsNum
      then do
        threadDelay 1000
        stopWhenTotalReached startTime loCounter totalObjsNum stopTF stopEKGF
      else do
        stopTime <- getCurrentTime
        let timeDiff = stopTime `diffUTCTime` startTime
        putStrLn $ "Stop time: " <> show stopTime
        putStrLn $ show n <> " TraceObjects were received during "
                          <> show timeDiff
        atomicModifyIORef' stopTF   $ const (True, ())
        atomicModifyIORef' stopEKGF $ const (True, ())

toMicroSecs :: NominalDiffTime -> Int
toMicroSecs dt = fromEnum dt `div` 1000000

runReConnector :: IO () -> Pico -> TVar ThreadId -> IO ()
runReConnector acceptor rcFreq tidVar = forever $ do
  putStrLn "ReConnect test, start acceptor..."
  withAsync acceptor $ \_ -> do
    threadDelay . toMicroSecs . secondsToNominalDiffTime $ rcFreq
    putStrLn "ReConnect test, stop acceptor..."
    tid <- readTVarIO tidVar
    putStrLn $ "KILL TID: " <> show tid
    killThread tid

-- Network part

data HowToConnect
  = LocalPipe !FilePath
  | RemoteSocket !String !String

launchAcceptors
  :: HowToConnect
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> IO ()
launchAcceptors endpoint configs tidVar =
  try (launchAcceptors' endpoint configs tidVar) >>= \case
    Left (_e :: SomeException) ->
      launchAcceptors endpoint configs tidVar
    Right _ -> return ()

launchAcceptors'
  :: HowToConnect
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> IO ()
launchAcceptors' endpoint configs tidVar = withIOManager $ \iocp -> do
  case endpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp localPipe
          address = localAddressFromPath localPipe
      void $ doListenToForwarder snocket address noTimeLimitsHandshake configs tidVar
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just host) (Just port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
      void $ doListenToForwarder snocket address timeLimitsHandshake configs tidVar

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> (EKGF.AcceptorConfiguration, TF.AcceptorConfiguration TraceObject)
  -> TVar ThreadId
  -> IO Void
doListenToForwarder snocket address timeLimits (ekgConfig, tfConfig) tidVar = do
  store <- EKG.newStore
  metricsStore <- newIORef emptyMetricsLocalStore
  loQueue <- newTBQueueIO 1000000
  niStore <- newIORef []

  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
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
         acceptorApp [ (acceptEKGMetrics ekgConfig store metricsStore, 1)
                     , (acceptTraceObjects tfConfig loQueue niStore,   2)
                     ]
      )
    )
    nullErrorPolicies
    $ \_ serverAsync -> do
      let tid = asyncThreadId serverAsync
      -- Store it to will be able to kill it later.
      putStrLn $ "STORE TID: " <> show tid
      atomically $ modifyTVar' tidVar (const tid)
      wait serverAsync -- Block until async exception.
 where
  acceptorApp
    :: [(RunMiniProtocol 'ResponderMode LBS.ByteString IO Void (), Word16)]
    -> OuroborosApplication 'ResponderMode addr LBS.ByteString IO Void ()
  acceptorApp protocols =
    OuroborosApplication $ \_connectionId _shouldStopSTM ->
      [ MiniProtocol
         { miniProtocolNum    = MiniProtocolNum num
         , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
         , miniProtocolRun    = prot
         }
      | (prot, num) <- protocols
      ]
