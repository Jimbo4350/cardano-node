{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Forwarder
  ( launchForwardersSimple
  ) where

import           Codec.CBOR.Term (Term)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import           Data.Fixed (Pico)
import           Data.Text (pack)
import           Data.Time.Clock (NominalDiffTime, getCurrentTime, secondsToNominalDiffTime)
import           Data.Void (Void)
import           Data.Word (Word16)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolLimits (..),
                                        MiniProtocolNum (..), MuxMode (..),
                                        OuroborosApplication (..), RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (cborTermVersionDataCodec,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (Snocket, socketSnocket)
import           Ouroboros.Network.Socket (connectToNode, nullNetworkConnectTracers)
import qualified System.Metrics as EKG

import           Cardano.Logging

import qualified Trace.Forward.Configuration as TF
import           Trace.Forward.Network.Forwarder (forwardTraceObjects)

import qualified System.Metrics.Configuration as EKGF
import           System.Metrics.Network.Forwarder (forwardEKGMetrics)

type Host = String
type Port = Word16
type Endpoint = (Host, Port)

launchForwardersSimple :: Endpoint -> IO ()
launchForwardersSimple (h, p) =
  try (launchForwarders' (h, p) Nothing (ekgConfig, tfConfig)) >>= \case
    Left (_e :: SomeException) ->
      launchForwarders' (h, p) Nothing (ekgConfig, tfConfig)
    Right _ -> return ()
 where
  ekgConfig :: EKGF.ForwarderConfiguration
  ekgConfig =
    EKGF.ForwarderConfiguration
      { EKGF.forwarderTracer    = nullTracer
      , EKGF.acceptorEndpoint   = EKGF.RemoteSocket (pack h) p
      , EKGF.reConnectFrequency = 1.0
      , EKGF.actionOnRequest    = const (return ())
      }

  tfConfig :: TF.ForwarderConfiguration TraceObject
  tfConfig =
    TF.ForwarderConfiguration
      { TF.forwarderTracer  = nullTracer
      , TF.acceptorEndpoint = TF.RemoteSocket (pack h) p
      , TF.nodeBasicInfo    = return [("NodeName", "node-1")]
      , TF.actionOnRequest  = const (return ())
      }

launchForwarders'
  :: Endpoint
  -> Maybe Pico
  -> (EKGF.ForwarderConfiguration, TF.ForwarderConfiguration TraceObject)
  -> IO ()
launchForwarders' (host, port) benchFillFreq configs = withIOManager $ \iocp -> do
  acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just host) (Just $ show port)
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
         forwarderApp [ (forwardEKGMetrics ekgConfig store,  1)
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
    { toHuman     = Just "Human Message"
    , toMachine   = Just "{\"msg\": \"forMachine\"}"
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

  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000
