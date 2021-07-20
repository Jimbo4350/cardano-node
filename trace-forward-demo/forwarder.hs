{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, writeTBQueue)
import           Control.Monad (forever)
import           Control.Monad.STM (atomically)
import "contra-tracer" Control.Tracer (contramap, stdoutTracer)
import           Data.Text (pack)
import           Data.Time.Clock (getCurrentTime)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Cardano.Logging (DetailLevel (..), SeverityS (..), TraceObject (..))

import           Trace.Forward.Forwarder (runTraceForwarder)
import           Trace.Forward.Configuration (ForwarderConfiguration (..),
                                              HowToConnect (..), Port)

main :: IO ()
main = do
  -- Prepare the forwarder's configuration.
  howToConnect <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (pack host) (read port :: Port)
    _            -> die "Usage: demo-forwarder (pathToLocalPipe | host port)"
  let config :: ForwarderConfiguration TraceObject
      config =
        ForwarderConfiguration
          { forwarderTracer  = contramap show stdoutTracer
          , acceptorEndpoint = howToConnect
          , nodeBasicInfo    = return [("NodeName", "node-1")]
          , actionOnRequest  = print -- const (return ())
          }

  -- Create a queue for 'TraceObject's: when the acceptor will ask for N 'TraceObject's
  -- they will be taken from this queue.
  queue <- newTBQueueIO 1000

  -- This thread will write 'TraceObject's to the queue.
  _ <- async $ traceObjectsWriter queue

  -- Run the forwarder. It will establish the connection with the acceptor,
  -- then the acceptor will periodically ask for 'TraceObject's, the forwarder
  -- will take them from the 'queue' and send them back.
  runTraceForwarder config queue

traceObjectsWriter :: TBQueue TraceObject -> IO ()
traceObjectsWriter queue = forever $ do
  now <- getCurrentTime
  atomically $ writeTBQueue queue (mkTraceObject now)
  threadDelay 500000
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
