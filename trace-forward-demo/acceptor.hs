{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import "contra-tracer" Control.Tracer (contramap, stdoutTracer)
import           Data.IORef (newIORef)
import           Data.Text (pack)
import           System.Environment (getArgs)
import           System.Exit (die)

import           Cardano.Logging (TraceObject)

import           Trace.Forward.Acceptor (runTraceAcceptor)
import           Trace.Forward.Configuration (AcceptorConfiguration (..),
                                              HowToConnect (..), Port)
import           Trace.Forward.Protocol.Type (Request (..))

main :: IO ()
main = do
  -- Prepare the acceptor's configuration.
  listenIt <- getArgs >>= \case
    [path]       -> return $ LocalPipe path
    [host, port] -> return $ RemoteSocket (pack host) (read port :: Port)
    _            -> die "Usage: demo-acceptor (pathToLocalPipe | host port)"
  weAreDone <- newIORef False
  let config :: AcceptorConfiguration TraceObject
      config =
        AcceptorConfiguration
          { acceptorTracer    = contramap show stdoutTracer
          , forwarderEndpoint = listenIt
          , whatToRequest     = GetTraceObjects 10
          , actionOnReply     = print
          , shouldWeStop      = weAreDone
          , actionOnDone      = putStrLn "We are done!"
          }

  -- Create an empty TBQueue where received 'LogObject's will be stored.
  queue <- newTBQueueIO 100

  -- Create an empty store where received node's info will be stored.
  niStore <- newIORef []

  -- Run the acceptor. It will listen to the forwarder, and after the connection
  -- will be established, the acceptor will ask for N 'LogObject's from the forwarder.
  -- After these 'LogObject's will be received, the acceptor will write them in the 'queue'.
  runTraceAcceptor config queue niStore
