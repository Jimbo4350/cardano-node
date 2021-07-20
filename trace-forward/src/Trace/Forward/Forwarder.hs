{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This top-level module will be used by the forwarder app
-- (the app that collects 'TraceObject's and sends them to the acceptor).
module Trace.Forward.Forwarder
  ( runTraceForwarder
  ) where

import qualified Codec.Serialise as CBOR
import           Control.Concurrent.STM.TBQueue (TBQueue)
import           Control.Exception (SomeException, try)
import           Data.Typeable (Typeable)

import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

import           Trace.Forward.Configuration (ForwarderConfiguration (..))
import           Trace.Forward.Network.Forwarder (connectToAcceptor)

-- | Please note that forwarder is a client from the __networking__ point of view:
-- it establishes network connection with the acceptor.
runTraceForwarder
  :: (CBOR.Serialise lo,
      ShowProxy lo,
      Typeable lo)
  => ForwarderConfiguration lo -- ^ Forwarder configuration.
  -> TBQueue lo                -- ^ The queue the forwarder will take 'TraceObject's from.
  -> IO ()
runTraceForwarder config loQueue =
  try (connectToAcceptor config loQueue) >>= \case
    Left (e :: SomeException) -> do
      putStrLn $ "trace-forward, forwarder problem: " <> show e
      runTraceForwarder config loQueue
    Right _ -> return ()
