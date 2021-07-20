{-# LANGUAGE RecordWildCards #-}

-- | This top-level module will be used by the 'cardano-tracer' app.
module Cardano.Tracer.Run
  ( runCardanoTracer
  -- | For testing purposes.
  , runCardanoTracerWithConfig
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Acceptors (runAcceptors)
import           Cardano.Tracer.CLI (TracerParams (..))
import           Cardano.Tracer.Configuration (TracerConfig, readTracerConfig)
import           Cardano.Tracer.Handlers (runHandlers)
import           Cardano.Tracer.Types (initAcceptedItems)

runCardanoTracer :: TracerParams -> IO ()
runCardanoTracer TracerParams{..} =
  readTracerConfig tracerConfig >>= runCardanoTracerWithConfig

runCardanoTracerWithConfig :: TracerConfig -> IO ()
runCardanoTracerWithConfig config = do
  acceptedItems <- initAcceptedItems
  -- Run two main threads:
  -- 1. For all acceptors: they ask 'TraceObject's and metrics from the node
  --    and collect them in 'acceptedItems'.
  -- 2. For all handlers: they take items from 'acceptedItems' and do something
  --    with them (write to log files and return by web-request via EKG API).
  concurrently_ (runAcceptors config acceptedItems)
                (runHandlers  config acceptedItems)
