module Cardano.Tracer.Handlers
  ( runHandlers
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems)
import           Cardano.Tracer.Handlers.Logs.Run (runLogsHandler) 
import           Cardano.Tracer.Handlers.Metrics.Run (runMetricsHandler)

runHandlers
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runHandlers config acceptedItems =
  concurrently_ (runLogsHandler    config acceptedItems)
                (runMetricsHandler config acceptedItems)
