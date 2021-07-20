{-# LANGUAGE RecordWildCards #-}

module Cardano.Tracer.Handlers.Metrics.Run
  ( runMetricsHandler
  ) where

import           Control.Concurrent.Async (concurrently_)

import           Cardano.Tracer.Configuration (TracerConfig (..))
import           Cardano.Tracer.Handlers.Metrics.Monitoring (runMonitoringServer)
import           Cardano.Tracer.Handlers.Metrics.Prometheus (runPrometheusServer)
import           Cardano.Tracer.Types (AcceptedItems)

runMetricsHandler
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runMetricsHandler TracerConfig{..} acceptedItems =
  case (hasEKG, hasPrometheus) of
    (Nothing,  Nothing)   -> return ()
    (Nothing,  Just prom) -> runPrometheusServer prom acceptedItems
    (Just ekg, Nothing)   -> runMonitoringServer ekg acceptedItems
    (Just ekg, Just prom) -> concurrently_ (runPrometheusServer prom acceptedItems)
                                           (runMonitoringServer ekg acceptedItems)
