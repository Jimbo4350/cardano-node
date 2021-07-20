module Cardano.TraceDispatcher.Resources
  (
    startResourceTracer
  , docResourceStats
  , namesForResources
  , severityResources
  ) where


import           Cardano.Logging
import           Cardano.Prelude hiding (trace)

startResourceTracer ::
     Trace IO ResourceStats
  -> IO ()
startResourceTracer tr = do
    void $ forkIO $ forever $ do
      mbrs <- readResourceStats
      case mbrs of
        Just rs -> traceWith tr rs
        Nothing -> pure ()
      threadDelay 1000000 -- TODO JNF:  make configurable
                               -- in microseconds

namesForResources :: ResourceStats -> [Text]
namesForResources _ = []

severityResources :: ResourceStats -> SeverityS
severityResources _ = Info
