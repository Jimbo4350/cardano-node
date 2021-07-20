{-# OPTIONS_GHC -Wno-orphans  #-}

module Examples.EKG (
  testEKG
) where

import           Cardano.Logging
import           Control.Concurrent
import           System.Remote.Monitoring (forkServer)


countDocumented :: Documented Int
countDocumented = Documented [DocMsg 0 [] "count"]

testEKG :: IO ()
testEKG = do
    server <- forkServer "localhost" 8000
    tracer <- ekgTracer (Right server)
    formattedTracer <- metricsFormatter "cardano" tracer
    configureTracers emptyTraceConfig countDocumented [formattedTracer]
    loop (appendName "ekg1" formattedTracer) 1
  where
    loop :: Trace IO Int -> Int -> IO ()
    loop tr count = do
      if count == 1000
        then pure ()
        else do
          traceWith (appendName "count" tr) count
          threadDelay 100000
          loop tr (count + 1)
