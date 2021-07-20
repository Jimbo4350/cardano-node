module Main (
    main
) where

import           Examples.Aggregation
import           Examples.Configuration
import           Examples.Documentation
import           Examples.EKG
import           Examples.FrequencyLimiting
import           Examples.Routing
import           Examples.Trivial

main :: IO ()
main = do
    test1
    test2
    testAggregation
    testRouting
    testConfig
    testLimiting
    docTracer
    testEKG
