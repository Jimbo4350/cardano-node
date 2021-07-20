{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Examples.Aggregation (
  testAggregation
) where

import qualified Data.Aeson as A
import           GHC.Generics (Generic)

import           Cardano.Logging

data BaseStats = BaseStats {
    bsMeasure :: Double,
    bsMin     :: Double,
    bsMax     :: Double,
    bsCount   :: Int,
    bsSum     :: Double
    } deriving (Eq, Ord, Show, Generic)

instance A.ToJSON BaseStats where
    toEncoding = A.genericToEncoding A.defaultOptions

instance LogFormatting BaseStats where
  forMachine = mempty
  asMetrics BaseStats {..} =
    [ DoubleM ["measure"] bsMeasure
    , DoubleM ["sum"] bsSum]

baseStatsDocumented :: Documented Double
baseStatsDocumented =
  Documented [
    DocMsg 0.0 [] "This is the value of the measurement"
  , DocMsg 0.0 [] "This is the sum of all measurments so far"
  ]

emptyStats :: BaseStats
emptyStats = BaseStats 0.0 100000000.0 (-100000000.0) 0 0.0

calculate :: BaseStats -> LoggingContext -> Double -> BaseStats
calculate BaseStats{..} _ val =
   BaseStats  val
              (min bsMin val)
              (max bsMax val)
              (1 + bsCount)
              (bsSum + val)

testAggregation :: IO ()
testAggregation = do
    simpleTracer  <- standardTracer Nothing
    formTracer <- humanFormatter True "cardano" simpleTracer
    tracer <- foldTraceM calculate emptyStats formTracer
    configureTracers emptyTraceConfig baseStatsDocumented [tracer]
    traceWith tracer 1.0
    traceWith tracer 2.0
    traceWith tracer 0.5
