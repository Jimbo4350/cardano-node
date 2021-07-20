{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Examples.FrequencyLimiting (
  testLimiting
) where

import           Control.Concurrent
import           Data.Aeson

import           Cardano.Logging
import           Examples.TestObjects

data LOX = LOS (TraceForgeEvent LogBlock) | LOL LimitingMessage

instance LogFormatting LOX where
  forMachine _ (LOS _) =
      mkObject
        [ "kind" .= String "TraceForgeEvent"
        ]
  forMachine _ (LOL (StartLimiting text)) =
      mkObject
        [ "kind" .= String "StartLimiting"
        , "msg"  .= String text
        ]
  forMachine _ (LOL (StopLimiting msg num)) =
      mkObject
        [ "kind" .= String "StopLimiting"
        , "msg"  .= String msg
        , "numSuppressed" .= num
        ]

repeated :: Trace IO (TraceForgeEvent LogBlock) -> Int -> Int -> IO ()
repeated _ 0 _ = pure ()
repeated t n d = do
  traceWith t (TraceStartLeadershipCheck (SlotNo (fromIntegral n)))
  threadDelay d
  repeated t (n-1) d

testLimiting :: IO ()
testLimiting = do
  t0 <- standardTracer Nothing
  tf <- humanFormatter True "cardano" t0
  t1 <- (\tt -> limitFrequency 5 "5 messages per second" (contramap LOS tt) (contramap LOL tt))
              (appendName "tracer1" tf)
  t2 <- (\tt -> limitFrequency 15 "15 messages per second" (contramap LOS tt) (contramap LOL tt))
              (appendName "tracer2" tf)
  let t = t1 <> t2
  repeated t 1000 10000 -- 100 messages per second
  repeated t 20 1000000 -- 1  message per second
  repeated t 300 100000 -- 10  message per second
