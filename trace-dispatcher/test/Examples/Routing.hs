{-# LANGUAGE FlexibleContexts #-}

module Examples.Routing (
  testRouting
) where

import           Cardano.Logging
import           Examples.TestObjects

routingTracer1 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> m (Trace m (TraceForgeEvent LogBlock))
routingTracer1 t1 t2 = routingTrace routingf (t1 <> t2)
  where
    routingf TraceStartLeadershipCheck {} = pure t1
    routingf _                            = pure t2

routingTracer2 :: (Monad m)
  => Trace m (TraceForgeEvent LogBlock)
  -> Trace m (TraceForgeEvent LogBlock)
  -> m (Trace m (TraceForgeEvent LogBlock))
routingTracer2 t1 t2 = pure (t1 <> t2)

testRouting :: IO ()
testRouting = do
    t <- standardTracer Nothing
    tf <- machineFormatter "cardano" t
    let t1 = appendName "tracer1" tf
    let t2 = appendName "tracer1" tf
    configureTracers emptyTraceConfig traceForgeEventDocu [t1, t2]
    r1 <- routingTracer1 t1 t2
    r2 <- routingTracer2 t1 t2
    traceWith r1 message2
    traceWith r2 message2
    traceWith (t1 <> t2) message3
