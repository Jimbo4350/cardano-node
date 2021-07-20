{-# LANGUAGE ScopedTypeVariables #-}
module Examples.Trivial (
    test1
  , test2
) where


import           Cardano.Logging
import           Examples.TestObjects


-- | Make shure the function append name is only called once
--   for every path element
test1 :: IO ()
test1 = do
    stdoutTracer' <- standardTracer Nothing
    simpleTracer <- machineFormatter "cardano" stdoutTracer'
    configureTracers emptyTraceConfig traceForgeEventDocu [simpleTracer]
    let simpleTracer1 = filterTraceBySeverity (Just WarningF) simpleTracer
    let simpleTracerC1 = appendName "Outer1" simpleTracer1
    let simpleTracerC2 = appendName "Inner1" simpleTracerC1
    let simpleTracerC3 = setSeverity Error
                        $ setPrivacy Confidential
                            $ appendName "Inner2" simpleTracerC1
    traceWith (setSeverity Error simpleTracerC2) message1
    traceWith (setSeverity Warning simpleTracerC3) message2
    traceWith simpleTracerC2 message3
    traceWith (setSeverity Critical (appendName "Inner3" simpleTracerC3)) message4

test2 :: IO ()
test2 = do
    stdoutTracer' <- standardTracer Nothing
    simpleTracer <- humanFormatter True "cardano" stdoutTracer'
    configureTracers emptyTraceConfig traceForgeEventDocu [simpleTracer]
    let simpleTracer1  = withSeverity loSeverity
                            (filterTraceBySeverity (Just WarningF) simpleTracer)
    let simpleTracerC1 = appendName "Outer1" simpleTracer1
    let simpleTracerC2 = appendName "Inner1" simpleTracerC1
    let simpleTracerC3 = setPrivacy Confidential $ appendName "Inner2" simpleTracerC1
    traceWith simpleTracerC2 message1
    traceWith (setSeverity Critical simpleTracerC3) message2
    traceWith simpleTracerC2 message3
    traceWith (appendName "Inner3" simpleTracerC3) message4
    traceWith (appendName "cont1" $ appendName "cont2" $ appendName "cont3" simpleTracerC2) message1

loSeverity :: TraceForgeEvent LogBlock -> SeverityS
loSeverity TraceStartLeadershipCheck {} = Warning
loSeverity TraceSlotIsImmutable {}      = Error
loSeverity TraceBlockFromFuture {}      = Error
