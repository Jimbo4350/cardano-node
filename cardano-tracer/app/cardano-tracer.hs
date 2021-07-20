import           Data.Version (showVersion)
import           Options.Applicative

import           Cardano.Tracer.CLI (TracerParams, parseTracerParams)
import           Cardano.Tracer.Run (runCardanoTracer)
import           Paths_cardano_tracer (version)

main :: IO ()
main = do
  tracerParams <- customExecParser (prefs showHelpOnEmpty) tracerInfo
  runCardanoTracer tracerParams
 where
  tracerInfo :: ParserInfo TracerParams
  tracerInfo = info
    (parseTracerParams <**> helper <**> versionOption)
    (fullDesc <> header "cardano-tracer - the logging/monitoring service for cardano node.")
  versionOption = infoOption
    (showVersion version)
    (long "version" <>
     short 'v' <>
     help "Show version")
