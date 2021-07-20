import           Test.Tasty

import qualified Cardano.Tracer.Test.Logs.File as Test.File
import qualified Cardano.Tracer.Test.Logs.Rotator as Test.Rotator

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "cardano-tracer"
    [ Test.File.tests
    , Test.Rotator.tests
    ]
