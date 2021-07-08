
import           Prelude

import           Cardano.Api

import           System.Directory
import           System.FilePath.Posix ((</>))

import           Cardano.PlutusExample.CustomDatumRedeemerGuess
import           Cardano.PlutusExample.AlwaysSucceeds (alwaysSucceedsScript)
import           Cardano.PlutusExample.DatumRedeemerGuess (guessScript)
import           Cardano.PlutusExample.MintingScript (apiExamplePlutusMintingScript)
import           Cardano.PlutusExample.Sum (sumScript)

main :: IO ()
main = do
  let dir = "generated-plutus-scripts"
  createDirectory dir

  _ <- writeFileTextEnvelope (dir </> "always-succeeds-spending.plutus") Nothing alwaysSucceedsScript
  _ <- writeFileTextEnvelope (dir </> "guess-42-datum-42-txin.plutus") Nothing guessScript
  _ <- writeFileTextEnvelope (dir </> "custom-guess-42-datum-42.plutus") Nothing customGuessScript
  _ <- writeFileTextEnvelope (dir </> "anyone-can-mint.plutus") Nothing apiExamplePlutusMintingScript
  _ <- writeFileTextEnvelope (dir </> "sum.plutus") Nothing sumScript
  return ()
