{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.PlutusExample
where
import Prelude

import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Trans.Except

import Cardano.CLI.Shelley.Script (readFileScriptInAnyLang)

import Cardano.Api
import Cardano.Benchmarking.FundSet
import Cardano.Benchmarking.GeneratorTx.Tx (mkFee, mkTxOutValueAdaOnly )

-- genTx assumes that inFunds and outValues are of equal value.
payToScript ::
     (Script PlutusScriptV1, Hash ScriptData)
  -> NetworkId
  -> [Fund]
  -> [Lovelace]
  -> Either String (Tx AlonzoEra, TxId)
payToScript (script, txOutDatumHash) networkId inFunds outValues
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = map mkTxOut outValues
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith Nothing
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    }

  mkTxOut v = TxOut plutusScriptAddr (mkTxOutValueAdaOnly v) (TxOutDatumHash ScriptDataInAlonzoEra txOutDatumHash)

  plutusScriptAddr = makeShelleyAddressInEra
                       networkId
                       (PaymentCredentialByScript $ hashScript script)
                       NoStakeAddress

readScript :: FilePath -> IO (Script PlutusScriptV1)
readScript fp = do
  res <- runExceptT $ readFileScriptInAnyLang fp
  case res of
    Left err -> do
      putStrLn $ show err
      error $ show err
    Right (ScriptInAnyLang (PlutusScriptLanguage PlutusScriptV1) script) -> return script
    Right _otherScript ->
      error "Wrong script version."

toScriptHash :: String -> (Hash ScriptData)
toScriptHash str
  = case deserialiseFromRawBytesHex (AsHash AsScriptData) (BSC.pack str) of
    Just x -> x
    Nothing  -> error $ "Invalid datum hash: " ++ show str
