{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.PlutusExample
where
import Prelude
import Control.Concurrent.MVar

import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Trans.Except

import Cardano.CLI.Shelley.Script (readFileScriptInAnyLang)

import Cardano.Api
import Cardano.Api.Shelley (ProtocolParameters)

import Cardano.Benchmarking.Types (NumberOfTxs(..))
import Cardano.Benchmarking.FundSet
import Cardano.Benchmarking.GeneratorTx.Tx as Tx (mkFee, mkTxOutValueAdaOnly, keyAddress )
import Cardano.Benchmarking.Wallet

payToScript ::
      SigningKey PaymentKey
  -> (Script PlutusScriptV1, Hash ScriptData)
  -> NetworkId
  -> TxGenerator AlonzoEra
payToScript key (script, txOutDatumHash) networkId inFunds outValues validity
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                         , newFunds $ getTxId b                       )
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

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] outValues

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra AlonzoEra $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = PlutusScriptFund
    }

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

spendFromScript ::
      SigningKey PaymentKey
  -> (PlutusScript PlutusScriptV1)
  -> NetworkId
  -> ProtocolParameters
  -> [Fund]
  -> [Fund]
  -> Validity
  -> Either String (Tx AlonzoEra, [Fund])  
spendFromScript key script networkId protocolParameters collateral inFunds validity
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                         , newFunds $ getTxId b                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ ScriptWitness ScriptWitnessForSpending plutusScriptWitness )) inFunds
    , txInsCollateral = TxInsCollateral CollateralInAlonzoEra $  map getFundTxIn collateral
    , txOuts = [mkTxOut outValue]
    , txFee = mkFee $ (fromIntegral requiredSteps) + (fromIntegral requiredMemory)
    , txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
    , txMetadata = TxMetadataNone
    , txAuxScripts = TxAuxScriptsNone
    , txExtraScriptData = BuildTxWith $ TxExtraScriptData ScriptDataInAlonzoEra [ScriptDataNumber 42]
    , txExtraKeyWits = TxExtraKeyWitnessesNone
    , txProtocolParams = BuildTxWith $ Just protocolParameters
    , txWithdrawals = TxWithdrawalsNone
    , txCertificates = TxCertificatesNone
    , txUpdateProposal = TxUpdateProposalNone
    , txMintValue = TxMintNone
    }

  requiredMemory = 700000000
  requiredSteps  = 700000000

  plutusScriptWitness = PlutusScriptWitness
                          PlutusScriptV1InAlonzo
                          PlutusScriptV1
                          script
                          (ScriptDatumForTxIn $ ScriptDataNumber 42) -- script date
                          (ScriptDataNumber 42) -- script redeemer
                          (ExecutionUnits requiredSteps requiredMemory)
                          

  outValue = (sum $ map getFundLovelace inFunds) - (fromIntegral requiredSteps) - (fromIntegral requiredMemory)

  mkTxOut v = TxOut (Tx.keyAddress  networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] [outValue]

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra AlonzoEra $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    , _fundVariant = PlainOldFund
    }


plutusWalletScript :: 
      SigningKey PaymentKey
  -> (PlutusScript PlutusScriptV1)
  -> NetworkId
  -> ProtocolParameters  
  -> [Fund]
  -> WalletRef
  -> NumberOfTxs
  -> Target
  -> WalletScript AlonzoEra
plutusWalletScript key script networkId protocolParameters collateral wRef (NumberOfTxs maxCount) targetNode
  = WalletScript (modifyMVarMasked wRef nextTx)
 where
  nextCall = plutusWalletScript key script networkId protocolParameters collateral wRef (NumberOfTxs maxCount) targetNode

  nextTx :: Wallet -> IO (Wallet, WalletStep AlonzoEra)
  nextTx w = if walletSeqNumber w > SeqNumber (fromIntegral maxCount)
    then return (w, Done)
    else case makeTransaction w of
      Right (wNew, tx) -> return (wNew, NextTx nextCall tx)
      Left err -> return (w, Error err)

  makeTransaction :: Wallet -> Either String (Wallet, Tx AlonzoEra)
  makeTransaction w = do
    let newSeqNumber = succ $ walletSeqNumber w
    inputFunds <- selectPlutusFund (walletFunds w)
    (tx, newFunds) <- spendFromScript key script networkId protocolParameters collateral inputFunds $ InFlight targetNode newSeqNumber
    let
      newWallet = (walletUpdateFunds newFunds inputFunds w) {walletSeqNumber = newSeqNumber}
    Right (newWallet , tx)
