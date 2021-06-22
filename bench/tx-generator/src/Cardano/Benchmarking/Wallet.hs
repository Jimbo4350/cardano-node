{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Benchmarking.Wallet
where
import           Prelude

import           Data.IxSet.Typed as IxSet
import           Data.Proxy
import           Control.Concurrent.MVar

import           Cardano.Api

import           Cardano.Benchmarking.Types (NumberOfOutputsPerTx(..), NumberOfTxs(..))
import           Cardano.Benchmarking.GeneratorTx.Tx as Tx hiding (Fund)
import           Cardano.Benchmarking.FundSet as FundSet

type WalletRef = MVar Wallet

data Wallet = Wallet {
    walletNetworkId :: !NetworkId
  , walletKey :: !(SigningKey PaymentKey)
  , walletSeqNumber :: !SeqNumber
  , walletFunds :: !FundSet
  }

initWallet :: NetworkId -> SigningKey PaymentKey -> IO (MVar Wallet)
initWallet network key = newMVar $ Wallet {
    walletNetworkId = network
  , walletKey = key
  , walletSeqNumber = SeqNumber 1
  , walletFunds = emptyFunds
  }

modifyWalletRef :: WalletRef -> (Wallet -> IO (Wallet, a)) -> IO a
modifyWalletRef = modifyMVar

walletRefInsertFund :: WalletRef -> Fund -> IO ()
walletRefInsertFund ref fund = modifyMVar_  ref $ \w -> return $ walletInsertFund fund w

walletInsertFund :: Fund -> Wallet -> Wallet
walletInsertFund f w
  = w { walletFunds = FundSet.insertFund (walletFunds w) f }

walletDeleteFund :: Fund -> Wallet -> Wallet
walletDeleteFund f w
  = w { walletFunds = FundSet.deleteFund (walletFunds w) f }

walletUpdateFunds :: [Fund] -> [Fund] -> Wallet -> Wallet
walletUpdateFunds add del w
  = foldl (flip walletInsertFund) w2 add
 where w2 = foldl (flip walletDeleteFund) w del

walletSelectFunds :: Wallet -> FundSelector -> Either String [Fund]
walletSelectFunds w s = s $ walletFunds w

walletExtractFunds :: Wallet -> FundSelector -> Either String (Wallet, [Fund])
walletExtractFunds w s
  = case walletSelectFunds w s of
    Left err -> Left err
    Right funds -> Right (walletUpdateFunds [] funds w, funds)

walletRefCreateCoins :: forall era. IsShelleyBasedEra era
  => WalletRef
  -> [Lovelace]
  -> IO (Either String (Tx era))
walletRefCreateCoins ref coins
  = modifyMVar ref $ \w -> case walletCreateCoins w coins of
     Right (newWallet, tx) -> return (newWallet, Right tx)
     Left err -> return (w, Left err)

walletCreateCoins :: forall era. IsShelleyBasedEra era
  => Wallet
  -> [Lovelace]
  -> Either String (Wallet, Tx era)
walletCreateCoins wallet genValues = do
  inputFunds <- selectMinValue (sum genValues) (walletFunds wallet)
  let outValues = includeChange (map getFundLovelace inputFunds) genValues
  (tx, newFunds) <- genTx (walletKey wallet) (walletNetworkId wallet) inputFunds Confirmed outValues
  Right (walletUpdateFunds newFunds inputFunds wallet, tx)

includeChange :: [Lovelace] -> [Lovelace] -> [Lovelace]
includeChange have spend = case compare changeValue 0 of
  GT -> changeValue : spend
  EQ -> spend
  LT -> error "genTX: Bad transaction: insufficient funds"
  where changeValue = sum have - sum spend

-- genTx assumes that inFunds and outValues are of equal value.
genTx :: forall era. IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NetworkId
  -> [Fund]
  -> Validity
  -> [Lovelace]
  -> Either String (Tx era, [Fund])
genTx key networkId inFunds validity outValues
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , newFunds $ getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = map mkTxOut outValues
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
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

  mkTxOut v = TxOut (Tx.keyAddress @ era networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  upperBound :: TxValidityUpperBound era
  upperBound = case shelleyBasedEra @ era of
    ShelleyBasedEraShelley -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    ShelleyBasedEraAllegra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    ShelleyBasedEraMary    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    ShelleyBasedEraAlonzo  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] outValues

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra (cardanoEra @ era) $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    }

type TxGenerator era = Validity -> [Fund] -> Either String (Tx era, [Fund])

benchmarkTransaction ::
     Wallet
  -> FundSelector
  -> TxGenerator era
  -> Target
  -> Either String (Wallet, Tx era)
benchmarkTransaction wallet selector txGenerator targetNode = do
  inputFunds <- selector (walletFunds wallet)
  let outValues = map getFundLovelace inputFunds
  (tx, newFunds) <- txGenerator (InFlight targetNode newSeqNumber) inputFunds
  let
    newWallet = (walletUpdateFunds newFunds inputFunds wallet) {walletSeqNumber = newSeqNumber}
  Right (newWallet , tx)
 where
  newSeqNumber = succ $ walletSeqNumber wallet

newtype WalletScript era = WalletScript { runWalletScript :: IO (WalletStep era) }

data WalletStep era
  = Done
  | NextTx !(WalletScript era) !(Tx era)
  | Error String

benchmarkWalletScript :: forall era .
     IsShelleyBasedEra era
  => WalletRef
  -> NumberOfTxs
  -> NumberOfOutputsPerTx
  -- in this version : numberOfInputs == numberOfOutputs
  -> Target
  -> WalletScript era
benchmarkWalletScript wRef (NumberOfTxs maxCount) (NumberOfOutputsPerTx numInputs) targetNode
  = WalletScript (modifyMVarMasked wRef nextTx)
 where
  nextCall = benchmarkWalletScript wRef (NumberOfTxs maxCount) (NumberOfOutputsPerTx numInputs) targetNode

  nextTx :: Wallet -> IO (Wallet, WalletStep era)
  nextTx w = if walletSeqNumber w > SeqNumber (fromIntegral maxCount)
    then return (w, Done)
    else case benchmarkTransaction w selector (txGenerator w) targetNode of
      Right (wNew, tx) -> return (wNew, NextTx nextCall tx)
      Left err -> return (w, Error err)

  selector = selectCountTarget numInputs targetNode

  txGenerator w validity funds = genTx (walletKey w) (walletNetworkId w) funds validity (map getFundLovelace funds)

{-
-- genTx assumes that inFunds and outValues are of equal value.
genTx2 :: forall era.
     IsShelleyBasedEra era
  => SigningKey PaymentKey
  -> NetworkId
  -> CardanoEra era
  -> [Fund]
  -> Validity
  -> [Lovelace]
  -> Either String (Tx era, [Fund])
genTx2 key networkId era inFunds validity outValues
  = case makeTransactionBody txBodyContent of
      Left err -> error $ show err
      Right b -> Right ( signShelleyTransaction b (map (WitnessPaymentKey . getFundKey) inFunds)
                       , newFunds $ getTxId b
                       )
 where
  txBodyContent = TxBodyContent {
      txIns = map (\f -> (getFundTxIn f, BuildTxWith $ KeyWitness KeyWitnessForSpending)) inFunds
    , txInsCollateral = TxInsCollateralNone
    , txOuts = map mkTxOut outValues
    , txFee = mkFee 0
    , txValidityRange = (TxValidityNoLowerBound, upperBound)
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

  mkTxOut v = TxOut (Tx.keyAddress @ era networkId key) (mkTxOutValueAdaOnly v) TxOutDatumHashNone

  upperBound :: TxValidityUpperBound era
  upperBound = case era of
    ByronEra -> error "byronEra"
    ShelleyEra -> TxValidityUpperBound ValidityUpperBoundInShelleyEra $ SlotNo maxBound
    AllegraEra -> TxValidityNoUpperBound ValidityNoUpperBoundInAllegraEra
    MaryEra    -> TxValidityNoUpperBound ValidityNoUpperBoundInMaryEra
    AlonzoEra  -> TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra

  newFunds txId = zipWith (mkNewFund txId) [TxIx 0 ..] outValues

  mkNewFund :: TxId -> TxIx -> Lovelace -> Fund
  mkNewFund txId txIx val = Fund $ InAnyCardanoEra era $ FundInEra {
      _fundTxIn = TxIn txId txIx
    , _fundVal = mkTxOutValueAdaOnly val
    , _fundSigningKey = key
    , _fundValidity = validity
    }
-}
