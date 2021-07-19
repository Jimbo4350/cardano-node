{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Cardano.CLI.Shelley.Run.Transaction
  ( ShelleyTxCmdError
  , renderShelleyTxCmdError
  , runTransactionCmd
  ) where

import           Cardano.Prelude hiding (All, Any)
import           Prelude (String, error)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Data.Type.Equality (TestEquality (..))

import           Control.Concurrent.STM
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither,
                   hoistMaybe, left, newExceptT)

import           Cardano.Api
import           Cardano.Api.Byron hiding (SomeByronSigningKey (..))
import           Cardano.Api.Shelley
import           Ouroboros.Consensus.Shelley.Eras (StandardAllegra, StandardMary, StandardShelley)

--TODO: do this nicely via the API too:
import qualified Cardano.Binary as CBOR

--TODO: following import needed for orphan Eq Script instance
import           Cardano.Ledger.ShelleyMA.TxBody ()
import           Shelley.Spec.Ledger.Scripts ()

import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Run.Friendly (friendlyTxBodyBS)
import           Cardano.CLI.Shelley.Key (InputDecodeError, readSigningKeyFileAnyOf)
import           Cardano.CLI.Shelley.Parsers
import           Cardano.CLI.Shelley.Run.Genesis (ShelleyGenesisCmdError (..), readShelleyGenesis,
                   renderShelleyGenesisCmdError)
import           Cardano.CLI.Shelley.Run.Query (ShelleyQueryCmdLocalStateQueryError (..),
                   renderLocalStateQueryError)
import           Cardano.CLI.Shelley.Script
import           Cardano.CLI.Types
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (SystemStart (..))
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Cardano.Block (EraMismatch (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr)
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Net.Query
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Net.Query
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as Net.Tx

import qualified System.IO as IO

{- HLINT ignore "Use let" -}

data ShelleyTxCmdError
  = ShelleyTxCmdAesonDecodeProtocolParamsError !FilePath !Text
  | ShelleyTxCmdReadFileError !(FileError ())
  | ShelleyTxCmdScriptFileError (FileError ScriptDecodeError)
  | ShelleyTxCmdReadTextViewFileError !(FileError TextEnvelopeError)
  | ShelleyTxCmdReadWitnessSigningDataError !ReadWitnessSigningDataError
  | ShelleyTxCmdWriteFileError !(FileError ())
  | ShelleyTxCmdEraConsensusModeMismatch
      !(Maybe FilePath)
      !AnyConsensusMode
      !AnyCardanoEra
      -- ^ Era
  | ShelleyTxCmdMetadataJsonParseError !FilePath !String
  | ShelleyTxCmdMetadataConversionError !FilePath !TxMetadataJsonError
  | ShelleyTxCmdMetaValidationError !FilePath ![(Word64, TxMetadataRangeError)]
  | ShelleyTxCmdScriptDataJsonParseError  !FilePath !String
  | ShelleyTxCmdScriptDataConversionError !FilePath !ScriptDataJsonError
  | ShelleyTxCmdScriptDataValidationError !FilePath !ScriptDataRangeError
  | ShelleyTxCmdMetaDecodeError !FilePath !CBOR.DecoderError
  | ShelleyTxCmdBootstrapWitnessError !ShelleyBootstrapWitnessError
  | ShelleyTxCmdSocketEnvError !EnvSocketError
  | ShelleyTxCmdTxSubmitError !Text
  | ShelleyTxCmdTxSubmitErrorByron !(ApplyTxErr ByronBlock)
  | ShelleyTxCmdTxSubmitErrorShelley !(ApplyTxErr (ShelleyBlock StandardShelley))
  | ShelleyTxCmdTxSubmitErrorAllegra !(ApplyTxErr (ShelleyBlock StandardAllegra))
  | ShelleyTxCmdTxSubmitErrorMary !(ApplyTxErr (ShelleyBlock StandardMary))
  | ShelleyTxCmdTxSubmitErrorEraMismatch !EraMismatch
  | ShelleyTxCmdTxFeatureMismatch !AnyCardanoEra !TxFeature
  | ShelleyTxCmdTxBodyError !TxBodyError
  | ShelleyTxCmdNotImplemented !Text
  | ShelleyTxCmdWitnessEraMismatch !AnyCardanoEra !AnyCardanoEra !WitnessFile
  | ShelleyTxCmdScriptLanguageNotSupportedInEra !AnyScriptLanguage !AnyCardanoEra
  | ShelleyTxCmdScriptExpectedSimple !FilePath !AnyScriptLanguage
  | ShelleyTxCmdScriptExpectedPlutus !FilePath !AnyScriptLanguage
  | ShelleyTxCmdGenesisCmdError !ShelleyGenesisCmdError
  | ShelleyTxCmdPolicyIdsMissing ![PolicyId]
  | ShelleyTxCmdPolicyIdsExcess  ![PolicyId]
  | ShelleyTxCmdAcquireFailure !AcquireFailure
  | ShelleyTxCmdUnsupportedMode !AnyConsensusMode
  | ShelleyTxCmdByronEra
  | ShelleyTxCmdEraConsensusModeMismatchTxBalance
      !TxBodyFile
      !AnyConsensusMode
      !AnyCardanoEra
  | ShelleyTxCmdBalanceTxBody !TxBodyErrorAutoBalance
  | ShelleyTxCmdEraConsensusModeMismatchQuery !AnyConsensusMode !AnyCardanoEra
  | ShelleyTxCmdByronEraQuery
  | ShelleyTxCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError

  deriving Show


renderShelleyTxCmdError :: ShelleyTxCmdError -> Text
renderShelleyTxCmdError err =
  case err of
    ShelleyTxCmdReadFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadTextViewFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdScriptFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdReadWitnessSigningDataError witSignDataErr ->
      renderReadWitnessSigningDataError witSignDataErr
    ShelleyTxCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyTxCmdMetadataJsonParseError fp jsonErr ->
       "Invalid JSON format in file: " <> show fp
                <> "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdMetadataConversionError fp metadataErr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError metadataErr)
    ShelleyTxCmdMetaDecodeError fp metadataErr ->
       "Error decoding CBOR metadata at: " <> show fp
                             <> " Error: " <> show metadataErr
    ShelleyTxCmdMetaValidationError fp errs ->
      "Error validating transaction metadata at: " <> show fp <> "\n" <>
      Text.intercalate "\n"
        [ "key " <> show k <> ":" <> Text.pack (displayError valErr)
        | (k, valErr) <- errs ]

    ShelleyTxCmdScriptDataJsonParseError  fp jsonErr ->
       "Invalid JSON format in file: " <> show fp <>
       "\nJSON parse error: " <> Text.pack jsonErr
    ShelleyTxCmdScriptDataConversionError fp cerr ->
       "Error reading metadata at: " <> show fp
                             <> "\n" <> Text.pack (displayError cerr)
    ShelleyTxCmdScriptDataValidationError fp verr ->
      "Error validating script data at: " <> show fp <> ":\n" <>
      Text.pack (displayError verr)

    ShelleyTxCmdSocketEnvError envSockErr -> renderEnvSocketError envSockErr
    ShelleyTxCmdAesonDecodeProtocolParamsError fp decErr ->
      "Error while decoding the protocol parameters at: " <> show fp
                                            <> " Error: " <> show decErr
    ShelleyTxCmdTxSubmitError res -> "Error while submitting tx: " <> res
    ShelleyTxCmdTxSubmitErrorByron res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorShelley res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorAllegra res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorMary res ->
      "Error while submitting tx: " <> Text.pack (show res)
    ShelleyTxCmdTxSubmitErrorEraMismatch EraMismatch{ledgerEraName, otherEraName} ->
      "The era of the node and the tx do not match. " <>
      "The node is running in the " <> ledgerEraName <>
      " era, but the transaction is for the " <> otherEraName <> " era."
    ShelleyTxCmdBootstrapWitnessError sbwErr ->
      renderShelleyBootstrapWitnessError sbwErr

    ShelleyTxCmdTxFeatureMismatch era TxFeatureImplicitFees ->
      "An explicit transaction fee must be specified for " <>
      renderEra era <> " era transactions."

    ShelleyTxCmdTxFeatureMismatch (AnyCardanoEra ShelleyEra)
                                  TxFeatureValidityNoUpperBound ->
      "A TTL must be specified for Shelley era transactions."

    ShelleyTxCmdTxFeatureMismatch era feature ->
      renderFeature feature <> " cannot be used for " <> renderEra era <>
      " era transactions."

    ShelleyTxCmdTxBodyError err' -> Text.pack (displayError err')

    ShelleyTxCmdNotImplemented msg ->
      "Feature not yet implemented: " <> msg

    ShelleyTxCmdWitnessEraMismatch era era' (WitnessFile file) ->
      "The era of a witness does not match the era of the transaction. " <>
      "The transaction is for the " <> renderEra era <> " era, but the " <>
      "witness in " <> show file <> " is for the " <> renderEra era' <> " era."

    ShelleyTxCmdScriptLanguageNotSupportedInEra (AnyScriptLanguage lang) era ->
      "The script language " <> show lang <> " is not supported in the " <>
      renderEra era <> " era."

    ShelleyTxCmdScriptExpectedSimple file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the simple script language, " <>
      "but it is actually using " <> show lang <> ". Alternatively, to use " <>
      "a Plutus script, you must also specify the redeemer " <>
      "(datum if appropriate) and script execution units."

    ShelleyTxCmdScriptExpectedPlutus file (AnyScriptLanguage lang) ->
      Text.pack file <> ": expected a script in the Plutus script language, " <>
      "but it is actually using " <> show lang <> "."

    ShelleyTxCmdEraConsensusModeMismatch fp mode era ->
       "Submitting " <> renderEra era <> " era transaction (" <> show fp <>
       ") is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdGenesisCmdError e -> renderShelleyGenesisCmdError e
    ShelleyTxCmdPolicyIdsMissing policyids ->
      "The \"--mint\" flag specifies an asset with a policy Id, but no \
      \corresponding monetary policy script has been provided as a witness \
      \(via the \"--minting-script-file\" flag). The policy Id in question is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)

    ShelleyTxCmdPolicyIdsExcess policyids ->
      "A script provided to witness minting does not correspond to the policy \
      \id of any asset specified in the \"--mint\" field. The script hash is: "
      <> Text.intercalate ", " (map serialiseToRawBytesHexText policyids)
    ShelleyTxCmdAcquireFailure acquireFail -> Text.pack $ show acquireFail
    ShelleyTxCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyTxCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyTxCmdEraConsensusModeMismatchTxBalance fp mode era ->
       "Cannot balance " <> renderEra era <> " era transaction body (" <> show fp <>
       ") because is not supported in the " <> renderMode mode <> " consensus mode."
    ShelleyTxCmdEraConsensusModeMismatchQuery (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyTxCmdByronEraQuery -> "Query not available in Byron era"
    ShelleyTxCmdLocalStateQueryError err' -> renderLocalStateQueryError err'
    ShelleyTxCmdBalanceTxBody err' -> Text.pack $ displayError err'

renderEra :: AnyCardanoEra -> Text
renderEra (AnyCardanoEra ByronEra)   = "Byron"
renderEra (AnyCardanoEra ShelleyEra) = "Shelley"
renderEra (AnyCardanoEra AllegraEra) = "Allegra"
renderEra (AnyCardanoEra MaryEra)    = "Mary"
renderEra (AnyCardanoEra AlonzoEra)  = "Alonzo"

renderFeature :: TxFeature -> Text
renderFeature TxFeatureShelleyAddresses     = "Shelley addresses"
renderFeature TxFeatureExplicitFees         = "Explicit fees"
renderFeature TxFeatureImplicitFees         = "Implicit fees"
renderFeature TxFeatureValidityLowerBound   = "A validity lower bound"
renderFeature TxFeatureValidityUpperBound   = "A validity upper bound"
renderFeature TxFeatureValidityNoUpperBound = "An absent validity upper bound"
renderFeature TxFeatureTxMetadata           = "Transaction metadata"
renderFeature TxFeatureAuxScripts           = "Auxiliary scripts"
renderFeature TxFeatureWithdrawals          = "Reward account withdrawals"
renderFeature TxFeatureCertificates         = "Certificates"
renderFeature TxFeatureMintValue            = "Asset minting"
renderFeature TxFeatureMultiAssetOutputs    = "Multi-Asset outputs"
renderFeature TxFeatureScriptWitnesses      = "Script witnesses"
renderFeature TxFeatureShelleyKeys          = "Shelley keys"
renderFeature TxFeatureCollateral           = "Collateral inputs"
renderFeature TxFeatureProtocolParameters   = "Protocol parameters"
renderFeature TxFeatureTxOutDatum           = "Transaction output datums"

runTransactionCmd :: TransactionCmd -> ExceptT ShelleyTxCmdError IO ()
runTransactionCmd cmd =
  case cmd of
    TxBuild era consensusModeParams nid txins txinsc txouts changeAddr mValue mLowBound
            mUpperBound certs wdrls metadataSchema scriptFiles
            metadataFiles mpparams mUpProp out ->
      runTxBuild era consensusModeParams nid txins txinsc txouts changeAddr mValue mLowBound
                 mUpperBound certs wdrls metadataSchema scriptFiles
                 metadataFiles mpparams mUpProp out
    TxBuildRaw era txins txinsc txouts mValue mLowBound mUpperBound
               fee certs wdrls metadataSchema scriptFiles
               metadataFiles mpparams mUpProp out ->
      runTxBuildRaw era txins txinsc txouts mLowBound mUpperBound
                    fee mValue certs wdrls metadataSchema
                    scriptFiles metadataFiles mpparams mUpProp out
    TxSign txinfile skfiles network txoutfile ->
      runTxSign txinfile skfiles network txoutfile
    TxSubmit anyConensusModeParams network txFp ->
      runTxSubmit anyConensusModeParams network txFp
    TxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                      nShelleyKeyWitnesses nByronKeyWitnesses ->
      runTxCalculateMinFee txbody mnw pGenesisOrParamsFile nInputs nOutputs
                           nShelleyKeyWitnesses nByronKeyWitnesses
    TxCalculateMinValue pParamSpec txOuts -> runTxCalculateMinValue pParamSpec txOuts
    TxHashScriptData scriptDataOrFile -> runTxHashScriptData scriptDataOrFile
    TxGetTxId txinfile -> runTxGetTxId txinfile
    TxView txinfile -> runTxView txinfile
    TxMintedPolicyId sFile -> runTxCreatePolicyId sFile
    TxCreateWitness txBodyfile witSignData mbNw outFile ->
      runTxCreateWitness txBodyfile witSignData mbNw outFile
    TxAssembleTxBodyWitness txBodyFile witnessFile outFile ->
      runTxSignWitness txBodyFile witnessFile outFile

-- ----------------------------------------------------------------------------
-- Building transactions
--

runTxBuildRaw
  :: AnyCardanoEra
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> [TxOutAnyEra]
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> Maybe Lovelace
  -- ^ Tx fee
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -- ^ Multi-Asset value(s)
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuildRaw (AnyCardanoEra era)
              inputsAndScripts inputsCollateral txouts
              mLowerBound mUpperBound
              mFee mValue
              certFiles withdrawals
              metadataSchema scriptFiles
              metadataFiles mpparams mUpdatePropFile
              (TxBodyFile fpath) = do
    txBodyContent <-
      TxBodyContent
        <$> validateTxIns  era inputsAndScripts
        <*> validateTxInsCollateral
                           era inputsCollateral
        <*> validateTxOuts era txouts
        <*> validateTxFee  era mFee
        <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                 <*> validateTxValidityUpperBound era mUpperBound)
        <*> validateTxMetadataInEra  era metadataSchema metadataFiles
        <*> validateTxAuxScripts     era scriptFiles
        <*> pure (BuildTxWith TxExtraScriptDataNone) --TODO alonzo: support this
        <*> pure TxExtraKeyWitnessesNone --TODO alonzo: support this
        <*> validateProtocolParameters era mpparams
        <*> validateTxWithdrawals    era withdrawals
        <*> validateTxCertificates   era certFiles
        <*> validateTxUpdateProposal era mUpdatePropFile
        <*> validateTxMintValue      era mValue

    txBody <-
      firstExceptT ShelleyTxCmdTxBodyError . hoistEither $
        makeTransactionBody txBodyContent
    liftIO $ print txBody
    firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
      writeFileTextEnvelope fpath Nothing txBody

runTxBuild
  :: AnyCardanoEra
  -> AnyConsensusModeParams
  -> NetworkId
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -- ^ TxIn with potential script witness
  -> [TxIn]
  -- ^ TxIn for collateral
  -> [TxOutAnyEra]
  -- ^ Normal outputs
  -> TxOutChangeAddress
  -- ^ A change output
  -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
  -- ^ Multi-Asset value(s)
  -> Maybe SlotNo
  -- ^ Tx lower bound
  -> Maybe SlotNo
  -- ^ Tx upper bound
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -- ^ Certificate with potential script witness
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> TxMetadataJsonSchema
  -> [ScriptFile]
  -> [MetadataFile]
  -> Maybe ProtocolParamsSourceSpec
  -> Maybe UpdateProposalFile
  -> TxBodyFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxBuild (AnyCardanoEra era) (AnyConsensusModeParams cModeParams) networkId txins txinsc txouts
           (TxOutChangeAddress changeAddr) mValue mLowerBound mUpperBound certFiles withdrawals
           metadataSchema scriptFiles metadataFiles mpparams mUpdatePropFile outBody@(TxBodyFile fpath) = do
  SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath

  let localNodeConnInfo = LocalNodeConnectInfo cModeParams networkId sockPath
      consensusMode = consensusModeOnly cModeParams
      dummyFee = Just $ Lovelace 0
      onlyInputs = [input | (input,_) <- txins]
  case consensusMode of
    CardanoMode -> do
      txBodyContent <-
        TxBodyContent
          <$> validateTxIns               era txins
          <*> validateTxInsCollateral     era txinsc
          <*> validateTxOuts              era txouts
          <*> validateTxFee               era dummyFee
          <*> ((,) <$> validateTxValidityLowerBound era mLowerBound
                   <*> validateTxValidityUpperBound era mUpperBound)
          <*> validateTxMetadataInEra     era metadataSchema metadataFiles
          <*> validateTxAuxScripts        era scriptFiles
          <*> pure (BuildTxWith TxExtraScriptDataNone) --TODO alonzo: support this
          <*> pure TxExtraKeyWitnessesNone --TODO alonzo: support this
          <*> validateProtocolParameters  era mpparams
          <*> validateTxWithdrawals       era withdrawals
          <*> validateTxCertificates      era certFiles
          <*> validateTxUpdateProposal    era mUpdatePropFile
          <*> validateTxMintValue         era mValue

      -- TODO: Combine queries
      let localConnInfo = LocalNodeConnectInfo
                            { localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
                            , localNodeNetworkId       = networkId
                            , localNodeSocketPath      = sockPath
                            }

      sbe <- getSbe $ cardanoEraStyle era
      eInMode <- case toEraInMode era CardanoMode of
                   Just result -> return result
                   Nothing ->
                     left (ShelleyTxCmdEraConsensusModeMismatchTxBalance outBody
                            (AnyConsensusMode CardanoMode) (AnyCardanoEra era))

      let utxoQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe
                        (QueryUTxO . QueryUTxOByTxIn $ Set.fromList onlyInputs)
      let pParamsQuery = QueryInEra eInMode $ QueryInShelleyBasedEra sbe QueryProtocolParameters
      utxo <- executeQuery era cModeParams localConnInfo utxoQuery
      pparams <- executeQuery era cModeParams localConnInfo pParamsQuery
      (eraHistory, systemStart) <- firstExceptT ShelleyTxCmdAcquireFailure
                                     $ newExceptT $ queryEraHistoryAndSystemStart localNodeConnInfo Nothing

      let cAddr = case anyAddressInEra (shelleyBasedToCardanoEra sbe) changeAddr of
                    Just addr -> addr
                    Nothing -> error $ "runTxBuild: Byron address used: " <> show changeAddr

      balancedTxBody <-
        firstExceptT ShelleyTxCmdBalanceTxBody
          . hoistEither
          $ makeTransactionBodyAutoBalance sbe eInMode systemStart eraHistory
                                           pparams Set.empty utxo txBodyContent
                                           cAddr Nothing

      firstExceptT ShelleyTxCmdWriteFileError . newExceptT
        $ writeFileTextEnvelope fpath Nothing balancedTxBody

    wrongMode -> left (ShelleyTxCmdUnsupportedMode (AnyConsensusMode wrongMode))


queryEraHistoryAndSystemStart
  :: LocalNodeConnectInfo CardanoMode
  -> Maybe ChainPoint
  -> IO (Either Net.Query.AcquireFailure (EraHistory CardanoMode, SystemStart))
queryEraHistoryAndSystemStart connctInfo mpoint = do
    resultVar <- newEmptyTMVarIO
    connectToLocalNode
      connctInfo
      LocalNodeClientProtocols
      { localChainSyncClient    = NoLocalChainSyncClient
      , localStateQueryClient   = Just (singleQuery mpoint resultVar)
      , localTxSubmissionClient = Nothing
      }
    atomically (takeTMVar resultVar)
  where
    singleQuery
      :: Maybe ChainPoint
      -> TMVar (Either Net.Query.AcquireFailure (EraHistory CardanoMode, SystemStart))
      -> Net.Query.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint
                                         (QueryInMode CardanoMode) IO ()
    singleQuery mPointVar' resultVar' =
      LocalStateQueryClient $ do
      pure . Net.Query.SendMsgAcquire mPointVar' $
        Net.Query.ClientStAcquiring
        { Net.Query.recvMsgAcquired =
          pure $ Net.Query.SendMsgQuery (QueryEraHistory CardanoModeIsMultiEra) $
            Net.Query.ClientStQuerying
            { Net.Query.recvMsgResult = \result1 -> do
              pure $ Net.Query.SendMsgQuery QuerySystemStart $
                Net.Query.ClientStQuerying
                { Net.Query.recvMsgResult = \result2 -> do
                  atomically $ putTMVar resultVar' (Right (result1, result2))

                  pure $ Net.Query.SendMsgRelease $ pure $ Net.Query.SendMsgDone ()
                }
            }
        , Net.Query.recvMsgFailure = \failure -> do
            atomically $ putTMVar resultVar' (Left failure)
            pure $ Net.Query.SendMsgDone ()
        }

-- ----------------------------------------------------------------------------
-- Transaction body validation and conversion
--

-- | An enumeration of era-dependent features where we have to check that it
-- is permissible to use this feature in this era.
--
data TxFeature = TxFeatureShelleyAddresses
               | TxFeatureExplicitFees
               | TxFeatureImplicitFees
               | TxFeatureValidityLowerBound
               | TxFeatureValidityUpperBound
               | TxFeatureValidityNoUpperBound
               | TxFeatureTxMetadata
               | TxFeatureAuxScripts
               | TxFeatureWithdrawals
               | TxFeatureCertificates
               | TxFeatureMintValue
               | TxFeatureMultiAssetOutputs
               | TxFeatureScriptWitnesses
               | TxFeatureShelleyKeys
               | TxFeatureCollateral
               | TxFeatureProtocolParameters
               | TxFeatureTxOutDatum
  deriving Show

txFeatureMismatch :: CardanoEra era
                  -> TxFeature
                  -> ExceptT ShelleyTxCmdError IO a
txFeatureMismatch era feature =
    left (ShelleyTxCmdTxFeatureMismatch (anyCardanoEra era) feature)

validateTxIns
  :: forall era.
     CardanoEra era
  -> [(TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))]
  -> ExceptT ShelleyTxCmdError IO
             [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))]
validateTxIns era = mapM convert
 where
   convert
     :: (TxIn, Maybe (ScriptWitnessFiles WitCtxTxIn))
     -> ExceptT ShelleyTxCmdError IO
                (TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn era))
   convert (txin, mScriptWitnessFiles) =
     case mScriptWitnessFiles of
       Just scriptWitnessFiles -> do
         sWit <- createScriptWitness era scriptWitnessFiles
         return ( txin
                , BuildTxWith $ ScriptWitness ScriptWitnessForSpending sWit
                )
       Nothing -> return (txin, BuildTxWith $ KeyWitness KeyWitnessForSpending)


validateTxInsCollateral :: CardanoEra era
                        -> [TxIn]
                        -> ExceptT ShelleyTxCmdError IO (TxInsCollateral era)
validateTxInsCollateral _   []    = return TxInsCollateralNone
validateTxInsCollateral era txins =
    case collateralSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCollateral
      Just supported -> return (TxInsCollateral supported txins)


validateTxOuts :: forall era.
                  CardanoEra era
               -> [TxOutAnyEra]
               -> ExceptT ShelleyTxCmdError IO [TxOut era]
validateTxOuts era = mapM toTxOutInAnyEra
  where
    toTxOutInAnyEra :: TxOutAnyEra
                    -> ExceptT ShelleyTxCmdError IO (TxOut era)
    toTxOutInAnyEra (TxOutAnyEra addr val mDatumHash) =
      case (scriptDataSupportedInEra era, mDatumHash) of
        (_, Nothing) ->
          TxOut <$> toAddressInAnyEra addr
                <*> toTxOutValueInAnyEra val
                <*> pure TxOutDatumHashNone
        (Just supported, Just dh) ->
          TxOut <$> toAddressInAnyEra addr
                <*> toTxOutValueInAnyEra val
                <*> pure (TxOutDatumHash supported dh)
        (Nothing, Just _) ->
          txFeatureMismatch era TxFeatureTxOutDatum

    toAddressInAnyEra :: AddressAny -> ExceptT ShelleyTxCmdError IO (AddressInEra era)
    toAddressInAnyEra addrAny =
      case addrAny of
        AddressByron   bAddr -> return (AddressInEra ByronAddressInAnyEra bAddr)
        AddressShelley sAddr ->
          case cardanoEraStyle era of
            LegacyByronEra -> txFeatureMismatch era TxFeatureShelleyAddresses

            ShelleyBasedEra era' ->
              return (AddressInEra (ShelleyAddressInEra era') sAddr)

    toTxOutValueInAnyEra :: Value -> ExceptT ShelleyTxCmdError IO (TxOutValue era)
    toTxOutValueInAnyEra val =
      case multiAssetSupportedInEra era of
        Left adaOnlyInEra ->
          case valueToLovelace val of
            Just l  -> return (TxOutAdaOnly adaOnlyInEra l)
            Nothing -> txFeatureMismatch era TxFeatureMultiAssetOutputs
        Right multiAssetInEra -> return (TxOutValue multiAssetInEra val)


validateTxFee :: CardanoEra era
              -> Maybe Lovelace
              -> ExceptT ShelleyTxCmdError IO (TxFee era)
validateTxFee era mfee =
    case (txFeesExplicitInEra era, mfee) of
      (Left  implicit, Nothing)  -> return (TxFeeImplicit implicit)
      (Right explicit, Just fee) -> return (TxFeeExplicit explicit fee)

      (Right _, Nothing) -> txFeatureMismatch era TxFeatureImplicitFees
      (Left  _, Just _)  -> txFeatureMismatch era TxFeatureExplicitFees


validateTxValidityLowerBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityLowerBound era)
validateTxValidityLowerBound _ Nothing = return TxValidityNoLowerBound
validateTxValidityLowerBound era (Just slot) =
    case validityLowerBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityLowerBound
      Just supported -> return (TxValidityLowerBound supported slot)


validateTxValidityUpperBound :: CardanoEra era
                             -> Maybe SlotNo
                             -> ExceptT ShelleyTxCmdError IO
                                        (TxValidityUpperBound era)
validateTxValidityUpperBound era Nothing =
    case validityNoUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityNoUpperBound
      Just supported -> return (TxValidityNoUpperBound supported)
validateTxValidityUpperBound era (Just slot) =
    case validityUpperBoundSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureValidityUpperBound
      Just supported -> return (TxValidityUpperBound supported slot)


validateTxMetadataInEra :: CardanoEra era
                        -> TxMetadataJsonSchema
                        -> [MetadataFile]
                        -> ExceptT ShelleyTxCmdError IO (TxMetadataInEra era)
validateTxMetadataInEra _ _ [] = return TxMetadataNone
validateTxMetadataInEra era schema files =
    case txMetadataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureTxMetadata
      Just supported -> do
        metadata <- mconcat <$> mapM (readFileTxMetadata schema) files
        return (TxMetadataInEra supported metadata)


validateTxAuxScripts :: CardanoEra era
                     -> [ScriptFile]
                     -> ExceptT ShelleyTxCmdError IO (TxAuxScripts era)
validateTxAuxScripts _ [] = return TxAuxScriptsNone
validateTxAuxScripts era files =
  case auxScriptsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureAuxScripts
    Just supported -> do
      scripts <- sequence
        [ do script <- firstExceptT ShelleyTxCmdScriptFileError $
                         readFileScriptInAnyLang file
             validateScriptSupportedInEra era script
        | ScriptFile file <- files ]
      return $ TxAuxScripts supported scripts

validateTxWithdrawals
  :: forall era.
     CardanoEra era
  -> [(StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT ShelleyTxCmdError IO (TxWithdrawals BuildTx era)
validateTxWithdrawals _ [] = return TxWithdrawalsNone
validateTxWithdrawals era withdrawals =
  case withdrawalsSupportedInEra era of
    Nothing -> txFeatureMismatch era TxFeatureWithdrawals
    Just supported -> do
      convWithdrawals <- mapM convert withdrawals
      return (TxWithdrawals supported convWithdrawals)
 where
  convert
    :: (StakeAddress, Lovelace, Maybe (ScriptWitnessFiles WitCtxStake))
    -> ExceptT ShelleyTxCmdError IO
              (StakeAddress,
               Lovelace,
               BuildTxWith BuildTx (Witness WitCtxStake era))
  convert (sAddr, ll, mScriptWitnessFiles) =
    case mScriptWitnessFiles of
      Just scriptWitnessFiles -> do
        sWit <- createScriptWitness era scriptWitnessFiles
        return ( sAddr
               , ll
               , BuildTxWith $ ScriptWitness ScriptWitnessForStakeAddr sWit
               )
      Nothing -> return (sAddr,ll, BuildTxWith $ KeyWitness KeyWitnessForStakeAddr)

validateTxCertificates
  :: forall era.
     CardanoEra era
  -> [(CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))]
  -> ExceptT ShelleyTxCmdError IO (TxCertificates BuildTx era)
validateTxCertificates era certFiles =
  case certificatesSupportedInEra era of
    Nothing
      | null certFiles -> return TxCertificatesNone
      | otherwise      -> txFeatureMismatch era TxFeatureCertificates
    Just supported -> do
      certs <- sequence
                 [ firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT $
                     readFileTextEnvelope AsCertificate certFile
                 | CertificateFile certFile <- map fst certFiles ]
      reqWits <- Map.fromList . catMaybes  <$> mapM convert certFiles
      return $ TxCertificates supported certs $ BuildTxWith reqWits
  where
   -- We get the stake credential witness for a certificate that requires it.
   -- NB: Only stake address deregistration and delegation requires
   -- witnessing (witness can be script or key)
   deriveStakeCredentialWitness
     :: CertificateFile
     -> ExceptT ShelleyTxCmdError IO (Maybe StakeCredential)
   deriveStakeCredentialWitness (CertificateFile certFile) = do
     cert <- firstExceptT ShelleyTxCmdReadTextViewFileError . newExceptT
               $ readFileTextEnvelope AsCertificate certFile
     case cert of
       StakeAddressDeregistrationCertificate sCred -> return $ Just sCred
       StakeAddressDelegationCertificate sCred _ -> return $ Just sCred
       _ -> return Nothing

   convert
     :: (CertificateFile, Maybe (ScriptWitnessFiles WitCtxStake))
     -> ExceptT ShelleyTxCmdError IO
                (Maybe (StakeCredential, Witness WitCtxStake era))
   convert (cert, mScriptWitnessFiles) = do
     mStakeCred <- deriveStakeCredentialWitness cert
     case mStakeCred of
       Nothing -> return Nothing
       Just sCred ->
         case mScriptWitnessFiles of
           Just scriptWitnessFiles -> do
            sWit <- createScriptWitness era scriptWitnessFiles
            return $ Just ( sCred
                          , ScriptWitness ScriptWitnessForStakeAddr sWit
                          )

           Nothing -> return $ Just (sCred, KeyWitness KeyWitnessForStakeAddr)

validateProtocolParameters
  :: CardanoEra era
  -> Maybe ProtocolParamsSourceSpec
  -> ExceptT ShelleyTxCmdError IO
            (BuildTxWith BuildTx (Maybe ProtocolParameters))
validateProtocolParameters _ Nothing = return (BuildTxWith Nothing)
validateProtocolParameters era (Just pparamsspec) =
    case scriptDataSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureProtocolParameters
      Just _  -> BuildTxWith . Just <$>
                   readProtocolParametersSourceSpec pparamsspec

validateTxUpdateProposal :: CardanoEra era
                         -> Maybe UpdateProposalFile
                         -> ExceptT ShelleyTxCmdError IO (TxUpdateProposal era)
validateTxUpdateProposal _ Nothing = return TxUpdateProposalNone
validateTxUpdateProposal era (Just (UpdateProposalFile file)) =
    case updateProposalSupportedInEra era of
      Nothing -> txFeatureMismatch era TxFeatureCertificates
      Just supported -> do
         prop <- firstExceptT ShelleyTxCmdReadTextViewFileError $ newExceptT $
                   readFileTextEnvelope AsUpdateProposal file
         return (TxUpdateProposal supported prop)


validateTxMintValue :: forall era.
                       CardanoEra era
                    -> Maybe (Value, [ScriptWitnessFiles WitCtxMint])
                    -> ExceptT ShelleyTxCmdError IO (TxMintValue BuildTx era)
validateTxMintValue _ Nothing = return TxMintNone
validateTxMintValue era (Just (val, scriptWitnessFiles)) =
    case multiAssetSupportedInEra era of
      Left _ -> txFeatureMismatch era TxFeatureMintValue
      Right supported -> do
        -- The set of policy ids for which we need witnesses:
        let witnessesNeededSet :: Set PolicyId
            witnessesNeededSet =
              Set.fromList [ pid | (AssetId pid _, _) <- valueToList val ]

        -- The set (and map) of policy ids for which we have witnesses:
        witnesses <- mapM (createScriptWitness era) scriptWitnessFiles
        let witnessesProvidedMap :: Map PolicyId (ScriptWitness WitCtxMint era)
            witnessesProvidedMap = Map.fromList
                                     [ (scriptWitnessPolicyId witness, witness)
                                     | witness <- witnesses ]
            witnessesProvidedSet = Map.keysSet witnessesProvidedMap

        -- Check not too many, nor too few:
        validateAllWitnessesProvided   witnessesNeededSet witnessesProvidedSet
        validateNoUnnecessaryWitnesses witnessesNeededSet witnessesProvidedSet

        return (TxMintValue supported val (BuildTxWith witnessesProvidedMap))
 where
    validateAllWitnessesProvided witnessesNeeded witnessesProvided
      | null witnessesMissing = return ()
      | otherwise = left (ShelleyTxCmdPolicyIdsMissing witnessesMissing)
      where
        witnessesMissing = Set.elems (witnessesNeeded Set.\\ witnessesProvided)

    validateNoUnnecessaryWitnesses witnessesNeeded witnessesProvided
      | null witnessesExtra = return ()
      | otherwise = left (ShelleyTxCmdPolicyIdsExcess witnessesExtra)
      where
        witnessesExtra = Set.elems (witnessesProvided Set.\\ witnessesNeeded)

scriptWitnessPolicyId :: ScriptWitness witctx era -> PolicyId
scriptWitnessPolicyId witness =
  case scriptWitnessScript witness of
    ScriptInEra _ script -> scriptPolicyId script


createScriptWitness
  :: CardanoEra era
  -> ScriptWitnessFiles witctx
  -> ExceptT ShelleyTxCmdError IO (ScriptWitness witctx era)
createScriptWitness era (SimpleScriptWitnessFile (ScriptFile scriptFile)) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ShelleyTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      SimpleScript version sscript ->
        return $ SimpleScriptWitness
                   langInEra version sscript

      -- If the supplied cli flags were for a simple script (i.e. the user did
      -- not supply the datum, redeemer or ex units), but the script file turns
      -- out to be a valid plutus script, then we must fail.
      PlutusScript{} ->
        left $ ShelleyTxCmdScriptExpectedSimple
                 scriptFile
                 (AnyScriptLanguage lang)

createScriptWitness era (PlutusScriptWitnessFiles
                          (ScriptFile scriptFile)
                          datumOrFile
                          redeemerOrFile
                          execUnits) = do
    script@(ScriptInAnyLang lang _) <- firstExceptT ShelleyTxCmdScriptFileError $
                                         readFileScriptInAnyLang scriptFile
    ScriptInEra langInEra script'   <- validateScriptSupportedInEra era script
    case script' of
      PlutusScript version pscript -> do
        datum    <- readScriptDatumOrFile    datumOrFile
        redeemer <- readScriptRedeemerOrFile redeemerOrFile
        return $ PlutusScriptWitness
                   langInEra version pscript
                   datum
                   redeemer
                   execUnits

      -- If the supplied cli flags were for a plutus script (i.e. the user did
      -- supply the datum, redeemer and ex units), but the script file turns
      -- out to be a valid simple script, then we must fail.
      SimpleScript{} ->
        left $ ShelleyTxCmdScriptExpectedPlutus
                 scriptFile
                 (AnyScriptLanguage lang)


readScriptDatumOrFile :: ScriptDatumOrFile witctx
                      -> ExceptT ShelleyTxCmdError IO (ScriptDatum witctx)
readScriptDatumOrFile (ScriptDatumOrFileForTxIn df) = ScriptDatumForTxIn <$>
                                                        readScriptDataOrFile df
readScriptDatumOrFile NoScriptDatumOrFileForMint    = pure NoScriptDatumForMint
readScriptDatumOrFile NoScriptDatumOrFileForStake   = pure NoScriptDatumForStake

readScriptRedeemerOrFile :: ScriptRedeemerOrFile
                         -> ExceptT ShelleyTxCmdError IO ScriptRedeemer
readScriptRedeemerOrFile = readScriptDataOrFile

readScriptDataOrFile :: ScriptDataOrFile
                     -> ExceptT ShelleyTxCmdError IO ScriptData
readScriptDataOrFile (ScriptDataValue d) = return d
readScriptDataOrFile (ScriptDataFile fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
            LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdScriptDataJsonParseError fp) $
            hoistEither $
              Aeson.eitherDecode' bs
    sd <- firstExceptT (ShelleyTxCmdScriptDataConversionError fp) $
            hoistEither $
              scriptDataFromJson ScriptDataJsonDetailedSchema v
    firstExceptT (ShelleyTxCmdScriptDataValidationError fp) $
      hoistEither $
        validateScriptData sd
    return sd


-- ----------------------------------------------------------------------------
-- Transaction signing
--

runTxSign :: TxBodyFile
          -> [WitnessSigningData]
          -> Maybe NetworkId
          -> TxFile
          -> ExceptT ShelleyTxCmdError IO ()
runTxSign (TxBodyFile txbodyFile) witSigningData mnw (TxFile txFile) = do

  InAnyShelleyBasedEra _era txbody <-
        --TODO: in principle we should be able to support Byron era txs too
        onlyInShelleyBasedEras "sign for Byron era transactions"
    =<< readFileTxBody txbodyFile

  sks <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError $
           mapM readWitnessSigningData witSigningData

  let (sksByron, sksShelley) = partitionSomeWitnesses $ map categoriseSomeWitness sks

  -- Byron witnesses require the network ID. This can either be provided
  -- directly or derived from a provided Byron address.
  byronWitnesses <- firstExceptT ShelleyTxCmdBootstrapWitnessError
    . hoistEither
    $ mkShelleyBootstrapWitnesses mnw txbody sksByron

  let shelleyKeyWitnesses = map (makeShelleyKeyWitness txbody) sksShelley
      tx = makeSignedTransaction (byronWitnesses ++ shelleyKeyWitnesses) txbody

  firstExceptT ShelleyTxCmdWriteFileError . newExceptT $
    writeFileTextEnvelope txFile Nothing tx


-- ----------------------------------------------------------------------------
-- Transaction submission
--


runTxSubmit
  :: AnyConsensusModeParams
  -> NetworkId
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO ()
runTxSubmit (AnyConsensusModeParams cModeParams) network txFile = do
    SocketPath sockPath <- firstExceptT ShelleyTxCmdSocketEnvError readEnvSocketPath

    InAnyCardanoEra era tx <- readFileTx txFile
    let cMode = AnyConsensusMode $ consensusModeOnly cModeParams
    eraInMode <- hoistMaybe
                   (ShelleyTxCmdEraConsensusModeMismatch (Just txFile) cMode (AnyCardanoEra era))
                   (toEraInMode era $ consensusModeOnly cModeParams)
    let txInMode = TxInMode tx eraInMode
        localNodeConnInfo = LocalNodeConnectInfo
                              { localConsensusModeParams = cModeParams
                              , localNodeNetworkId = network
                              , localNodeSocketPath = sockPath
                              }

    res <- liftIO $ submitTxToNodeLocal localNodeConnInfo txInMode
    case res of
      Net.Tx.SubmitSuccess -> liftIO $ putTextLn "Transaction successfully submitted."
      Net.Tx.SubmitFail reason ->
        case reason of
          TxValidationErrorInMode err _eraInMode -> left . ShelleyTxCmdTxSubmitError . Text.pack $ show err
          TxValidationEraMismatch mismatchErr -> left $ ShelleyTxCmdTxSubmitErrorEraMismatch mismatchErr

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinFee
  :: TxBodyFile
  -> Maybe NetworkId
  -> ProtocolParamsSourceSpec
  -> TxInCount
  -> TxOutCount
  -> TxShelleyWitnessCount
  -> TxByronWitnessCount
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinFee (TxBodyFile txbodyFile) nw protocolParamsSourceSpec
                     (TxInCount nInputs) (TxOutCount nOutputs)
                     (TxShelleyWitnessCount nShelleyKeyWitnesses)
                     (TxByronWitnessCount nByronKeyWitnesses) = do

    InAnyShelleyBasedEra _era txbody <-
          --TODO: in principle we should be able to support Byron era txs too
          onlyInShelleyBasedEras "calculate-min-fee for Byron era transactions"
      =<< readFileTxBody txbodyFile

    pparams <- readProtocolParametersSourceSpec protocolParamsSourceSpec

    let tx = makeSignedTransaction [] txbody
        Lovelace fee = estimateTransactionFee
                             (fromMaybe Mainnet nw)
                             (protocolParamTxFeeFixed pparams)
                             (protocolParamTxFeePerByte pparams)
                             tx
                             nInputs nOutputs
                             nByronKeyWitnesses nShelleyKeyWitnesses

    liftIO $ putStrLn $ (show fee :: String) <> " Lovelace"

-- ----------------------------------------------------------------------------
-- Transaction fee calculation
--

runTxCalculateMinValue
  :: ProtocolParamsSourceSpec
  -> Value
  -> ExceptT ShelleyTxCmdError IO ()
runTxCalculateMinValue protocolParamsSourceSpec value = do
  pp <- readProtocolParametersSourceSpec protocolParamsSourceSpec

  let minValues =
        case protocolParamMinUTxOValue pp of
          Nothing -> panic "TODO alonzo: runTxCalculateMinValue using new protocol params"
          --TODO alonzo: there is a new formula for the min amount of ada in
          -- a tx output, which uses a new param protocolParamUTxOCostPerWord
          Just minUTxOValue -> calcMinimumDeposit value minUTxOValue

  liftIO $ IO.print minValues

runTxCreatePolicyId :: ScriptFile -> ExceptT ShelleyTxCmdError IO ()
runTxCreatePolicyId (ScriptFile sFile) = do
  ScriptInAnyLang _ script <- firstExceptT ShelleyTxCmdScriptFileError $
                                readFileScriptInAnyLang sFile
  liftIO . putTextLn . serialiseToRawBytesHexText $ hashScript script

readProtocolParametersSourceSpec :: ProtocolParamsSourceSpec
                                 -> ExceptT ShelleyTxCmdError IO
                                            ProtocolParameters
readProtocolParametersSourceSpec (ParamsFromGenesis (GenesisFile f)) =
    fromShelleyPParams . sgProtocolParams <$>
      firstExceptT ShelleyTxCmdGenesisCmdError
        (readShelleyGenesis f identity)
readProtocolParametersSourceSpec (ParamsFromFile f) =
    readProtocolParameters f

--TODO: eliminate this and get only the necessary params, and get them in a more
-- helpful way rather than requiring them as a local file.
readProtocolParameters :: ProtocolParamsFile
                       -> ExceptT ShelleyTxCmdError IO ProtocolParameters
readProtocolParameters (ProtocolParamsFile fpath) = do
  pparams <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fpath) $ LBS.readFile fpath
  firstExceptT (ShelleyTxCmdAesonDecodeProtocolParamsError fpath . Text.pack) . hoistEither $
    Aeson.eitherDecode' pparams


-- ----------------------------------------------------------------------------
-- Witness handling
--

data SomeWitness
  = AByronSigningKey           (SigningKey ByronKey) (Maybe (Address ByronAddr))
  | APaymentSigningKey         (SigningKey PaymentKey)
  | APaymentExtendedSigningKey (SigningKey PaymentExtendedKey)
  | AStakeSigningKey           (SigningKey StakeKey)
  | AStakeExtendedSigningKey   (SigningKey StakeExtendedKey)
  | AStakePoolSigningKey       (SigningKey StakePoolKey)
  | AGenesisSigningKey         (SigningKey GenesisKey)
  | AGenesisExtendedSigningKey (SigningKey GenesisExtendedKey)
  | AGenesisDelegateSigningKey (SigningKey GenesisDelegateKey)
  | AGenesisDelegateExtendedSigningKey
                               (SigningKey GenesisDelegateExtendedKey)
  | AGenesisUTxOSigningKey     (SigningKey GenesisUTxOKey)


-- | Error reading the data required to construct a key witness.
data ReadWitnessSigningDataError
  = ReadWitnessSigningDataSigningKeyDecodeError !(FileError InputDecodeError)
  | ReadWitnessSigningDataScriptError !(FileError JsonDecodeError)
  | ReadWitnessSigningDataSigningKeyAndAddressMismatch
  -- ^ A Byron address was specified alongside a non-Byron signing key.
  deriving Show

-- | Render an error message for a 'ReadWitnessSigningDataError'.
renderReadWitnessSigningDataError :: ReadWitnessSigningDataError -> Text
renderReadWitnessSigningDataError err =
  case err of
    ReadWitnessSigningDataSigningKeyDecodeError fileErr ->
      "Error reading signing key: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataScriptError fileErr ->
      "Error reading script: " <> Text.pack (displayError fileErr)
    ReadWitnessSigningDataSigningKeyAndAddressMismatch ->
      "Only a Byron signing key may be accompanied by a Byron address."

readWitnessSigningData
  :: WitnessSigningData
  -> ExceptT ReadWitnessSigningDataError IO SomeWitness
readWitnessSigningData (KeyWitnessSigningData skFile mbByronAddr) = do
    res <- firstExceptT ReadWitnessSigningDataSigningKeyDecodeError
      . newExceptT
      $ readSigningKeyFileAnyOf bech32FileTypes textEnvFileTypes skFile
    case (res, mbByronAddr) of
      (AByronSigningKey _ _, Just _) -> pure res
      (AByronSigningKey _ _, Nothing) -> pure res
      (_, Nothing) -> pure res
      (_, Just _) ->
        -- A Byron address should only be specified along with a Byron signing key.
        left ReadWitnessSigningDataSigningKeyAndAddressMismatch
  where
    textEnvFileTypes =
      [ FromSomeType (AsSigningKey AsByronKey)
                          (`AByronSigningKey` mbByronAddr)
      , FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      , FromSomeType (AsSigningKey AsGenesisKey)
                          AGenesisSigningKey
      , FromSomeType (AsSigningKey AsGenesisExtendedKey)
                          AGenesisExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateKey)
                          AGenesisDelegateSigningKey
      , FromSomeType (AsSigningKey AsGenesisDelegateExtendedKey)
                          AGenesisDelegateExtendedSigningKey
      , FromSomeType (AsSigningKey AsGenesisUTxOKey)
                          AGenesisUTxOSigningKey
      ]

    bech32FileTypes =
      [ FromSomeType (AsSigningKey AsPaymentKey)
                          APaymentSigningKey
      , FromSomeType (AsSigningKey AsPaymentExtendedKey)
                          APaymentExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakeKey)
                          AStakeSigningKey
      , FromSomeType (AsSigningKey AsStakeExtendedKey)
                          AStakeExtendedSigningKey
      , FromSomeType (AsSigningKey AsStakePoolKey)
                          AStakePoolSigningKey
      ]

partitionSomeWitnesses
  :: [ByronOrShelleyWitness]
  -> ( [ShelleyBootstrapWitnessSigningKeyData]
     , [ShelleyWitnessSigningKey]
     )
partitionSomeWitnesses = reversePartitionedWits . foldl' go mempty
  where
    reversePartitionedWits (bw, skw) =
      (reverse bw, reverse skw)

    go (byronAcc, shelleyKeyAcc) byronOrShelleyWit =
      case byronOrShelleyWit of
        AByronWitness byronWit ->
          (byronWit:byronAcc, shelleyKeyAcc)
        AShelleyKeyWitness shelleyKeyWit ->
          (byronAcc, shelleyKeyWit:shelleyKeyAcc)


-- | Some kind of Byron or Shelley witness.
data ByronOrShelleyWitness
  = AByronWitness !ShelleyBootstrapWitnessSigningKeyData
  | AShelleyKeyWitness !ShelleyWitnessSigningKey

categoriseSomeWitness :: SomeWitness -> ByronOrShelleyWitness
categoriseSomeWitness swsk =
  case swsk of
    AByronSigningKey           sk addr -> AByronWitness (ShelleyBootstrapWitnessSigningKeyData sk addr)
    APaymentSigningKey         sk      -> AShelleyKeyWitness (WitnessPaymentKey         sk)
    APaymentExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessPaymentExtendedKey sk)
    AStakeSigningKey           sk      -> AShelleyKeyWitness (WitnessStakeKey           sk)
    AStakeExtendedSigningKey   sk      -> AShelleyKeyWitness (WitnessStakeExtendedKey   sk)
    AStakePoolSigningKey       sk      -> AShelleyKeyWitness (WitnessStakePoolKey       sk)
    AGenesisSigningKey         sk      -> AShelleyKeyWitness (WitnessGenesisKey sk)
    AGenesisExtendedSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisExtendedKey sk)
    AGenesisDelegateSigningKey sk      -> AShelleyKeyWitness (WitnessGenesisDelegateKey sk)
    AGenesisDelegateExtendedSigningKey sk
                                       -> AShelleyKeyWitness (WitnessGenesisDelegateExtendedKey sk)
    AGenesisUTxOSigningKey     sk      -> AShelleyKeyWitness (WitnessGenesisUTxOKey     sk)

-- | Data required for constructing a Shelley bootstrap witness.
data ShelleyBootstrapWitnessSigningKeyData
  = ShelleyBootstrapWitnessSigningKeyData
      !(SigningKey ByronKey)
      -- ^ Byron signing key.
      !(Maybe (Address ByronAddr))
      -- ^ An optionally specified Byron address.
      --
      -- If specified, both the network ID and derivation path are extracted
      -- from the address and used in the construction of the Byron witness.

-- | Error constructing a Shelley bootstrap witness (i.e. a Byron key witness
-- in the Shelley era).
data ShelleyBootstrapWitnessError
  = MissingNetworkIdOrByronAddressError
  -- ^ Neither a network ID nor a Byron address were provided to construct the
  -- Shelley bootstrap witness. One or the other is required.
  deriving Show

-- | Render an error message for a 'ShelleyBootstrapWitnessError'.
renderShelleyBootstrapWitnessError :: ShelleyBootstrapWitnessError -> Text
renderShelleyBootstrapWitnessError MissingNetworkIdOrByronAddressError =
  "Transactions witnessed by a Byron signing key must be accompanied by a "
    <> "network ID. Either provide a network ID or provide a Byron "
    <> "address with each Byron signing key (network IDs can be derived "
    <> "from Byron addresses)."

-- | Construct a Shelley bootstrap witness (i.e. a Byron key witness in the
-- Shelley era).
mkShelleyBootstrapWitness
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> ShelleyBootstrapWitnessSigningKeyData
  -> Either ShelleyBootstrapWitnessError (KeyWitness era)
mkShelleyBootstrapWitness Nothing _ (ShelleyBootstrapWitnessSigningKeyData _ Nothing) =
  Left MissingNetworkIdOrByronAddressError
mkShelleyBootstrapWitness (Just nw) txBody (ShelleyBootstrapWitnessSigningKeyData skey Nothing) =
  Right $ makeShelleyBootstrapWitness (WitnessNetworkId nw) txBody skey
mkShelleyBootstrapWitness _ txBody (ShelleyBootstrapWitnessSigningKeyData skey (Just addr)) =
  Right $ makeShelleyBootstrapWitness (WitnessByronAddress addr) txBody skey

-- | Attempt to construct Shelley bootstrap witnesses until an error is
-- encountered.
mkShelleyBootstrapWitnesses
  :: IsShelleyBasedEra era
  => Maybe NetworkId
  -> TxBody era
  -> [ShelleyBootstrapWitnessSigningKeyData]
  -> Either ShelleyBootstrapWitnessError [KeyWitness era]
mkShelleyBootstrapWitnesses mnw txBody =
  mapM (mkShelleyBootstrapWitness mnw txBody)


-- ----------------------------------------------------------------------------
-- Other misc small commands
--

runTxHashScriptData :: ScriptDataOrFile -> ExceptT ShelleyTxCmdError IO ()
runTxHashScriptData scriptDataOrFile = do
    d <- readScriptDataOrFile scriptDataOrFile
    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (hashScriptData d)

runTxGetTxId :: InputTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxGetTxId txfile = do
    InAnyCardanoEra _era txbody <-
      case txfile of
        InputTxBodyFile (TxBodyFile txbodyFile) -> readFileTxBody txbodyFile
        InputTxFile (TxFile txFile) -> do
          InAnyCardanoEra era tx <- readFileTx txFile
          return . InAnyCardanoEra era $ getTxBody tx

    liftIO $ BS.putStrLn $ serialiseToRawBytesHex (getTxId txbody)

runTxView :: InputTxFile -> ExceptT ShelleyTxCmdError IO ()
runTxView txfile = do
  InAnyCardanoEra era txbody <-
    case txfile of
      InputTxBodyFile (TxBodyFile txbodyFile) -> readFileTxBody txbodyFile
      InputTxFile (TxFile txFile) -> do
        InAnyCardanoEra era tx <- readFileTx txFile
        return . InAnyCardanoEra era $ getTxBody tx
  liftIO $ BS.putStr $ friendlyTxBodyBS era txbody


-- ----------------------------------------------------------------------------
-- Witness commands
--

runTxCreateWitness
  :: TxBodyFile
  -> WitnessSigningData
  -> Maybe NetworkId
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxCreateWitness (TxBodyFile txbodyFile) witSignData mbNw (OutputFile oFile) = do

  InAnyShelleyBasedEra _era txbody <-
        --TODO: in principle we should be able to support Byron era txs too
        onlyInShelleyBasedEras "witness for Byron era transactions"
    =<< readFileTxBody txbodyFile
  -- We use the era of the tx we read to determine the era we use for the rest:

  someWit <- firstExceptT ShelleyTxCmdReadWitnessSigningDataError
    $ readWitnessSigningData witSignData

  witness <-
    case categoriseSomeWitness someWit of
      -- Byron witnesses require the network ID. This can either be provided
      -- directly or derived from a provided Byron address.
      AByronWitness bootstrapWitData ->
        firstExceptT ShelleyTxCmdBootstrapWitnessError
          . hoistEither
          $ mkShelleyBootstrapWitness mbNw txbody bootstrapWitData
      AShelleyKeyWitness skShelley ->
        pure $ makeShelleyKeyWitness txbody skShelley

  firstExceptT ShelleyTxCmdWriteFileError
    . newExceptT
    $ writeFileTextEnvelope oFile Nothing witness


runTxSignWitness
  :: TxBodyFile
  -> [WitnessFile]
  -> OutputFile
  -> ExceptT ShelleyTxCmdError IO ()
runTxSignWitness (TxBodyFile txbodyFile) witnessFiles (OutputFile oFp) = do

    InAnyCardanoEra era txbody  <- readFileTxBody txbodyFile
    InAnyShelleyBasedEra _ _ <-
          --TODO: in principle we should be able to support Byron era txs too
          onlyInShelleyBasedEras "sign for Byron era transactions"
                                 (InAnyCardanoEra era txbody)

    witnesses <-
      sequence
        [ do InAnyCardanoEra era' witness <- readFileWitness file
             case testEquality era era' of
               Nothing   -> left $ ShelleyTxCmdWitnessEraMismatch
                                     (AnyCardanoEra era)
                                     (AnyCardanoEra era')
                                     witnessFile
               Just Refl -> return witness
        | witnessFile@(WitnessFile file) <- witnessFiles ]

    let tx = makeSignedTransaction witnesses txbody
    firstExceptT ShelleyTxCmdWriteFileError
      . newExceptT
      $ writeFileTextEnvelope oFp Nothing tx


-- ----------------------------------------------------------------------------
-- Reading files in any era
--

readFileWitness :: FilePath
                -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra KeyWitness)
readFileWitness = readFileInAnyCardanoEra AsKeyWitness


readFileTxBody :: FilePath
               -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra TxBody)
readFileTxBody = readFileInAnyCardanoEra AsTxBody


readFileTx :: FilePath -> ExceptT ShelleyTxCmdError IO (InAnyCardanoEra Tx)
readFileTx = readFileInAnyCardanoEra AsTx


readFileInAnyCardanoEra
  :: ( HasTextEnvelope (thing ByronEra)
     , HasTextEnvelope (thing ShelleyEra)
     , HasTextEnvelope (thing AllegraEra)
     , HasTextEnvelope (thing MaryEra)
     , HasTextEnvelope (thing AlonzoEra)
     )
  => (forall era. AsType era -> AsType (thing era))
  -> FilePath
  -> ExceptT ShelleyTxCmdError IO
            (InAnyCardanoEra thing)
readFileInAnyCardanoEra asThing file =
    firstExceptT ShelleyTxCmdReadTextViewFileError
  . newExceptT
  $ readFileTextEnvelopeAnyOf
      [ FromSomeType (asThing AsByronEra)   (InAnyCardanoEra ByronEra)
      , FromSomeType (asThing AsShelleyEra) (InAnyCardanoEra ShelleyEra)
      , FromSomeType (asThing AsAllegraEra) (InAnyCardanoEra AllegraEra)
      , FromSomeType (asThing AsMaryEra)    (InAnyCardanoEra MaryEra)
      , FromSomeType (asThing AsAlonzoEra)  (InAnyCardanoEra AlonzoEra)
      ]
      file

-- | Constrain the era to be Shelley based. Fail for the Byron era.
--
onlyInShelleyBasedEras :: Text
                       -> InAnyCardanoEra a
                       -> ExceptT ShelleyTxCmdError IO
                                  (InAnyShelleyBasedEra a)
onlyInShelleyBasedEras notImplMsg (InAnyCardanoEra era x) =
    case cardanoEraStyle era of
      LegacyByronEra       -> left (ShelleyTxCmdNotImplemented notImplMsg)
      ShelleyBasedEra era' -> return (InAnyShelleyBasedEra era' x)


-- ----------------------------------------------------------------------------
-- Reading other files
--

validateScriptSupportedInEra :: CardanoEra era
                             -> ScriptInAnyLang
                             -> ExceptT ShelleyTxCmdError IO (ScriptInEra era)
validateScriptSupportedInEra era script@(ScriptInAnyLang lang _) =
    case toScriptInEra era script of
      Nothing -> left $ ShelleyTxCmdScriptLanguageNotSupportedInEra
                          (AnyScriptLanguage lang) (anyCardanoEra era)
      Just script' -> pure script'


-- ----------------------------------------------------------------------------
-- Transaction metadata
--

readFileTxMetadata :: TxMetadataJsonSchema -> MetadataFile
                   -> ExceptT ShelleyTxCmdError IO TxMetadata
readFileTxMetadata mapping (MetadataFileJSON fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          LBS.readFile fp
    v  <- firstExceptT (ShelleyTxCmdMetadataJsonParseError fp) $
          hoistEither $
            Aeson.eitherDecode' bs
    txMetadata <- firstExceptT (ShelleyTxCmdMetadataConversionError fp) $ hoistEither $
      metadataFromJson mapping v
    firstExceptT (ShelleyTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata

readFileTxMetadata _ (MetadataFileCBOR fp) = do
    bs <- handleIOExceptT (ShelleyTxCmdReadFileError . FileIOError fp) $
          BS.readFile fp
    txMetadata <- firstExceptT (ShelleyTxCmdMetaDecodeError fp) $ hoistEither $
      deserialiseFromCBOR AsTxMetadata bs
    firstExceptT (ShelleyTxCmdMetaValidationError fp) $ hoistEither $ do
        validateTxMetadata txMetadata
        return txMetadata

executeQuery
  :: forall result era mode. CardanoEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT ShelleyTxCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ByronEraInByronMode -> left ShelleyTxCmdByronEraQuery
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquireFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q


queryResult
  :: Either AcquireFailure (Either EraMismatch a)
  -> ExceptT ShelleyTxCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ ShelleyTxCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . ShelleyTxCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyTxCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (ShelleyTxCmdEraConsensusModeMismatchQuery (AnyConsensusMode mode) (anyCardanoEra era))
                   $ toEraInMode era mode

getSbe :: CardanoEraStyle era
       -> ExceptT ShelleyTxCmdError IO (ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyTxCmdByronEra
getSbe (ShelleyBasedEra sbe) = return sbe


