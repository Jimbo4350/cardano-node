{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Era.Shelley ()
  where

import           Cardano.Prelude
import           Cardano.Logging

import           Data.Aeson(ToJSON, Value(..), (.=))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Cardano.Tracing.OrphanInstances.Shelley()

import           Cardano.Api.Orphans ()

import           Cardano.Slotting.Block (BlockNo (..))

import           Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import           Ouroboros.Consensus.Util.Condense (condense)
import           Ouroboros.Network.Block (SlotNo (..), blockHash, blockNo, blockSlot)
import           Ouroboros.Network.Point (WithOrigin, withOriginToMaybe)

import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)
import           Ouroboros.Consensus.Shelley.Ledger.Inspect
import           Ouroboros.Consensus.Shelley.Protocol (TPraosCannotForge (..))
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

-- import qualified Cardano.Ledger.AuxiliaryData as Core
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.ShelleyMA.Rules.Utxo as MA
import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail (..))
import qualified Cardano.Ledger.Alonzo.Rules.Utxo as Alonzo
-- import qualified Cardano.Ledger.ShelleyMA.Timelocks as MA

-- TODO: this should be exposed via Cardano.Api
import           Shelley.Spec.Ledger.API hiding (ShelleyBasedEra)
import           Shelley.Spec.Ledger.BlockChain (LastAppliedBlock (..))

import           Shelley.Spec.Ledger.STS.Bbody
import           Shelley.Spec.Ledger.STS.Chain
import           Shelley.Spec.Ledger.STS.Deleg
import           Shelley.Spec.Ledger.STS.Delegs
import           Shelley.Spec.Ledger.STS.Delpl
import           Shelley.Spec.Ledger.STS.Epoch
import           Shelley.Spec.Ledger.STS.Ledger
import           Shelley.Spec.Ledger.STS.Ledgers
import           Shelley.Spec.Ledger.STS.Mir
import           Shelley.Spec.Ledger.STS.NewEpoch
import           Shelley.Spec.Ledger.STS.Newpp
import           Shelley.Spec.Ledger.STS.Ocert
import           Shelley.Spec.Ledger.STS.Overlay
import           Shelley.Spec.Ledger.STS.Pool
import           Shelley.Spec.Ledger.STS.PoolReap
import           Shelley.Spec.Ledger.STS.Ppup
import           Shelley.Spec.Ledger.STS.Rupd
import           Shelley.Spec.Ledger.STS.Snap
import           Shelley.Spec.Ledger.STS.Tick
import           Shelley.Spec.Ledger.STS.Updn
import           Shelley.Spec.Ledger.STS.Upec
import           Shelley.Spec.Ledger.STS.Utxo
import           Shelley.Spec.Ledger.STS.Utxow

{- HLINT ignore "Use :" -}

--
-- | instances of @LogFormatting@
--
-- NOTE: this list is sorted in roughly topological order.

instance ShelleyBasedEra era => LogFormatting (GenTx (ShelleyBlock era)) where
  forMachine dtal tx =
    mkObject $
        ( "txid" .= txId tx )
      : [ "tx"   .= condense tx | dtal == DDetailed ]

-- instance ToJSON (SupportsMempool.TxId (GenTx (ShelleyBlock era))) where
--   toJSON i = toJSON (condense i)

instance ShelleyBasedEra era => LogFormatting (Header (ShelleyBlock era)) where
  forMachine _dtal b = mkObject
        [ "kind" .= String "ShelleyBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
--      , "delegate" .= condense (headerSignerVk h)
        ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => LogFormatting (ApplyTxError era) where
  forMachine dtal (ApplyTxError predicateFailures) =
    HMS.unions $ map (forMachine dtal) predicateFailures

instance LogFormatting (TPraosCannotForge era) where
  forMachine _dtal (TPraosCannotForgeKeyNotUsableYet wallClockPeriod keyStartPeriod) =
    mkObject
      [ "kind" .= String "TPraosCannotForgeKeyNotUsableYet"
      , "keyStart" .= keyStartPeriod
      , "wallClock" .= wallClockPeriod
      ]
  forMachine _dtal (TPraosCannotForgeWrongVRF genDlgVRFHash coreNodeVRFHash) =
    mkObject
      [ "kind" .= String "TPraosCannotLeadWrongVRF"
      , "expected" .= genDlgVRFHash
      , "actual" .= coreNodeVRFHash
      ]

-- deriving newtype instance ToJSON KESPeriod

instance LogFormatting HotKey.KESInfo where
  forMachine _dtal forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in
      if (kesPeriodsUntilExpiry > 7)
        then mkObject
              [ "kind" .= String "KESInfo"
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod
              , "evolution" .= endKesPeriod
              ]
        else mkObject
              [ "kind" .= String "ExpiryLogMessage"
              , "keyExpiresIn" .= kesPeriodsUntilExpiry
              , "startPeriod" .= startKesPeriod
              , "endPeriod" .= currKesPeriod
              , "evolution" .= endKesPeriod
              ]
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  forHuman forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in if (kesPeriodsUntilExpiry > 7)
      then "KES info startPeriod  " <> show startKesPeriod
            <> " currPeriod " <> show currKesPeriod
            <> " endPeriod " <> show endKesPeriod
             <> (Text.pack . show) kesPeriodsUntilExpiry
             <> " KES periods."
      else "Operational key will expire in "
             <> (Text.pack . show) kesPeriodsUntilExpiry
             <> " KES periods."
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

  asMetrics forgeStateInfo =
      let maxKesEvos = endKesPeriod - startKesPeriod
          oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
          -- TODO JNF: What is the sense of it?
      in  [
            IntM ["operationalCertificateStartKESPeriod"]
              (fromIntegral startKesPeriod)
          , IntM ["operationalCertificateExpiryKESPeriod"]
              (fromIntegral (startKesPeriod + maxKesEvos))
          , IntM ["currentKESPeriod"]
              (fromIntegral currKesPeriod)
          , IntM ["remainingKESPeriods"]
              (fromIntegral (max 0 (oCertExpiryKesPeriod - currKesPeriod)))
          ]
    where
    HotKey.KESInfo
      { kesStartPeriod = KESPeriod startKesPeriod
      , kesEvolution = currKesPeriod
      , kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo


instance LogFormatting HotKey.KESEvolutionError where
  forMachine dtal (HotKey.KESCouldNotEvolve kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESCouldNotEvolve"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]
  forMachine dtal (HotKey.KESKeyAlreadyPoisoned kesInfo targetPeriod) =
    mkObject
      [ "kind" .= String "KESKeyAlreadyPoisoned"
      , "kesInfo" .= forMachine dtal kesInfo
      , "targetPeriod" .= targetPeriod
      ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "BBODY" era))
         ) => LogFormatting (ShelleyLedgerError era) where
  forMachine dtal (BBodyError (BlockTransitionError fs)) =
    mkObject [ "kind" .= String "BBodyError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.PParamsDelta era)
         ) => LogFormatting (ShelleyLedgerUpdate era) where
  forMachine dtal (ShelleyUpdatedProtocolUpdates updates) =
    mkObject [ "kind" .= String "ShelleyUpdatedProtocolUpdates"
             , "updates" .= map (forMachine dtal) updates
             ]

instance ToJSON (Core.PParamsDelta era)
         => LogFormatting (ProtocolUpdate era) where
  forMachine dtal ProtocolUpdate{protocolUpdateProposal, protocolUpdateState} =
    mkObject [ "proposal" .= forMachine dtal protocolUpdateProposal
             , "state"    .= forMachine dtal protocolUpdateState
             ]

instance ToJSON (Core.PParamsDelta era)
         => LogFormatting (UpdateProposal era) where
  forMachine _dtal UpdateProposal{proposalParams, proposalVersion, proposalEpoch} =
    mkObject [ "params"  .= proposalParams
             , "version" .= proposalVersion
             , "epoch"   .= proposalEpoch
             ]

instance LogFormatting (UpdateState crypto) where
  forMachine _dtal UpdateState{proposalVotes, proposalReachedQuorum} =
    mkObject [ "proposal"      .= proposalVotes
             , "reachedQuorum" .= proposalReachedQuorum
             ]

instance Core.Crypto crypto => LogFormatting (ChainTransitionError crypto) where
  forMachine dtal (ChainTransitionError fs) =
    mkObject [ "kind" .= String "ChainTransitionError"
             , "failures" .= map (forMachine dtal) fs
             ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (Core.EraRule "UTXOW" era))
         , LogFormatting (PredicateFailure (Core.EraRule "BBODY" era))
         , LogFormatting (PredicateFailure (Core.EraRule "TICK" era))
         , LogFormatting (PredicateFailure (Core.EraRule "TICKN" era))
         ) => LogFormatting (ChainPredicateFailure era) where
  forMachine _dtal (HeaderSizeTooLargeCHAIN hdrSz maxHdrSz) =
    mkObject [ "kind" .= String "HeaderSizeTooLarge"
             , "headerSize" .= hdrSz
             , "maxHeaderSize" .= maxHdrSz
             ]
  forMachine _dtal (BlockSizeTooLargeCHAIN blkSz maxBlkSz) =
    mkObject [ "kind" .= String "BlockSizeTooLarge"
             , "blockSize" .= blkSz
             , "maxBlockSize" .= maxBlkSz
             ]
  forMachine _dtal (ObsoleteNodeCHAIN currentPtcl supportedPtcl) =
    mkObject [ "kind" .= String "ObsoleteNode"
             , "explanation" .= String explanation
             , "currentProtocol" .= currentPtcl
             , "supportedProtocol" .= supportedPtcl ]
      where
        explanation = "A scheduled major protocol version change (hard fork) \
                      \has taken place on the chain, but this node does not \
                      \understand the new major protocol version. This node \
                      \must be upgraded before it can continue with the new \
                      \protocol version."
  forMachine dtal (BbodyFailure f) = forMachine dtal f
  forMachine dtal (TickFailure  f) = forMachine dtal f
  forMachine dtal (TicknFailure  f) = forMachine dtal f
  forMachine dtal (PrtclFailure f) = forMachine dtal f
  forMachine dtal (PrtclSeqFailure f) = forMachine dtal f

instance LogFormatting (PrtlSeqFailure crypto) where
  forMachine _dtal (WrongSlotIntervalPrtclSeq (SlotNo lastSlot) (SlotNo currSlot)) =
    mkObject [ "kind" .= String "WrongSlotInterval"
             , "lastSlot" .= lastSlot
             , "currentSlot" .= currSlot
             ]
  forMachine _dtal (WrongBlockNoPrtclSeq lab currentBlockNo) =
    mkObject [ "kind" .= String "WrongBlockNo"
             , "lastAppliedBlockNo" .= showLastAppBlockNo lab
             , "currentBlockNo" .= (String . textShow $ unBlockNo currentBlockNo)
             ]
  forMachine _dtal (WrongBlockSequencePrtclSeq lastAppliedHash currentHash) =
    mkObject [ "kind" .= String "WrongBlockSequence"
             , "lastAppliedBlockHash" .= String (textShow lastAppliedHash)
             , "currentBlockHash" .= String (textShow currentHash)
             ]

instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGERS" era))
         ) => LogFormatting (BbodyPredicateFailure era) where
  forMachine _dtal (WrongBlockBodySizeBBODY actualBodySz claimedBodySz) =
    mkObject [ "kind" .= String "WrongBlockBodySizeBBODY"
             , "actualBlockBodySize" .= actualBodySz
             , "claimedBlockBodySize" .= claimedBodySz
             ]
  forMachine _dtal (InvalidBodyHashBBODY actualHash claimedHash) =
    mkObject [ "kind" .= String "InvalidBodyHashBBODY"
             , "actualBodyHash" .= textShow actualHash
             , "claimedBodyHash" .= textShow claimedHash
             ]
  forMachine dtal (LedgersFailure f) = forMachine dtal f


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "LEDGER" era))
         ) => LogFormatting (LedgersPredicateFailure era) where
  forMachine dtal (LedgerFailure f) = forMachine dtal f


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (UTXOW era))
         , LogFormatting (PredicateFailure (Core.EraRule "DELEGS" era))
         , LogFormatting (PredicateFailure (Core.EraRule "UTXOW" era))
         ) => LogFormatting (LedgerPredicateFailure era) where
  forMachine dtal (UtxowFailure f) = forMachine dtal f
  forMachine dtal (DelegsFailure f) = forMachine dtal f


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (UTXO era))
         , LogFormatting (PredicateFailure (Core.EraRule "UTXO" era))
         ) => LogFormatting (UtxowPredicateFailure era) where
  forMachine _dtal (InvalidWitnessesUTXOW wits') =
    mkObject [ "kind" .= String "InvalidWitnessesUTXOW"
             , "invalidWitnesses" .= map textShow wits'
             ]
  forMachine _dtal (MissingVKeyWitnessesUTXOW (WitHashes wits')) =
    mkObject [ "kind" .= String "MissingVKeyWitnessesUTXOW"
             , "missingWitnesses" .= wits'
             ]
  forMachine _dtal (MissingScriptWitnessesUTXOW missingScripts) =
    mkObject [ "kind" .= String "MissingScriptWitnessesUTXOW"
             , "missingScripts" .= missingScripts
             ]
  forMachine _dtal (ScriptWitnessNotValidatingUTXOW failedScripts) =
    mkObject [ "kind" .= String "ScriptWitnessNotValidatingUTXOW"
             , "failedScripts" .= failedScripts
             ]
  forMachine dtal (UtxoFailure f) = forMachine dtal f
  forMachine _dtal (MIRInsufficientGenesisSigsUTXOW genesisSigs) =
    mkObject [ "kind" .= String "MIRInsufficientGenesisSigsUTXOW"
             , "genesisSigs" .= genesisSigs
             ]
  forMachine _dtal (MissingTxBodyMetadataHash metadataHash) =
    mkObject [ "kind" .= String "MissingTxBodyMetadataHash"
             , "metadataHash" .= metadataHash
             ]
  forMachine _dtal (MissingTxMetadata txBodyMetadataHash) =
    mkObject [ "kind" .= String "MissingTxMetadata"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             ]
  forMachine _dtal (ConflictingMetadataHash txBodyMetadataHash fullMetadataHash) =
    mkObject [ "kind" .= String "ConflictingMetadataHash"
             , "txBodyMetadataHash" .= txBodyMetadataHash
             , "fullMetadataHash" .= fullMetadataHash
             ]
  forMachine _dtal InvalidMetadata =
    mkObject [ "kind" .= String "InvalidMetadata"
             ]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , LogFormatting (PredicateFailure (Core.EraRule "PPUP" era))
         )
      => LogFormatting (UtxoPredicateFailure era) where
  forMachine _dtal (BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (ExpiredUTxO ttl slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "ttl"  .= ttl
             , "slot" .= slot ]
  forMachine _dtal (MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  forMachine _dtal (OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine dtal (UpdateFailure f) = forMachine dtal f

  forMachine _dtal (WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]

-- instance ToJSON MA.ValidityInterval where
--   toJSON vi =
--     Aeson.object $
--         [ "invalidBefore"    .= x | x <- mbfield (MA.invalidBefore    vi) ]
--      ++ [ "invalidHereafter" .= x | x <- mbfield (MA.invalidHereafter vi) ]
--     where
--       mbfield SNothing  = []
--       mbfield (SJust x) = [x]

instance ( ShelleyBasedEra era
         , ToJSON (Core.Value era)
         , ToJSON (Core.TxOut era)
         , LogFormatting (PredicateFailure (Core.EraRule "PPUP" era))
         ) => LogFormatting (MA.UtxoPredicateFailure era) where
  forMachine _dtal (MA.BadInputsUTxO badInputs) =
    mkObject [ "kind" .= String "BadInputsUTxO"
             , "badInputs" .= badInputs
             , "error" .= renderBadInputsUTxOErr badInputs
             ]
  forMachine _dtal (MA.OutsideValidityIntervalUTxO validityInterval slot) =
    mkObject [ "kind" .= String "ExpiredUTxO"
             , "validityInterval" .= validityInterval
             , "slot" .= slot ]
  forMachine _dtal (MA.MaxTxSizeUTxO txsize maxtxsize) =
    mkObject [ "kind" .= String "MaxTxSizeUTxO"
             , "size" .= txsize
             , "maxSize" .= maxtxsize ]
  forMachine _dtal MA.InputSetEmptyUTxO =
    mkObject [ "kind" .= String "InputSetEmptyUTxO" ]
  forMachine _dtal (MA.FeeTooSmallUTxO minfee txfee) =
    mkObject [ "kind" .= String "FeeTooSmallUTxO"
             , "minimum" .= minfee
             , "fee" .= txfee ]
  forMachine _dtal (MA.ValueNotConservedUTxO consumed produced) =
    mkObject [ "kind" .= String "ValueNotConservedUTxO"
             , "consumed" .= consumed
             , "produced" .= produced
             , "error" .= renderValueNotConservedErr consumed produced
             ]
  forMachine _dtal (MA.WrongNetwork network addrs) =
    mkObject [ "kind" .= String "WrongNetwork"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  forMachine _dtal (MA.WrongNetworkWithdrawal network addrs) =
    mkObject [ "kind" .= String "WrongNetworkWithdrawal"
             , "network" .= network
             , "addrs"   .= addrs
             ]
  -- TODO: Add the minimum allowed UTxO value to OutputTooSmallUTxO
  forMachine _dtal (MA.OutputTooSmallUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooSmallUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "The output is smaller than the allow minimum \
                                 \UTxO value defined in the protocol parameters"
             ]
  forMachine dtal (MA.UpdateFailure f) = forMachine dtal f
  forMachine _dtal (MA.OutputBootAddrAttrsTooBig badOutputs) =
    mkObject [ "kind" .= String "OutputBootAddrAttrsTooBig"
             , "outputs" .= badOutputs
             , "error" .= String "The Byron address attributes are too big"
             ]
  forMachine _dtal MA.TriesToForgeADA =
    mkObject [ "kind" .= String "TriesToForgeADA" ]
  forMachine _dtal (MA.OutputTooBigUTxO badOutputs) =
    mkObject [ "kind" .= String "OutputTooBigUTxO"
             , "outputs" .= badOutputs
             , "error" .= String "Too many asset ids in the tx output"
             ]

renderBadInputsUTxOErr ::  Set (TxIn era) -> Value
renderBadInputsUTxOErr txIns
  | Set.null txIns = String "The transaction contains no inputs."
  | otherwise = String "The transaction contains inputs that do not exist in the UTxO set."

renderValueNotConservedErr :: Show val => val -> val -> Value
renderValueNotConservedErr consumed produced = String $
    "This transaction consumed " <> show consumed <> " but produced " <> show produced

instance LogFormatting (PpupPredicateFailure era) where
  forMachine _dtal (NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  forMachine _dtal (PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  forMachine _dtal (PVCannotFollowPPUP badPv) =
    mkObject [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]


instance ( ShelleyBasedEra era
         , LogFormatting (PredicateFailure (Core.EraRule "DELPL" era))
         ) => LogFormatting (DelegsPredicateFailure era) where
  forMachine _dtal (DelegateeNotRegisteredDELEG targetPool) =
    mkObject [ "kind" .= String "DelegateeNotRegisteredDELEG"
             , "targetPool" .= targetPool
             ]
  forMachine _dtal (WithdrawalsNotInRewardsDELEGS incorrectWithdrawals) =
    mkObject [ "kind" .= String "WithdrawalsNotInRewardsDELEGS"
             , "incorrectWithdrawals" .= incorrectWithdrawals
             ]
  forMachine dtal (DelplFailure f) = forMachine dtal f


instance ( LogFormatting (PredicateFailure (Core.EraRule "POOL" era))
         , LogFormatting (PredicateFailure (Core.EraRule "DELEG" era))
         ) => LogFormatting (DelplPredicateFailure era) where
  forMachine dtal (PoolFailure f) = forMachine dtal f
  forMachine dtal (DelegFailure f) = forMachine dtal f

instance LogFormatting (DelegPredicateFailure era) where
  forMachine _dtal (StakeKeyAlreadyRegisteredDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyAlreadyRegisteredDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential already registered"
             ]
  forMachine _dtal (StakeKeyInRewardsDELEG alreadyRegistered) =
    mkObject [ "kind" .= String "StakeKeyInRewardsDELEG"
             , "credential" .= String (textShow alreadyRegistered)
             , "error" .= String "Staking credential registered in rewards map"
             ]
  forMachine _dtal (StakeKeyNotRegisteredDELEG notRegistered) =
    mkObject [ "kind" .= String "StakeKeyNotRegisteredDELEG"
             , "credential" .= String (textShow notRegistered)
             , "error" .= String "Staking credential not registered"
             ]
  forMachine _dtal (StakeKeyNonZeroAccountBalanceDELEG remBalance) =
    mkObject [ "kind" .= String "StakeKeyNonZeroAccountBalanceDELEG"
             , "remainingBalance" .= remBalance
             ]
  forMachine _dtal (StakeDelegationImpossibleDELEG unregistered) =
    mkObject [ "kind" .= String "StakeDelegationImpossibleDELEG"
             , "credential" .= String (textShow unregistered)
             , "error" .= String "Cannot delegate this stake credential because it is not registered"
             ]
  forMachine _dtal WrongCertificateTypeDELEG =
    mkObject [ "kind" .= String "WrongCertificateTypeDELEG" ]
  forMachine _dtal (GenesisKeyNotInMappingDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "GenesisKeyNotInMappingDELEG"
             , "unknownKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key is not in the delegation mapping"
             ]
  forMachine _dtal (DuplicateGenesisDelegateDELEG (KeyHash genesisKeyHash)) =
    mkObject [ "kind" .= String "DuplicateGenesisDelegateDELEG"
             , "duplicateKeyHash" .= String (textShow genesisKeyHash)
             , "error" .= String "This genesis key has already been delegated to"
             ]
  forMachine _dtal (InsufficientForInstantaneousRewardsDELEG mirpot neededMirAmount reserves) =
    mkObject [ "kind" .= String "InsufficientForInstantaneousRewardsDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "neededAmount" .= neededMirAmount
             , "reserves" .= reserves
             ]
  forMachine _dtal (MIRCertificateTooLateinEpochDELEG currSlot boundSlotNo) =
    mkObject [ "kind" .= String "MIRCertificateTooLateinEpochDELEG"
             , "currentSlotNo" .= currSlot
             , "mustBeSubmittedBeforeSlotNo" .= boundSlotNo
             ]
  forMachine _dtal (DuplicateGenesisVRFDELEG vrfKeyHash) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "keyHash" .= vrfKeyHash
             ]
  forMachine _dtal MIRTransferNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRTransferNotCurrentlyAllowed"
             ]
  forMachine _dtal MIRNegativesNotCurrentlyAllowed =
    mkObject [ "kind" .= String "MIRNegativesNotCurrentlyAllowed"
             ]
  forMachine _dtal (InsufficientForTransferDELEG mirpot attempted available) =
    mkObject [ "kind" .= String "DuplicateGenesisVRFDELEG"
             , "pot" .= String (case mirpot of
                                  ReservesMIR -> "Reserves"
                                  TreasuryMIR -> "Treasury")
             , "attempted" .= attempted
             , "available" .= available
             ]
  forMachine _dtal MIRProducesNegativeUpdate =
    mkObject [ "kind" .= String "MIRProducesNegativeUpdate"
             ]


instance LogFormatting (PoolPredicateFailure era) where
  forMachine _dtal (StakePoolNotRegisteredOnKeyPOOL (KeyHash unregStakePool)) =
    mkObject [ "kind" .= String "StakePoolNotRegisteredOnKeyPOOL"
             , "unregisteredKeyHash" .= String (textShow unregStakePool)
             , "error" .= String "This stake pool key hash is unregistered"
             ]
  forMachine _dtal (StakePoolRetirementWrongEpochPOOL currentEpoch intendedRetireEpoch maxRetireEpoch) =
    mkObject [ "kind" .= String "StakePoolRetirementWrongEpochPOOL"
             , "currentEpoch" .= String (textShow currentEpoch)
             , "intendedRetirementEpoch" .= String (textShow intendedRetireEpoch)
             , "maxEpochForRetirement" .= String (textShow maxRetireEpoch)
             ]
  forMachine _dtal (StakePoolCostTooLowPOOL certCost protCost) =
    mkObject [ "kind" .= String "StakePoolCostTooLowPOOL"
             , "certificateCost" .= String (textShow certCost)
             , "protocolParCost" .= String (textShow protCost)
             , "error" .= String "The stake pool cost is too low"
             ]


-- Apparently this should never happen according to the Shelley exec spec
  forMachine _dtal (WrongCertificateTypePOOL index) =
    case index of
      0 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Delegation certificate"
                    ]
      1 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: MIR certificate"
                    ]
      2 -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "error" .= String "Wrong certificate type: Genesis certificate"
                    ]
      k -> mkObject [ "kind" .= String "WrongCertificateTypePOOL"
                    , "certificateType" .= k
                    , "error" .= String "Wrong certificate type: Unknown certificate type"
                    ]

  forMachine _dtal (WrongNetworkPOOL networkId listedNetworkId poolId) =
    mkObject [ "kind" .= String "WrongNetworkPOOL"
             , "networkId" .= String (textShow networkId)
             , "listedNetworkId" .= String (textShow listedNetworkId)
             , "poolId" .= String (textShow poolId)
             , "error" .= String "Wrong network ID in pool registration certificate"
             ]


instance ( LogFormatting (PredicateFailure (Core.EraRule "NEWEPOCH" era))
         , LogFormatting (PredicateFailure (Core.EraRule "RUPD" era))
         ) => LogFormatting (TickPredicateFailure era) where
  forMachine dtal (NewEpochFailure f) = forMachine dtal f
  forMachine dtal (RupdFailure f) = forMachine dtal f

instance LogFormatting TicknPredicateFailure where
  forMachine _dtal x = case x of {} -- no constructors

instance ( LogFormatting (PredicateFailure (Core.EraRule "EPOCH" era))
         , LogFormatting (PredicateFailure (Core.EraRule "MIR" era))
         ) => LogFormatting (NewEpochPredicateFailure era) where
  forMachine dtal (EpochFailure f) = forMachine dtal f
  forMachine dtal (MirFailure f) = forMachine dtal f
  forMachine _dtal (CorruptRewardUpdate update) =
    mkObject [ "kind" .= String "CorruptRewardUpdate"
             , "update" .= String (show update) ]


instance ( LogFormatting (PredicateFailure (Core.EraRule "POOLREAP" era))
         , LogFormatting (PredicateFailure (Core.EraRule "SNAP" era))
         , LogFormatting (PredicateFailure (Core.EraRule "UPEC" era))
         ) => LogFormatting (EpochPredicateFailure era) where
  forMachine dtal (PoolReapFailure f) = forMachine dtal f
  forMachine dtal (SnapFailure f) = forMachine dtal f
  forMachine dtal (UpecFailure f) = forMachine dtal f


instance LogFormatting (PoolreapPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (SnapPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors

-- TODO: Need to elaborate more on this error
instance LogFormatting (NewppPredicateFailure era) where
  forMachine _dtal (UnexpectedDepositPot outstandingDeposits depositPot) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "outstandingDeposits" .= String (textShow outstandingDeposits)
             , "depositPot" .= String (textShow depositPot)
             ]


instance LogFormatting (MirPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance LogFormatting (RupdPredicateFailure era) where
  forMachine _dtal x = case x of {} -- no constructors


instance Core.Crypto crypto => LogFormatting (PrtclPredicateFailure crypto) where
  forMachine  dtal (OverlayFailure f) = forMachine dtal f
  forMachine  dtal (UpdnFailure f) = forMachine dtal f


instance Core.Crypto crypto => LogFormatting (OverlayPredicateFailure crypto) where
  forMachine _dtal (UnknownGenesisKeyOVERLAY (KeyHash genKeyHash)) =
    mkObject [ "kind" .= String "UnknownGenesisKeyOVERLAY"
             , "unknownKeyHash" .= String (textShow genKeyHash)
             ]
  forMachine _dtal (VRFKeyBadLeaderValue seedNonce (SlotNo currSlotNo) prevHashNonce leaderElecVal) =
    mkObject [ "kind" .= String "VRFKeyBadLeaderValueOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "leaderElectionValue" .= String (textShow leaderElecVal)
             ]
  forMachine _dtal (VRFKeyBadNonce seedNonce (SlotNo currSlotNo) prevHashNonce blockNonce) =
    mkObject [ "kind" .= String "VRFKeyBadNonceOVERLAY"
             , "seedNonce" .= String (textShow seedNonce)
             , "currentSlot" .= String (textShow currSlotNo)
             , "previousHashAsNonce" .= String (textShow prevHashNonce)
             , "blockNonce" .= String (textShow blockNonce)
             ]
  forMachine _dtal (VRFKeyWrongVRFKey issuerHash regVRFKeyHash unregVRFKeyHash) =
    mkObject [ "kind" .= String "VRFKeyWrongVRFKeyOVERLAY"
             , "poolHash" .= textShow issuerHash
             , "registeredVRFKeHash" .= textShow regVRFKeyHash
             , "unregisteredVRFKeyHash" .= textShow unregVRFKeyHash
             ]
  --TODO: Pipe slot number with VRFKeyUnknown
  forMachine _dtal (VRFKeyUnknown (KeyHash kHash)) =
    mkObject [ "kind" .= String "VRFKeyUnknownOVERLAY"
             , "keyHash" .= String (textShow kHash)
             ]
  forMachine _dtal (VRFLeaderValueTooBig leadElecVal weightOfDelegPool actSlotCoefff) =
    mkObject [ "kind" .= String "VRFLeaderValueTooBigOVERLAY"
             , "leaderElectionValue" .= String (textShow leadElecVal)
             , "delegationPoolWeight" .= String (textShow weightOfDelegPool)
             , "activeSlotCoefficient" .= String (textShow actSlotCoefff)
             ]
  forMachine _dtal (NotActiveSlotOVERLAY notActiveSlotNo) =
    -- TODO: Elaborate on NotActiveSlot error
    mkObject [ "kind" .= String "NotActiveSlotOVERLAY"
             , "slot" .= String (textShow notActiveSlotNo)
             ]
  forMachine _dtal (WrongGenesisColdKeyOVERLAY actual expected) =
    mkObject [ "kind" .= String "WrongGenesisColdKeyOVERLAY"
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine _dtal (WrongGenesisVRFKeyOVERLAY issuer actual expected) =
    mkObject [ "kind" .= String "WrongGenesisVRFKeyOVERLAY"
             , "issuer" .= issuer
             , "actual" .= actual
             , "expected" .= expected ]
  forMachine dtal (OcertFailure f) = forMachine dtal f


instance LogFormatting (OcertPredicateFailure crypto) where
  forMachine _dtal (KESBeforeStartOCERT (KESPeriod oCertstart) (KESPeriod current)) =
    mkObject [ "kind" .= String "KESBeforeStartOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "currentKESPeriod" .= String (textShow current)
             , "error" .= String "Your operational certificate's KES start period \
                                 \is before the KES current period."
             ]
  forMachine _dtal (KESAfterEndOCERT (KESPeriod current) (KESPeriod oCertstart) maxKESEvolutions) =
    mkObject [ "kind" .= String "KESAfterEndOCERT"
             , "currentKESPeriod" .= String (textShow current)
             , "opCertKESStartPeriod" .= String (textShow oCertstart)
             , "maxKESEvolutions" .= String  (textShow maxKESEvolutions)
             , "error" .= String "The operational certificate's KES start period is \
                                 \greater than the max number of KES + the KES current period"
             ]
  forMachine _dtal (CounterTooSmallOCERT lastKEScounterUsed currentKESCounter) =
    mkObject [ "kind" .= String "CounterTooSmallOCert"
             , "currentKESCounter" .= String (textShow currentKESCounter)
             , "lastKESCounter" .= String (textShow lastKEScounterUsed)
             , "error" .= String "The operational certificate's last KES counter is greater \
                                 \than the current KES counter."
             ]
  forMachine _dtal (InvalidSignatureOCERT oCertCounter oCertKESStartPeriod) =
    mkObject [ "kind" .= String "InvalidSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow oCertKESStartPeriod)
             , "opCertCounter" .= String (textShow oCertCounter)
             ]
  forMachine _dtal (InvalidKesSignatureOCERT currKESPeriod startKESPeriod expectedKESEvolutions err) =
    mkObject [ "kind" .= String "InvalidKesSignatureOCERT"
             , "opCertKESStartPeriod" .= String (textShow startKESPeriod)
             , "opCertKESCurrentPeriod" .= String (textShow currKESPeriod)
             , "opCertExpectedKESEvolutions" .= String (textShow expectedKESEvolutions)
             , "error" .= err ]
  forMachine _dtal (NoCounterForKeyHashOCERT (KeyHash stakePoolKeyHash)) =
    mkObject [ "kind" .= String "NoCounterForKeyHashOCERT"
             , "stakePoolKeyHash" .= String (textShow stakePoolKeyHash)
             , "error" .= String "A counter was not found for this stake pool key hash"
             ]


instance LogFormatting (UpdnPredicateFailure crypto) where
  forMachine _dtal x = case x of {} -- no constructors

instance LogFormatting (UpecPredicateFailure era) where
  forMachine _dtal (NewPpFailure (UnexpectedDepositPot totalOutstanding depositPot)) =
    mkObject [ "kind" .= String "UnexpectedDepositPot"
             , "totalOutstanding" .=  String (textShow totalOutstanding)
             , "depositPot" .= String (textShow depositPot)
             ]

instance LogFormatting (Alonzo.UtxoPredicateFailure (AlonzoEra StandardCrypto)) where
  forMachine _ _ = panic "ToJSON: UtxoPredicateFailure not implemented yet"

instance LogFormatting (AlonzoBbodyPredFail (AlonzoEra StandardCrypto)) where
  forMachine _ _ = panic "ToJSON: AlonzoBbodyPredFail not implemented yet"

instance LogFormatting (AlonzoPredFail (AlonzoEra StandardCrypto)) where
  forMachine _ _ = panic "ToJSON: AlonzoPredFail not implemented yet"

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = Text.pack . show

showLastAppBlockNo :: WithOrigin (LastAppliedBlock crypto) -> Text
showLastAppBlockNo wOblk =  case withOriginToMaybe wOblk of
                     Nothing -> "Genesis Block"
                     Just blk -> textShow . unBlockNo $ labBlockNo blk

-- Common to cardano-cli

-- deriving newtype instance Core.Crypto crypto => ToJSON (Core.AuxiliaryDataHash crypto)
--
-- deriving newtype instance ToJSON (TxId crypto)
