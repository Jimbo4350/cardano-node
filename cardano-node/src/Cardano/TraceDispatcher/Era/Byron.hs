{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-unused-imports  #-}

module Cardano.TraceDispatcher.Era.Byron () where

-- TODO: Temporary hack for toJSON instances
import Cardano.Tracing.OrphanInstances.Byron ()

import           Cardano.Logging
import           Cardano.Prelude
import           Data.Aeson (Value (String), (.=))

import qualified Data.Set as Set
import qualified Data.Text as Text

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Network.Block (blockHash, blockNo, blockSlot)

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock (..),
                     ByronOtherHeaderEnvelopeError (..), TxId (..),
                     byronHeaderRaw)
import           Ouroboros.Consensus.Byron.Ledger.Inspect
                     (ByronLedgerUpdate (..), ProtocolUpdate (..),
                     UpdateState (..))
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, txId)
import           Ouroboros.Consensus.Util.Condense (condense)

import           Cardano.Chain.Block (ABlockOrBoundaryHdr (..), AHeader (..),
                     ChainValidationError (..), delegationCertificate)
import           Cardano.Chain.Byron.API (ApplyMempoolPayloadErr (..))
import           Cardano.Chain.Delegation (delegateVK)
import           Cardano.Crypto.Signing (VerificationKey)

{- HLINT ignore "Use :" -}

--
-- | instances of @LogFormatting@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance LogFormatting ApplyMempoolPayloadErr where
  forMachine _dtal (MempoolTxErr utxoValidationErr) =
    mkObject
      [ "kind" .= String "MempoolTxErr"
      , "error" .= String (show utxoValidationErr)
      ]
  forMachine _dtal (MempoolDlgErr delegScheduleError) =
    mkObject
      [ "kind" .= String "MempoolDlgErr"
      , "error" .= String (show delegScheduleError)
      ]
  forMachine _dtal (MempoolUpdateProposalErr iFaceErr) =
    mkObject
      [ "kind" .= String "MempoolUpdateProposalErr"
      , "error" .= String (show iFaceErr)
      ]
  forMachine _dtal (MempoolUpdateVoteErr iFaceErrr) =
    mkObject
      [ "kind" .= String "MempoolUpdateVoteErr"
      , "error" .= String (show iFaceErrr)
      ]

instance LogFormatting ByronLedgerUpdate where
  forMachine dtal (ByronUpdatedProtocolUpdates protocolUpdates) =
    mkObject
      [ "kind"            .= String "ByronUpdatedProtocolUpdates"
      , "protocolUpdates" .= map (forMachine dtal) protocolUpdates
      ]

instance LogFormatting ProtocolUpdate where
  forMachine dtal (ProtocolUpdate updateVersion updateState) =
    mkObject
      [ "kind"                  .= String "ProtocolUpdate"
      , "protocolUpdateVersion" .= updateVersion
      , "protocolUpdateState"   .= forMachine dtal updateState
      ]

instance LogFormatting UpdateState where
  forMachine _dtal updateState = case updateState of
      UpdateRegistered slot ->
        mkObject
          [ "kind" .= String "UpdateRegistered"
          , "slot" .= slot
          ]
      UpdateActive votes ->
        mkObject
          [ "kind"  .= String "UpdateActive"
          , "votes" .= map (Text.pack . show) (Set.toList votes)
          ]
      UpdateConfirmed slot ->
        mkObject
          [ "kind" .= String "UpdateConfirmed"
          , "slot" .= slot
          ]
      UpdateStablyConfirmed endorsements ->
        mkObject
          [ "kind"         .= String "UpdateStablyConfirmed"
          , "endorsements" .= map (Text.pack . show) (Set.toList endorsements)
          ]
      UpdateCandidate slot epoch ->
        mkObject
          [ "kind" .= String "UpdateCandidate"
          , "slot" .= slot
          , "epoch" .= epoch
          ]
      UpdateStableCandidate transitionEpoch ->
        mkObject
          [ "kind"            .= String "UpdateStableCandidate"
          , "transitionEpoch" .= transitionEpoch
          ]

instance LogFormatting (GenTx ByronBlock) where
  forMachine dtal tx =
    mkObject $
        ( "txid" .= txId tx )
     :  [ "tx"   .= condense tx | dtal == DDetailed ]


-- instance ToJSON (TxId (GenTx ByronBlock)) where
--   toJSON (ByronTxId             i) = toJSON (condense i)
--   toJSON (ByronDlgId            i) = toJSON (condense i)
--   toJSON (ByronUpdateProposalId i) = toJSON (condense i)
--   toJSON (ByronUpdateVoteId     i) = toJSON (condense i)
--

instance LogFormatting ChainValidationError where
  forMachine _dtal ChainValidationBoundaryTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBoundaryTooLarge" ]
  forMachine _dtal ChainValidationBlockAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationBlockAttributesTooLarge" ]
  forMachine _dtal (ChainValidationBlockTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationBlockTooLarge" ]
  forMachine _dtal ChainValidationHeaderAttributesTooLarge =
    mkObject
      [ "kind" .= String "ChainValidationHeaderAttributesTooLarge" ]
  forMachine _dtal (ChainValidationHeaderTooLarge _ _) =
    mkObject
      [ "kind" .= String "ChainValidationHeaderTooLarge" ]
  forMachine _dtal (ChainValidationDelegationPayloadError err) =
    mkObject
      [ "kind" .= String err ]
  forMachine _dtal (ChainValidationInvalidDelegation _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidDelegation" ]
  forMachine _dtal (ChainValidationGenesisHashMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationGenesisHashMismatch" ]
  forMachine _dtal (ChainValidationExpectedGenesisHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedGenesisHash" ]
  forMachine _dtal (ChainValidationExpectedHeaderHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationExpectedHeaderHash" ]
  forMachine _dtal (ChainValidationInvalidHash _ _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidHash" ]
  forMachine _dtal (ChainValidationMissingHash _) =
    mkObject
      [ "kind" .= String "ChainValidationMissingHash" ]
  forMachine _dtal (ChainValidationUnexpectedGenesisHash _) =
    mkObject
      [ "kind" .= String "ChainValidationUnexpectedGenesisHash" ]
  forMachine _dtal (ChainValidationInvalidSignature _) =
    mkObject
      [ "kind" .= String "ChainValidationInvalidSignature" ]
  forMachine _dtal (ChainValidationDelegationSchedulingError _) =
    mkObject
      [ "kind" .= String "ChainValidationDelegationSchedulingError" ]
  forMachine _dtal (ChainValidationProtocolMagicMismatch _ _) =
    mkObject
      [ "kind" .= String "ChainValidationProtocolMagicMismatch" ]
  forMachine _dtal ChainValidationSignatureLight =
    mkObject
      [ "kind" .= String "ChainValidationSignatureLight" ]
  forMachine _dtal (ChainValidationTooManyDelegations _) =
    mkObject
      [ "kind" .= String "ChainValidationTooManyDelegations" ]
  forMachine _dtal (ChainValidationUpdateError _ _) =
    mkObject
      [ "kind" .= String "ChainValidationUpdateError" ]
  forMachine _dtal (ChainValidationUTxOValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationUTxOValidationError" ]
  forMachine _dtal (ChainValidationProofValidationError _) =
    mkObject
      [ "kind" .= String "ChainValidationProofValidationError" ]


instance LogFormatting (Header ByronBlock) where
  forMachine _dtal b =
    mkObject $
        [ "kind" .= String "ByronBlock"
        , "hash" .= condense (blockHash b)
        , "slotNo" .= condense (blockSlot b)
        , "blockNo" .= condense (blockNo b)
        ] <>
        case byronHeaderRaw b of
          ABOBBoundaryHdr{} -> []
          ABOBBlockHdr h ->
            [ "delegate" .= condense (headerSignerVk h) ]
   where
     headerSignerVk :: AHeader ByteString -> VerificationKey
     headerSignerVk =
       delegateVK . delegationCertificate . headerSignature


instance LogFormatting ByronOtherHeaderEnvelopeError where
  forMachine _dtal (UnexpectedEBBInSlot slot) =
    mkObject
      [ "kind" .= String "UnexpectedEBBInSlot"
      , "slot" .= slot
      ]
