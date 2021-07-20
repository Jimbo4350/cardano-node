{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | User-friendly pretty-printing for textual user interfaces (TUI)
module Cardano.CLI.Run.Friendly (friendlyTxBodyBS) where

import           Cardano.Prelude

import           Data.Aeson (Value (..), object, toJSON, (.=))
import qualified Data.Aeson as Aeson
import           Data.Yaml (array)
import           Data.Yaml.Pretty (defConfig, encodePretty, setConfCompare)

import           Cardano.Api as Api
import           Cardano.Api.Byron (Lovelace (..))
import           Cardano.Api.Shelley (Address (ShelleyAddress), StakeAddress (..))
import qualified Shelley.Spec.Ledger.API as Shelley

import           Cardano.CLI.Helpers (textShow)

friendlyTxBodyBS :: CardanoEra era -> TxBody era -> ByteString
friendlyTxBodyBS era =
  encodePretty (setConfCompare compare defConfig) . friendlyTxBody era

friendlyTxBody :: CardanoEra era -> TxBody era -> Aeson.Value
friendlyTxBody
  era
  (TxBody
    TxBodyContent
      { txIns
      , txOuts
      , txFee
      , txValidityRange
      , txMetadata
      , txAuxScripts
      , txWithdrawals
      , txCertificates
      , txUpdateProposal
      , txMintValue
      }) =
  object
    $   [ "era"     .= era
        , "fee"     .= friendlyFee txFee
        , "inputs"  .= friendlyInputs txIns
        , "outputs" .= map friendlyTxOut txOuts
        ]
    ++  [ "auxiliary scripts" .= friendlyAuxScripts txAuxScripts
        | Just _ <- [auxScriptsSupportedInEra era]
        ]
    ++  [ "certificates" .= friendlyCertificates txCertificates
        | Just _ <- [certificatesSupportedInEra era]
        ]
    ++  [ "metadata" .= friendlyMetadata txMetadata
        | Just _ <- [txMetadataSupportedInEra era]
        ]
    ++  [ "mint" .= friendlyMintValue txMintValue
        | Right _ <- [multiAssetSupportedInEra era]
        ]
    ++  [ "update proposal" .= friendlyUpdateProposal txUpdateProposal
        | Just _ <- [updateProposalSupportedInEra era]
        ]
    ++  friendlyValidityRange era txValidityRange
    ++  [ "withdrawals" .= friendlyWithdrawals txWithdrawals
        | Just _ <- [withdrawalsSupportedInEra era]
        ]

friendlyValidityRange
  :: CardanoEra era
  -> (TxValidityLowerBound era, TxValidityUpperBound era)
  -> [(Text, Aeson.Value)]
friendlyValidityRange era = \case
  ( TxValidityNoLowerBound,
    TxValidityUpperBound ValidityUpperBoundInShelleyEra ttl
    ) ->
      -- special case: in Shelley, upper bound is TTL, and no lower bound
      ["time to live" .= ttl]
  (lowerBound, upperBound) ->
    [ "validity range" .=
        object
          ( [ "lower bound" .=
                case lowerBound of
                  TxValidityNoLowerBound   -> Null
                  TxValidityLowerBound _ s -> toJSON s
            | isLowerBoundSupported
            ]
          ++
            [ "upper bound" .=
                case upperBound of
                  TxValidityNoUpperBound _ -> Null
                  TxValidityUpperBound _ s -> toJSON s
            | isUpperBoundSupported
            ]
          )
    | isLowerBoundSupported || isUpperBoundSupported
    ]
  where
    isLowerBoundSupported = isJust $ validityLowerBoundSupportedInEra era
    isUpperBoundSupported = isJust $ validityUpperBoundSupportedInEra era

friendlyWithdrawals :: TxWithdrawals ViewTx era -> Aeson.Value
friendlyWithdrawals TxWithdrawalsNone = Null
friendlyWithdrawals (TxWithdrawals _ withdrawals) =
  array
    [ object
        [ "address"     .= serialiseAddress addr
        , "network"     .= net
        , "credential"  .= cred
        , "amount"      .= friendlyLovelace amount
        ]
    | (addr@(StakeAddress net cred), amount, _) <- withdrawals
    ]

friendlyTxOut :: TxOut era -> Aeson.Value
friendlyTxOut (TxOut addr amount mdatum) =
  case addr of
    AddressInEra ByronAddressInAnyEra _ ->
      object $ ("address era" .= String "Byron") : common
    AddressInEra (ShelleyAddressInEra _) (ShelleyAddress net cred stake) ->
      object
        $   [ "address era"         .= String "Shelley"
            , "network"             .= net
            , "payment credential"  .= cred
            , "stake reference"     .= friendlyStakeReference stake
            ]
        ++  ["datum" .= datum | TxOutDatumHash _ datum <- [mdatum]]
        ++  common
  where
    common :: [(Text, Aeson.Value)]
    common =
      [ "address" .= serialiseAddressForTxOut addr
      , "amount"  .= friendlyTxOutValue amount
      ]

friendlyStakeReference :: Shelley.StakeReference crypto -> Aeson.Value
friendlyStakeReference = \case
  Shelley.StakeRefBase cred -> toJSON cred
  Shelley.StakeRefNull -> Null
  Shelley.StakeRefPtr ptr -> toJSON ptr

friendlyUpdateProposal :: TxUpdateProposal era -> Aeson.Value
friendlyUpdateProposal = \case
  TxUpdateProposalNone -> Null
  TxUpdateProposal _ p -> String $ textShow p

friendlyCertificates :: TxCertificates ViewTx era -> Aeson.Value
friendlyCertificates = \case
  TxCertificatesNone    -> Null
  TxCertificates _ cs _ -> toJSON $ map textShow cs

friendlyFee :: TxFee era -> Aeson.Value
friendlyFee = \case
  TxFeeImplicit _     -> "implicit"
  TxFeeExplicit _ fee -> friendlyLovelace fee

friendlyLovelace :: Lovelace -> Aeson.Value
friendlyLovelace (Lovelace value) = String $ textShow value <> " Lovelace"

friendlyMintValue :: TxMintValue ViewTx era -> Aeson.Value
friendlyMintValue = \case
  TxMintNone        -> Null
  TxMintValue _ v _ -> toJSON v

friendlyTxOutValue :: TxOutValue era -> Aeson.Value
friendlyTxOutValue = \case
  TxOutAdaOnly _ lovelace -> friendlyLovelace lovelace
  TxOutValue _ multiasset -> toJSON multiasset

friendlyMetadata :: TxMetadataInEra era -> Aeson.Value
friendlyMetadata = \case
  TxMetadataNone                   -> Null
  TxMetadataInEra _ (TxMetadata m) -> toJSON $ friendlyMetadataValue <$> m

friendlyMetadataValue :: TxMetadataValue -> Aeson.Value
friendlyMetadataValue = \case
  TxMetaNumber int   -> toJSON int
  TxMetaBytes  bytes -> String $ textShow bytes
  TxMetaList   lst   -> array $ map friendlyMetadataValue lst
  TxMetaMap    m     ->
    array
      [array [friendlyMetadataValue k, friendlyMetadataValue v] | (k, v) <- m]
  TxMetaText   text  -> toJSON text

friendlyAuxScripts :: TxAuxScripts era -> Aeson.Value
friendlyAuxScripts = \case
  TxAuxScriptsNone       -> Null
  TxAuxScripts _ scripts -> String $ textShow scripts

friendlyInputs :: [(TxIn, build)] -> Aeson.Value
friendlyInputs = toJSON . map fst
