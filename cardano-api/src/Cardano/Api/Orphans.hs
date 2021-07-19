{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Cardano.Api.Orphans () where

import           Prelude

import           Data.Aeson (FromJSON (..), ToJSON (..), object, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (FromJSONKey (..), ToJSONKey (..), toJSONKeyText)
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import           Control.Applicative
import           Control.Iterate.SetAlgebra (BiMap (..), Bimap)

import           Cardano.Ledger.BaseTypes (StrictMaybe (..), strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Slotting.Slot (SlotNo (..))

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Alonzo as Alonzo
import qualified Cardano.Ledger.Alonzo.Genesis as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Coin as Shelley
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Core as Ledger
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Ledger
import qualified Cardano.Ledger.Mary.Value as Mary
import qualified Cardano.Ledger.SafeHash as SafeHash
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Ouroboros.Consensus.Shelley.Eras as Consensus
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.Delegation.Certificates as Shelley
import qualified Shelley.Spec.Ledger.EpochBoundary as ShelleyEpoch
import qualified Shelley.Spec.Ledger.LedgerState as ShelleyLedger
import           Shelley.Spec.Ledger.PParams (PParamsUpdate)
import qualified Shelley.Spec.Ledger.RewardUpdate as Shelley
import qualified Shelley.Spec.Ledger.Rewards as Shelley

-- Orphan instances involved in the JSON output of the API queries.
-- We will remove/replace these as we provide more API wrapper types

instance ToJSON (Mary.Value era) where
  toJSON (Mary.Value l ps) =
    object
      [ "lovelace" .= toJSON l
      , "policies" .= toJSON ps
      ]

instance ToJSONKey Mary.AssetName where
  toJSONKey = toJSONKeyText render
    where
      render = Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON (Mary.PolicyID era) where
  toJSON (Mary.PolicyID (Shelley.ScriptHash h)) = Aeson.String (hashToText h)

instance ToJSONKey (Mary.PolicyID era) where
  toJSONKey = toJSONKeyText render
    where
      render (Mary.PolicyID (Shelley.ScriptHash h)) = hashToText h

instance ToJSON Mary.AssetName where
  toJSON = Aeson.String . Text.decodeLatin1 . B16.encode . Mary.assetName

instance ToJSON Shelley.AccountState where
  toJSON (Shelley.AccountState tr rs) = object [ "treasury" .= tr
                                               , "reserves" .= rs
                                               ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParams era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.EpochState era) where
  toJSON eState = object [ "esAccountState" .= Shelley.esAccountState eState
                         , "esSnapshots" .= Shelley.esSnapshots eState
                         , "esLState" .= Shelley.esLState eState
                         , "esPrevPp" .= Shelley.esPrevPp eState
                         , "esPp" .= Shelley.esPp eState
                         , "esNonMyopic" .= Shelley.esNonMyopic eState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.LedgerState era) where
  toJSON lState = object [ "utxoState" .= Shelley._utxoState lState
                         , "delegationState" .= Shelley._delegationState lState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         , ToJSON (Core.PParamsDelta era)
         ) => ToJSON (Shelley.UTxOState era) where
  toJSON utxoState = object [ "utxo" .= Shelley._utxo utxoState
                            , "deposited" .= Shelley._deposited utxoState
                            , "fees" .= Shelley._fees utxoState
                            , "ppups" .= Shelley._ppups utxoState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.PPUPState era) where
  toJSON ppUpState = object [ "proposals" .= Shelley.proposals ppUpState
                            , "futureProposals" .= Shelley.futureProposals ppUpState
                            ]

instance ( ToJSON (Core.PParamsDelta era)
         , Shelley.UsesPParams era
         ) => ToJSON (Shelley.ProposedPPUpdates era) where
  toJSON (Shelley.ProposedPPUpdates ppUpdates) = toJSON $ Map.toList ppUpdates

instance ToJSON (PParamsUpdate era) where
  toJSON pp =
    Aeson.object $
        [ "minFeeA"               .= x | x <- mbfield (Shelley._minfeeA pp) ]
     ++ [ "minFeeB"               .= x | x <- mbfield (Shelley._minfeeB pp) ]
     ++ [ "maxBlockBodySize"      .= x | x <- mbfield (Shelley._maxBBSize pp) ]
     ++ [ "maxTxSize"             .= x | x <- mbfield (Shelley._maxTxSize pp) ]
     ++ [ "maxBlockHeaderSize"    .= x | x <- mbfield (Shelley._maxBHSize pp) ]
     ++ [ "keyDeposit"            .= x | x <- mbfield (Shelley._keyDeposit pp) ]
     ++ [ "poolDeposit"           .= x | x <- mbfield (Shelley._poolDeposit pp) ]
     ++ [ "eMax"                  .= x | x <- mbfield (Shelley._eMax pp) ]
     ++ [ "nOpt"                  .= x | x <- mbfield (Shelley._nOpt pp) ]
     ++ [ "a0"                    .= x | x <- mbfield (Shelley._a0 pp) ]
     ++ [ "rho"                   .= x | x <- mbfield (Shelley._rho pp) ]
     ++ [ "tau"                   .= x | x <- mbfield (Shelley._tau pp) ]
     ++ [ "decentralisationParam" .= x | x <- mbfield (Shelley._d pp) ]
     ++ [ "extraEntropy"          .= x | x <- mbfield (Shelley._extraEntropy pp) ]
     ++ [ "protocolVersion"       .= x | x <- mbfield (Shelley._protocolVersion pp) ]
     ++ [ "minUTxOValue"          .= x | x <- mbfield (Shelley._minUTxOValue pp) ]
     ++ [ "minPoolCost"           .= x | x <- mbfield (Shelley._minPoolCost pp) ]
    where
      mbfield SNothing  = []
      mbfield (SJust x) = [x]

instance Crypto.Crypto crypto => ToJSON (Shelley.DPState crypto) where
  toJSON dpState = object [ "dstate" .= Shelley._dstate dpState
                          , "pstate" .= Shelley._pstate dpState
                          ]

instance Crypto.Crypto crypto => ToJSON (Shelley.DState crypto) where
  toJSON dState = object [ "rewards" .= Shelley._rewards dState
                         , "delegations" .= ShelleyLedger._delegations dState
                         , "ptrs" .= Shelley._ptrs dState
                         , "fGenDelegs" .= Map.toList (Shelley._fGenDelegs dState)
                         , "genDelegs" .= Shelley._genDelegs dState
                         , "irwd" .= Shelley._irwd dState
                         ]

instance ToJSON (ShelleyLedger.FutureGenDeleg crypto) where
  toJSON fGenDeleg =
    object [ "fGenDelegSlot" .= ShelleyLedger.fGenDelegSlot fGenDeleg
           , "fGenDelegGenKeyHash" .= ShelleyLedger.fGenDelegGenKeyHash fGenDeleg
           ]

instance Crypto.Crypto crypto => ToJSON (Shelley.GenDelegs crypto) where
  toJSON (Shelley.GenDelegs delegs) = toJSON delegs

instance ToJSON (Shelley.InstantaneousRewards crypto) where
  toJSON iRwds = object [ "iRReserves" .= Shelley.iRReserves iRwds
                        , "iRTreasury" .= Shelley.iRTreasury iRwds
                        ]

instance ToJSON (Bimap Shelley.Ptr (Shelley.Credential Shelley.Staking crypto)) where
  toJSON (MkBiMap ptsStakeM stakePtrSetM) =
    object [ "stakedCreds" .= Map.toList ptsStakeM
           , "credPtrR" .= toJSON stakePtrSetM
           ]
instance ToJSON Shelley.Ptr where
  toJSON (Shelley.Ptr slotNo txIndex certIndex) =
    object [ "slot" .= unSlotNo slotNo
           , "txIndex" .= txIndex
           , "certIndex" .= certIndex
           ]


instance Crypto.Crypto crypto => ToJSON (Shelley.PState crypto) where
  toJSON pState = object [ "pParams pState" .= Shelley._pParams pState
                         , "fPParams pState" .= Shelley._fPParams pState
                         , "retiring pState" .= Shelley._retiring pState
                         ]

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.TxOut era)
         ) => ToJSON (Shelley.UTxO era) where
  toJSON (Shelley.UTxO utxo) = toJSON utxo

instance ( Consensus.ShelleyBasedEra era
         , ToJSON (Core.Value era)
         ) => ToJSON (Shelley.TxOut era) where
  toJSON (Shelley.TxOut addr amount) =
    object
      [ "address" .= addr
      , "amount" .= amount
      ]

instance Crypto.Crypto crypto => ToJSON (Shelley.TxIn crypto) where
  toJSON = toJSON . txInToText

instance Crypto.Crypto crypto => ToJSONKey (Shelley.TxIn crypto) where
  toJSONKey = toJSONKeyText txInToText

txInToText :: Crypto.Crypto crypto => Shelley.TxIn crypto -> Text
txInToText (Shelley.TxIn (Shelley.TxId txidHash) ix) =
  hashToText (SafeHash.extractHash txidHash)
    <> Text.pack "#"
    <> Text.pack (show ix)

hashToText :: Crypto.Hash crypto a -> Text
hashToText = Text.decodeLatin1 . Crypto.hashToBytesAsHex

instance Crypto.Crypto crypto => ToJSON (Shelley.NonMyopic crypto) where
  toJSON nonMy = object [ "likelihoodsNM" .= Shelley.likelihoodsNM nonMy
                        , "rewardPotNM" .= Shelley.rewardPotNM nonMy
                        ]

instance ToJSON Shelley.Likelihood where
  toJSON (Shelley.Likelihood llhd) =
    toJSON $ fmap (\(Shelley.LogWeight f) -> exp $ realToFrac f :: Double) llhd

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShots crypto) where
  toJSON ss = object [ "pstakeMark" .= Shelley._pstakeMark ss
                     , "pstakeSet" .= Shelley._pstakeSet ss
                     , "pstakeGo" .= Shelley._pstakeGo ss
                     , "feeSS" .= Shelley._feeSS ss
                     ]

instance Crypto.Crypto crypto => ToJSON (Shelley.SnapShot crypto) where
  toJSON ss = object [ "stake" .= Shelley._stake ss
                     , "delegations" .= ShelleyEpoch._delegations ss
                     , "poolParams" .= Shelley._poolParams ss
                     ]

instance ToJSON (Shelley.Stake crypto) where
  toJSON (Shelley.Stake s) = toJSON s

instance Crypto.Crypto crypto => ToJSON (Shelley.RewardUpdate crypto) where
  toJSON rUpdate = object [ "deltaT" .= Shelley.deltaT rUpdate
                          , "deltaR" .= Shelley.deltaR rUpdate
                          , "rs" .= Shelley.rs rUpdate
                          , "deltaF" .= Shelley.deltaF rUpdate
                          , "nonMyopic" .= Shelley.nonMyopic rUpdate
                          ]

instance Crypto.Crypto crypto => ToJSON (Shelley.PulsingRewUpdate crypto) where
  toJSON (Shelley.Pulsing _ _) = Aeson.Null
  toJSON (Shelley.Complete ru) = toJSON ru

instance ToJSON Shelley.DeltaCoin where
  toJSON (Shelley.DeltaCoin i) = toJSON i

instance Crypto.Crypto crypto => ToJSON (Shelley.PoolDistr crypto) where
  toJSON (Shelley.PoolDistr m) = toJSON m

instance ToJSON (Shelley.IndividualPoolStake crypto) where
  toJSON indivPoolStake =
    object [ "individualPoolStake" .= Shelley.individualPoolStake indivPoolStake
           , "individualPoolStakeVrf" .= Shelley.individualPoolStakeVrf indivPoolStake
           ]

instance ToJSON (Shelley.Reward crypto) where
  toJSON reward =
     object [ "rewardType" .= Shelley.rewardType reward
            , "rewardPool" .= Shelley.rewardPool reward
            , "rewardAmount" .= Shelley.rewardAmount reward
            ]

instance ToJSON Shelley.RewardType where
  toJSON Shelley.MemberReward = "MemberReward"
  toJSON Shelley.LeaderReward = "LeaderReward"

instance ToJSON (SafeHash.SafeHash c a) where
  toJSON = toJSON . SafeHash.extractHash

-----

instance ToJSON Alonzo.ExUnits
deriving instance FromJSON Alonzo.ExUnits

instance ToJSON Alonzo.Prices where
  toJSON Alonzo.Prices { Alonzo.prSteps, Alonzo.prMem } =
    -- We cannot round-trip via NonNegativeInterval, so we go via Rational
    object [ "prSteps" .= Ledger.unboundRational prSteps
           , "prMem"   .= Ledger.unboundRational prMem
           ]

instance FromJSON Alonzo.Prices where
  parseJSON =
    Aeson.withObject "prices" $ \o -> do
      steps <- o .: "prSteps"
      mem   <- o .: "prMem"
      prSteps <- checkBoundedRational steps
      prMem   <- checkBoundedRational mem
      return Alonzo.Prices { Alonzo.prSteps, Alonzo.prMem }
    where
      -- We cannot round-trip via NonNegativeInterval, so we go via Rational
      checkBoundedRational r =
        case Ledger.boundRational r of
          Nothing -> fail ("too much precision for bounded rational: " ++ show r)
          Just s  -> return s

-- TODO: Alonzo - make explicit instances
deriving newtype instance FromJSON Alonzo.CostModel
deriving newtype instance ToJSON Alonzo.CostModel

instance FromJSON Alonzo.Language where
  parseJSON v = case v of
    Aeson.String "PlutusV1" -> return Alonzo.PlutusV1
    wrong -> fail $ "Error decoding Language. Expected a JSON string but got: " <> show wrong
instance ToJSON Alonzo.Language where
  toJSON Alonzo.PlutusV1 = Aeson.String "PlutusV1"

instance ToJSONKey Alonzo.Language where
  toJSONKey = toJSONKeyText (Text.decodeLatin1 . LBS.toStrict . Aeson.encode)

instance FromJSONKey Alonzo.Language where
  fromJSONKey = Aeson.FromJSONKeyTextParser parseLang
   where
     parseLang :: Text -> Aeson.Parser Alonzo.Language
     parseLang lang =
       case Aeson.eitherDecode $ LBS.fromStrict $ Text.encodeUtf8 lang of
         Left err -> fail (show err)
         Right lang' -> return lang'

-- We defer parsing of the cost model so that we can
-- read it as a filepath. This is to reduce further pollution
-- of the genesis file.
instance FromJSON Alonzo.AlonzoGenesis where
  parseJSON = Aeson.withObject "Alonzo Genesis" $ \o -> do
    coinsPerUTxOWord     <- o .:  "lovelacePerUTxOWord"
                        <|> o .:  "adaPerUTxOWord" --TODO: deprecate
    cModels              <- o .:? "costModels"
    prices               <- o .:  "executionPrices"
    maxTxExUnits         <- o .:  "maxTxExUnits"
    maxBlockExUnits      <- o .:  "maxBlockExUnits"
    maxValSize           <- o .:  "maxValueSize"
    collateralPercentage <- o .:  "collateralPercentage"
    maxCollateralInputs  <- o .:  "maxCollateralInputs"
    case cModels of
      Nothing -> case Alonzo.defaultCostModel of
        Just m -> return Alonzo.AlonzoGenesis
          { Alonzo.coinsPerUTxOWord
          , Alonzo.costmdls = Map.singleton Alonzo.PlutusV1 m
          , Alonzo.prices
          , Alonzo.maxTxExUnits
          , Alonzo.maxBlockExUnits
          , Alonzo.maxValSize
          , Alonzo.collateralPercentage
          , Alonzo.maxCollateralInputs
          }
        Nothing -> fail "Failed to extract the cost model params from defaultCostModel"
      Just costmdls -> return Alonzo.AlonzoGenesis
        { Alonzo.coinsPerUTxOWord
        , Alonzo.costmdls
        , Alonzo.prices
        , Alonzo.maxTxExUnits
        , Alonzo.maxBlockExUnits
        , Alonzo.maxValSize
        , Alonzo.collateralPercentage
        , Alonzo.maxCollateralInputs
        }

-- We don't render the cost model so that we can
-- render it later in 'AlonzoGenWrapper' as a filepath
-- and keep the cost model (which is chunky) as a separate file.
instance ToJSON Alonzo.AlonzoGenesis where
  toJSON v = object
      [ "lovelacePerUTxOWord" .= Alonzo.coinsPerUTxOWord v
      , "costModels" .= Alonzo.costmdls v
      , "executionPrices" .= Alonzo.prices v
      , "maxTxExUnits" .= Alonzo.maxTxExUnits v
      , "maxBlockExUnits" .= Alonzo.maxBlockExUnits v
      , "maxValueSize" .= Alonzo.maxValSize v
      , "collateralPercentage" .= Alonzo.collateralPercentage v
      , "maxCollateralInputs" .= Alonzo.maxCollateralInputs v
      ]

instance ToJSON (Alonzo.PParams era) where
  toJSON pp =
    Aeson.object
      [ "minFeeA" .= Alonzo._minfeeA pp
      , "minFeeB" .= Alonzo._minfeeB pp
      , "maxBlockBodySize" .= Alonzo._maxBBSize pp
      , "maxTxSize" .= Alonzo._maxTxSize pp
      , "maxBlockHeaderSize" .= Alonzo._maxBHSize pp
      , "keyDeposit" .= Alonzo._keyDeposit pp
      , "poolDeposit" .= Alonzo._poolDeposit pp
      , "eMax" .= Alonzo._eMax pp
      , "nOpt" .= Alonzo._nOpt pp
      , "a0"  .= Alonzo._a0 pp
      , "rho" .= Alonzo._rho pp
      , "tau" .= Alonzo._tau pp
      , "decentralisationParam" .= Alonzo._d pp
      , "extraEntropy" .= Alonzo._extraEntropy pp
      , "protocolVersion" .= Alonzo._protocolVersion pp
      , "minPoolCost" .= Alonzo._minPoolCost pp
      , "lovelacePerUTxOWord" .= Alonzo._coinsPerUTxOWord pp
      , "costmdls" .= Alonzo._costmdls pp
      , "prices" .= Alonzo._prices pp
      , "maxTxExUnits" .= Alonzo._maxTxExUnits pp
      , "maxBlockExUnits" .= Alonzo._maxBlockExUnits pp
      , "maxValSize" .= Alonzo._maxValSize pp
      , "collateralPercentage" .= Alonzo._collateralPercentage pp
      , "maxCollateralInputs " .= Alonzo._maxCollateralInputs pp
      ]

instance FromJSON (Alonzo.PParams era) where
  parseJSON =
    Aeson.withObject "PParams" $ \obj ->
      Alonzo.PParams
        <$> obj .: "minFeeA"
        <*> obj .: "minFeeB"
        <*> obj .: "maxBlockBodySize"
        <*> obj .: "maxTxSize"
        <*> obj .: "maxBlockHeaderSize"
        <*> obj .: "keyDeposit"
        <*> obj .: "poolDeposit"
        <*> obj .: "eMax"
        <*> obj .: "nOpt"
        <*> obj .: "a0"
        <*> obj .: "rho"
        <*> obj .: "tau"
        <*> obj .: "decentralisationParam"
        <*> obj .: "extraEntropy"
        <*> obj .: "protocolVersion"
        <*> obj .: "minPoolCost" .!= mempty
        <*> obj .: "lovelacePerUTxOWord"
        <*> obj .: "costmdls"
        <*> obj .: "prices"
        <*> obj .: "maxTxExUnits"
        <*> obj .: "maxBlockExUnits"
        <*> obj .: "maxValSize"
        <*> obj .: "collateralPercentage"
        <*> obj .: "maxCollateralInputs"

deriving instance ToJSON (Alonzo.PParamsUpdate (Alonzo.AlonzoEra StandardCrypto))

instance (Ledger.Era era, Show (Ledger.Value era), ToJSON (Ledger.Value era))
    => ToJSON (Alonzo.TxOut era) where
  toJSON (Alonzo.TxOut addr v dataHash) =
    object [ "address" .= toJSON addr
           , "value" .= toJSON v
           , "datahash" .= case strictMaybeToMaybe dataHash of
                             Nothing -> Aeson.Null
                             Just dHash ->
                               Aeson.String . Crypto.hashToTextAsHex
                                 $ SafeHash.extractHash dHash
           ]

deriving instance Show Alonzo.AlonzoGenesis
