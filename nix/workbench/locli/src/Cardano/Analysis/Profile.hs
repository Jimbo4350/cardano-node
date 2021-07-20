{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving#-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
module Cardano.Analysis.Profile (module Cardano.Analysis.Profile) where

import           Prelude (String)
import           Cardano.Prelude

import qualified Data.Aeson.Types as Aeson
import           Data.Aeson (FromJSON(..), Object, withObject, (.:))
import qualified Data.Attoparsec.Text as Atto
import qualified Data.Attoparsec.Time as Iso8601
import           Data.Text (intercalate, pack)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import qualified Data.Time.Clock as Time
import qualified Data.Time.Clock.POSIX as Time
import           Options.Applicative
import qualified Options.Applicative as Opt

import           Ouroboros.Network.Block (SlotNo(..))


data GenesisProfile
  = GenesisProfile
  { active_slots_coeff   :: Float
  , delegators           :: Word64
  , dense_pool_density   :: Word64
  , epoch_length         :: Word64
  , parameter_k          :: Word64
  , max_block_size       :: Word64
  , max_tx_size          :: Word64
  , n_pools              :: Word64
  , slot_duration        :: NominalDiffTime
  , utxo                 :: Word64
  }
  deriving (Show, Generic)

data GeneratorProfile
  = GeneratorProfile
  { add_tx_size              :: Word64
  , inputs_per_tx            :: Word64
  , outputs_per_tx           :: Word64
  , tps                      :: Word64
  , tx_count                 :: Word64
  }
  deriving (Show, Generic)

data Profile
  = Profile
  { genesis          :: GenesisProfile
  , generator        :: GeneratorProfile
  , tag              :: Text
  , profile_name     :: Text
  , genesis_cache_id :: Text
  , era              :: Text
  , date             :: UTCTime
  }
  deriving (Show)

renderChainInfoExport :: ChainInfo -> [Text]
renderChainInfoExport CInfo{..} =
  Data.Text.intercalate "," <$>
  [[ "Profile",    profile_name prof]
  ,[ "Era",        era prof ]
  ,[ "Date",       show $ date prof]
  ,[ "Pools",      show $ n_pools gsis]
  ,[ "Density",    show $ dense_pool_density gsis]
  ,[ "Delegators", show $ delegators gsis]
  ,[ "UTxO",       show $ utxo gsis]
  ]

newtype Genesis
  = Genesis
  { systemStart  :: UTCTime
  }
  deriving (Show, Generic)

data ChainInfo
  = ChainInfo
  { cProfile :: Profile
  , cGenesis :: Genesis
  }
  deriving (Show)

pattern CInfo
  :: GenesisProfile
  -> GeneratorProfile
  -> Profile
  -> UTCTime
  -> ChainInfo
pattern CInfo { gsis, gtor, prof, system_start } <-
  ChainInfo prof@(Profile gsis gtor _ _ _ _ _)
            (Genesis system_start)

instance FromJSON GenesisProfile
instance FromJSON GeneratorProfile
instance FromJSON Genesis
instance FromJSON Profile where
  parseJSON = withObject "Profile" $ \v -> do
    meta :: Object <- v .: "meta"
    profile_content :: Object <- meta .: "profile_content"
    gener :: Object <- profile_content .: "generator"
    Profile
      <$> profile_content .: "genesis"
      <*> profile_content .: "generator"
      <*> meta .: "tag"
      <*> meta .: "profile"
      <*> meta .: "genesis_cache_id"
      <*> gener .: "era"
      <*> ((meta .: "timestamp" :: Aeson.Parser Integer)
           <&> Time.posixSecondsToUTCTime . realToFrac)

newtype SlotStart =
  SlotStart { unSlotStart :: UTCTime }
  deriving (Eq, Aeson.FromJSON, Generic, NFData, Show, Aeson.ToJSON)

slotStart :: ChainInfo -> SlotNo -> SlotStart
slotStart CInfo{..} =
  SlotStart
  . flip Time.addUTCTime system_start
  . (* slot_duration gsis)
  . fromIntegral
  . unSlotNo

sinceSlot :: UTCTime -> SlotStart -> NominalDiffTime
sinceSlot t (SlotStart start) = Time.diffUTCTime t start

-- pChainParams :: Parser ChainParams
-- pChainParams =
--   ChainParams
--     <$> (optUTCTime  "system-start"
--                      "Cluster system start time.")

optUTCTime :: String -> String -> Parser UTCTime
optUTCTime optname desc =
  Opt.option (readerFromAttoParser Iso8601.utcTime)
    $ long optname
    <> metavar "ISO8601-TIME"
    <> help desc

optDuration :: String -> String -> NominalDiffTime -> Parser NominalDiffTime
optDuration optname desc def=
  Opt.option ((realToFrac :: Double -> NominalDiffTime) <$> Opt.auto)
    $ long optname
    <> metavar "SEC"
    <> help desc
    <> value def

optWord :: String -> String -> Word64 -> Parser Word64
optWord optname desc def =
  Opt.option auto
    $ long optname
    <> metavar "INT"
    <> help desc
    <> value def

-- Stolen from: cardano-cli/src/Cardano/CLI/Shelley/Parsers.hs
readerFromAttoParser :: Atto.Parser a -> Opt.ReadM a
readerFromAttoParser p =
    Opt.eitherReader (Atto.parseOnly (p <* Atto.endOfInput) . pack)
