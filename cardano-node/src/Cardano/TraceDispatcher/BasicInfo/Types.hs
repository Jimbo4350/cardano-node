{-# LANGUAGE RecordWildCards #-}

module Cardano.TraceDispatcher.BasicInfo.Types
  (
    BasicInfo(..)
  , BasicInfoCommon (..)
  , BasicInfoShelleyBased (..)
  , BasicInfoByron (..)
  , BasicInfoNetwork (..)
  ) where


import           Data.Aeson (Value (String), (.=))
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import           Network.Socket (SockAddr)
import           Data.Text (pack)

import           Cardano.Api (NetworkMagic (..))
import           Cardano.Logging
import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..))
import           Cardano.Prelude hiding (trace)

import           Ouroboros.Consensus.Node (DnsSubscriptionTarget (..),
                     IPSubscriptionTarget (..))
import           Ouroboros.Network.NodeToNode (DiffusionMode (..))


data BasicInfo =
    BICommon BasicInfoCommon
  | BIShelley BasicInfoShelleyBased
  | BIByron BasicInfoByron
  | BINetwork BasicInfoNetwork

data BasicInfoCommon = BasicInfoCommon {
    biConfigPath    :: FilePath
  , biNetworkMagic  :: NetworkMagic
  , biProtocol      :: Text
  , biVersion       :: Text
  , biCommit        :: Text
  , biNodeStartTime :: UTCTime
  }

data BasicInfoShelleyBased = BasicInfoShelleyBased {
    bisEra               :: Text
  , bisSystemStartTime   :: UTCTime
  , bisSlotLength        :: NominalDiffTime
  , bisEpochLength       :: Word64
  , bisSlotsPerKESPeriod :: Word64
}

data BasicInfoByron = BasicInfoByron {
    bibSystemStartTime :: UTCTime
  , bibSlotLength      :: NominalDiffTime
  , bibEpochLength     :: Word64
}

data BasicInfoNetwork = BasicInfoNetwork {
    niAddresses     :: [SocketOrSocketInfo SockAddr SockAddr]
  , niDiffusionMode :: DiffusionMode
  , niDnsProducers  :: [DnsSubscriptionTarget]
  , niIpProducers   :: IPSubscriptionTarget
  }

instance LogFormatting BasicInfo where
  forHuman (BINetwork (BasicInfoNetwork {..})) =
      "Addresses " <> show niAddresses
      <> ", DiffusionMode " <> show niDiffusionMode
      <> ", DnsProducers " <> show niDnsProducers
      <> ", IpProducers " <> show niIpProducers
  forHuman (BIByron (BasicInfoByron {..})) =
      "Era Byron"
      <> ", Slot length " <> show bibSlotLength
      <> ", Epoch length " <> show bibEpochLength
  forHuman (BIShelley (BasicInfoShelleyBased {..})) =
      "Era " <> bisEra
      <> ", Slot length " <> show bisSlotLength
      <> ", Epoch length " <> show bisEpochLength
      <> ", Slots per KESPeriod " <> show bisSlotsPerKESPeriod
  forHuman (BICommon (BasicInfoCommon {..})) =
      "Config path " <> pack biConfigPath
      <> ", Network magic " <> show biNetworkMagic
      <> ", Protocol " <> show biProtocol
      <> ", Version " <> show biVersion
      <> ", Commit " <> show biCommit
      <> ", Node start time " <> show biNodeStartTime

  forMachine _dtal (BINetwork (BasicInfoNetwork {..})) =
      mkObject [ "kind" .= String "BasicInfoNetwork"
               , "addresses" .= String (show niAddresses)
               , "diffusionMode"  .= String (show niDiffusionMode)
               , "dnsProducers" .= String (show niDnsProducers)
               , "ipProducers" .= String (show niIpProducers)
               ]
  forMachine _dtal (BIByron (BasicInfoByron {..})) =
      mkObject [ "kind" .= String "BasicInfoByron"
               , "systemStartTime" .= String (show bibSystemStartTime)
               , "slotLength"  .= String (show bibSlotLength)
               , "epochLength" .= String (show bibEpochLength)
               ]
  forMachine _dtal (BIShelley (BasicInfoShelleyBased {..})) =
      mkObject [ "kind" .= String "BasicInfoShelleyBased"
               , "era"  .= String bisEra
               , "systemStartTime" .= String (show bisSystemStartTime)
               , "slotLength"  .= String (show bisSlotLength)
               , "epochLength" .= String (show bisEpochLength)
               , "slotsPerKESPeriod" .= String (show bisSlotsPerKESPeriod)
               ]
  forMachine _dtal (BICommon (BasicInfoCommon {..})) =
      mkObject [ "kind" .= String "BasicInfoCommon"
               , "configPath" .= String (pack biConfigPath)
               , "networkMagic"  .= String (show biNetworkMagic)
               , "protocol" .= String biProtocol
               , "version" .= String biVersion
               , "commit" .= String biCommit
               , "nodeStartTime" .= biNodeStartTime
               ]
