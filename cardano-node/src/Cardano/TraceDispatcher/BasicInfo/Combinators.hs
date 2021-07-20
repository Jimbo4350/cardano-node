{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.BasicInfo.Combinators
  (
    getBasicInfo
  , severityBasicInfo
  , namesForBasicInfo
  , docBasicInfo
  ) where

import           Cardano.Config.Git.Rev (gitRev)
import           Cardano.Logging
import           Cardano.Prelude hiding (trace)
import           Paths_cardano_node (version)

import           Data.Text (pack)
import           Data.Time (getCurrentTime)
import           Data.Version (showVersion)

import           Cardano.Api.Protocol.Types (BlockType (..), protocolInfo)
import qualified Cardano.Chain.Genesis as Gen
import           Cardano.Node.Configuration.POM (NodeConfiguration, ncProtocol)
import           Cardano.Node.Protocol (SomeConsensusProtocol (..))
import           Cardano.Node.Types (protocolName)
import           Cardano.Slotting.Slot (EpochSize (..))
import           Cardano.TraceDispatcher.BasicInfo.Types

import qualified Ouroboros.Consensus.BlockchainTime.WallClock.Types as WCT
import           Ouroboros.Consensus.Byron.Ledger.Conversions
                     (fromByronEpochSlots, fromByronSlotLength,
                     genesisSlotLength)
import           Ouroboros.Consensus.Cardano.Block (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Cardano.CanHardFork
                     (ByronPartialLedgerConfig (..),
                     ShelleyPartialLedgerConfig (..))
import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode
                     (ConfigSupportsNode (..))
import           Ouroboros.Consensus.HardFork.Combinator.Degenerate
                     (HardForkLedgerConfig (..))
import           Ouroboros.Consensus.Node.ProtocolInfo (ProtocolInfo (..))
import           Ouroboros.Consensus.Shelley.Ledger.Ledger
                     (shelleyLedgerGenesis)

import qualified Shelley.Spec.Ledger.API as SL

severityBasicInfo :: BasicInfo -> SeverityS
severityBasicInfo _ = Notice

namesForBasicInfo :: BasicInfo -> [Text]
namesForBasicInfo BICommon {}  = ["Common"]
namesForBasicInfo BIShelley {} = ["ShelleyBased"]
namesForBasicInfo BIByron {}   = ["Byron"]
namesForBasicInfo BINetwork {} = ["Network"]

protoBasicInfoCommon :: BasicInfoCommon
protoBasicInfoCommon = undefined

protoBasicInfoShelley :: BasicInfoShelleyBased
protoBasicInfoShelley = undefined

protoBasicInfoByron :: BasicInfoByron
protoBasicInfoByron = undefined

protoBasicInfoNetwork :: BasicInfoNetwork
protoBasicInfoNetwork = undefined

docBasicInfo :: Documented BasicInfo
docBasicInfo = Documented [
    DocMsg
      (BICommon protoBasicInfoCommon)
      []
      "biConfigPath is the path to the config in use. \
      \\nbiProtocol is the name of the protocol, e.g. \"Byron\", \"Shelley\" \
      \or \"Byron; Shelley\". \
      \\nbiVersion is the version of the node software running. \
      \\nbiCommit is the commit revision of the software running. \
      \\nbiNodeStartTime gives the time this node was started."
  , DocMsg
      (BIShelley protoBasicInfoShelley)
      []
      "bisEra is the current era, e.g. \"Shelley\", \"Allegra\", \"Mary\" \
      \or \"Alonzo\". \
      \\nbisSystemStartTime TODO JNF \
      \\nbisSlotLength gives the length of a slot as time interval. \
      \\nbisEpochLength gives the number of slots which forms an epoch. \
      \\nbisSlotsPerKESPeriod gives the slots per KES period."
  , DocMsg
      (BIByron protoBasicInfoByron)
      []
      "bibSystemStartTime TODO JNF \
      \\nbibSlotLength gives the length of a slot as time interval. \
      \\nbibEpochLength gives the number of slots which forms an epoch."
  , DocMsg
      (BINetwork protoBasicInfoNetwork)
      []
      "niAddresses IPv4 or IPv6 socket ready to accept connections\
      \or diffusion addresses. \
      \\nniDiffusionMode shows if the node runs only initiator or both\
      \initiator or responder node. \
      \\nniDnsProducers shows the list of domain names to subscribe to. \
      \\nniIpProducers shows the list of ip subscription addresses."
  ]

getBasicInfo ::
     NodeConfiguration
  -> SomeConsensusProtocol
  -> FilePath
  -> IO [BasicInfo]
getBasicInfo nc (SomeConsensusProtocol whichP pForInfo) fp = do
  nodeStartTime <- getCurrentTime
  let cfg = pInfoConfig $ protocolInfo pForInfo
      basicInfoCommon = BICommon $ BasicInfoCommon {
                biProtocol = pack . protocolName $ ncProtocol nc
              , biVersion  = pack . showVersion $ version
              , biCommit   = gitRev
              , biNodeStartTime = nodeStartTime
              , biConfigPath = fp
              , biNetworkMagic = getNetworkMagic $ Consensus.configBlock cfg
              }
      protocolDependentItems =
        case whichP of
          ByronBlockType ->
            let DegenLedgerConfig cfgByron = Consensus.configLedger cfg
            in [getGenesisValuesByron cfg cfgByron]
          ShelleyBlockType ->
            let DegenLedgerConfig cfgShelley = Consensus.configLedger cfg
            in [getGenesisValues "Shelley" cfgShelley]
          CardanoBlockType ->
            let CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo = Consensus.configLedger cfg
            in getGenesisValuesByron cfg cfgByron
               : getGenesisValues "Shelley" cfgShelley
               : getGenesisValues "Allegra" cfgAllegra
               : getGenesisValues "Mary"    cfgMary
               : [getGenesisValues "Alonzo"  cfgAlonzo]
  pure (basicInfoCommon : protocolDependentItems)
    where
      getGenesisValues era config =
        let genesis = shelleyLedgerGenesis $ shelleyLedgerConfig config
        in BIShelley $ BasicInfoShelleyBased {
            bisEra               = era
          , bisSystemStartTime   = SL.sgSystemStart genesis
          , bisSlotLength        = WCT.getSlotLength . WCT.mkSlotLength
                                      $ SL.sgSlotLength genesis
          , bisEpochLength       = unEpochSize . SL.sgEpochLength $ genesis
          , bisSlotsPerKESPeriod = SL.sgSlotsPerKESPeriod genesis
        }
      getGenesisValuesByron cfg config =
        let genesis = byronLedgerConfig config
        in BIByron $ BasicInfoByron {
            bibSystemStartTime = WCT.getSystemStart . getSystemStart
                                  $ Consensus.configBlock cfg
          , bibSlotLength      = WCT.getSlotLength . fromByronSlotLength
                                  $ genesisSlotLength genesis
          , bibEpochLength     = unEpochSize . fromByronEpochSlots
                                  $ Gen.configEpochSlots genesis
          }
