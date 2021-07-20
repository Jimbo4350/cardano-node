{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.ChainDB.Formatting
  (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.Aeson as A
import           Data.HashMap.Strict (insertWith)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Text.Show

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Render

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.HeaderValidation (HeaderEnvelopeError (..),
                     HeaderError (..), OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerEvent (..))
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import qualified Ouroboros.Consensus.Protocol.PBFT as PBFT
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import           Ouroboros.Consensus.Util.Condense (condense)

import qualified Ouroboros.Network.AnchoredFragment as AF


addedHdrsNewChain :: HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk)
  -> AF.AnchoredFragment (Header blk)
  -> [Header blk]
addedHdrsNewChain fro to_ =
 case AF.intersect fro to_ of
   Just (_, _, _, s2 :: AF.AnchoredFragment (Header blk)) ->
     AF.toOldestFirst s2
   Nothing -> [] -- No sense to do validation here.

kindContext :: Text -> A.Object -> A.Object
kindContext toAdd = insertWith f "kind" (String toAdd)
  where
    f (String new) (String old) = String (new <> "." <> old)
    f (String new) _            = String new
    f _ o                       = o


instance ( StandardHash blk
         , LogFormatting (ValidationErr (BlockProtocol blk))
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderError blk) where
  forMachine dtal (HeaderProtocolError err) =
    mkObject
      [ "kind" .= String "HeaderProtocolError"
      , "error" .= forMachine dtal err
      ]
  forMachine dtal (HeaderEnvelopeError err) =
    mkObject
      [ "kind" .= String "HeaderEnvelopeError"
      , "error" .= forMachine dtal err
      ]

instance ( StandardHash blk
         , LogFormatting (OtherHeaderEnvelopeError blk)
         )
      => LogFormatting (HeaderEnvelopeError blk) where
  forMachine _dtal (UnexpectedBlockNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedBlockNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedSlotNo expect act) =
    mkObject
      [ "kind" .= String "UnexpectedSlotNo"
      , "expected" .= condense expect
      , "actual" .= condense act
      ]
  forMachine _dtal (UnexpectedPrevHash expect act) =
    mkObject
      [ "kind" .= String "UnexpectedPrevHash"
      , "expected" .= String (Text.pack $ show expect)
      , "actual" .= String (Text.pack $ show act)
      ]
  forMachine dtal (OtherHeaderEnvelopeError err) =
    forMachine dtal err


instance (   LogFormatting (LedgerError blk)
           , LogFormatting (HeaderError blk))
        => LogFormatting (ExtValidationError blk) where
    forMachine dtal (ExtValidationErrorLedger err) = forMachine dtal err
    forMachine dtal (ExtValidationErrorHeader err) = forMachine dtal err

    forHuman (ExtValidationErrorLedger err) =  forHuman err
    forHuman (ExtValidationErrorHeader err) =  forHuman err

    asMetrics (ExtValidationErrorLedger err) =  asMetrics err
    asMetrics (ExtValidationErrorHeader err) =  asMetrics err

instance LogFormatting LedgerDB.DiskSnapshot where
  forMachine DDetailed snap =
    mkObject [ "kind" .= String "snapshot"
             , "snapshot" .= String (Text.pack $ show snap) ]
  forMachine _ _snap = mkObject [ "kind" .= String "snapshot" ]

instance (  LogFormatting (Header blk)
          , LogFormatting (LedgerEvent blk)
          , LogFormatting (RealPoint blk)
          , ConvertRawHash blk
          , ConvertRawHash (Header blk)
          , HasHeader (Header blk)
          , LedgerSupportsProtocol blk
          , InspectLedger blk
          ) => LogFormatting (ChainDB.TraceEvent blk) where
  forHuman (ChainDB.TraceAddBlockEvent v)          = forHuman v
  forHuman (ChainDB.TraceFollowerEvent v)          = forHuman v
  forHuman (ChainDB.TraceCopyToImmutableDBEvent v) = forHuman v
  forHuman (ChainDB.TraceGCEvent v)                = forHuman v
  forHuman (ChainDB.TraceInitChainSelEvent v)      = forHuman v
  forHuman (ChainDB.TraceOpenEvent v)              = forHuman v
  forHuman (ChainDB.TraceIteratorEvent v)          = forHuman v
  forHuman (ChainDB.TraceLedgerEvent v)            = forHuman v
  forHuman (ChainDB.TraceLedgerReplayEvent v)      = forHuman v
  forHuman (ChainDB.TraceImmutableDBEvent v)       = forHuman v
  forHuman (ChainDB.TraceVolatileDBEvent v)        = forHuman v

  forMachine details (ChainDB.TraceAddBlockEvent v) =
    kindContext "AddBlockEvent" $ forMachine details v
  forMachine details (ChainDB.TraceFollowerEvent v) =
    kindContext "FollowerEvent" $ forMachine details v
  forMachine details (ChainDB.TraceCopyToImmutableDBEvent v) =
    kindContext "CopyToImmutableDBEvent" $ forMachine details v
  forMachine details (ChainDB.TraceGCEvent v) =
    kindContext "TraceGCEvent" $ forMachine details v
  forMachine details (ChainDB.TraceInitChainSelEvent v) =
    kindContext "InitChainSelEvent" $ forMachine details v
  forMachine details (ChainDB.TraceOpenEvent v) =
    kindContext "OpenEvent" $ forMachine details v
  forMachine details (ChainDB.TraceIteratorEvent v) =
    kindContext "IteratorEvent" $ forMachine details v
  forMachine details (ChainDB.TraceLedgerEvent v) =
    kindContext "LedgerEvent" $ forMachine details v
  forMachine details (ChainDB.TraceLedgerReplayEvent v) =
    kindContext "LedgerReplayEvent" $ forMachine details v
  forMachine details (ChainDB.TraceImmutableDBEvent v) =
    kindContext "ImmutableDBEvent" $ forMachine details v
  forMachine details (ChainDB.TraceVolatileDBEvent v) =
    kindContext "VolatileDBEvent" $ forMachine details v

  asMetrics (ChainDB.TraceAddBlockEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceFollowerEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceCopyToImmutableDBEvent v) = asMetrics v
  asMetrics (ChainDB.TraceGCEvent v)                = asMetrics v
  asMetrics (ChainDB.TraceInitChainSelEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceOpenEvent v)              = asMetrics v
  asMetrics (ChainDB.TraceIteratorEvent v)          = asMetrics v
  asMetrics (ChainDB.TraceLedgerEvent v)            = asMetrics v
  asMetrics (ChainDB.TraceLedgerReplayEvent v)      = asMetrics v
  asMetrics (ChainDB.TraceImmutableDBEvent v)       = asMetrics v
  asMetrics (ChainDB.TraceVolatileDBEvent v)        = asMetrics v

instance ( LogFormatting (Header blk)
         , LogFormatting (LedgerEvent blk)
         , LogFormatting (RealPoint blk)
         , ConvertRawHash blk
         , ConvertRawHash (Header blk)
         , HasHeader (Header blk)
         , LedgerSupportsProtocol blk
         , InspectLedger blk
         ) => LogFormatting (ChainDB.TraceAddBlockEvent blk) where
  forHuman (ChainDB.IgnoreBlockOlderThanK pt) =
    "Ignoring block older than K: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      "Ignoring block already in DB: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.IgnoreInvalidBlock pt _reason) =
      "Ignoring previously seen invalid block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.AddedBlockToQueue pt sz) =
      "Block added to queue: " <> renderRealPointAsPhrase pt <> " queue size " <> condenseT sz
  forHuman (ChainDB.BlockInTheFuture pt slot) =
      "Ignoring block from future: " <> renderRealPointAsPhrase pt <> ", slot " <> condenseT slot
  forHuman (ChainDB.StoreButDontChange pt) =
      "Ignoring block: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TryAddToCurrentChain pt) =
      "Block fits onto the current chain: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.TrySwitchToAFork pt _) =
      "Block fits onto some fork: " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.AddedToCurrentChain es _ _ c) =
      "Chain extended, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.SwitchedToAFork es _ _ c) =
      "Switched to a fork, new tip: " <> renderPointAsPhrase (AF.headPoint c) <>
        Text.concat [ "\nEvent: " <> showT e | e <- es ]
  forHuman (ChainDB.AddBlockValidation ev') = forHuman ev'
  forHuman (ChainDB.AddedBlockToVolatileDB pt _ _) =
      "Chain added block " <> renderRealPointAsPhrase pt
  forHuman (ChainDB.ChainSelectionForFutureBlock pt) =
      "Chain selection run for block previously from future: " <> renderRealPointAsPhrase pt

  forMachine dtal (ChainDB.IgnoreBlockOlderThanK pt) =
      mkObject [ "kind" .= String "IgnoreBlockOlderThanK"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreBlockAlreadyInVolatileDB pt) =
      mkObject [ "kind" .= String "IgnoreBlockAlreadyInVolatileDB"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.IgnoreInvalidBlock pt reason) =
      mkObject [ "kind" .= String "IgnoreInvalidBlock"
               , "block" .= forMachine dtal pt
               , "reason" .= showT reason ]
  forMachine dtal (ChainDB.AddedBlockToQueue pt sz) =
      mkObject [ "kind" .= String "AddedBlockToQueue"
               , "block" .= forMachine dtal pt
               , "queueSize" .= toJSON sz ]
  forMachine dtal (ChainDB.BlockInTheFuture pt slot) =
      mkObject [ "kind" .= String "BlockInTheFuture"
               , "block" .= forMachine dtal pt
               , "slot" .= forMachine dtal slot ]
  forMachine dtal (ChainDB.StoreButDontChange pt) =
      mkObject [ "kind" .= String "StoreButDontChange"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TryAddToCurrentChain pt) =
      mkObject [ "kind" .= String "TryAddToCurrentChain"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.TrySwitchToAFork pt _) =
      mkObject [ "kind" .= String "TraceAddBlockEvent.TrySwitchToAFork"
               , "block" .= forMachine dtal pt ]
  forMachine dtal (ChainDB.AddedToCurrentChain events _ base extended) =
      mkObject $
               [ "kind" .=  String "AddedToCurrentChain"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint extended)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain base extended)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.SwitchedToAFork events _ old new) =
      mkObject $
               [ "kind" .= String "TraceAddBlockEvent.SwitchedToAFork"
               , "newtip" .= renderPointForDetails dtal (AF.headPoint new)
               ]
            ++ [ "headers" .= toJSON (forMachine dtal `map` addedHdrsNewChain old new)
               | dtal == DDetailed ]
            ++ [ "events" .= toJSON (map (forMachine dtal) events)
               | not (null events) ]
  forMachine dtal (ChainDB.AddBlockValidation ev') =
    kindContext "AddBlockEvent" $ forMachine dtal ev'
  forMachine dtal (ChainDB.AddedBlockToVolatileDB pt (BlockNo bn) _) =
      mkObject [ "kind" .= String "AddedBlockToVolatileDB"
               , "block" .= forMachine dtal pt
               , "blockNo" .= showT bn ]
  forMachine dtal (ChainDB.ChainSelectionForFutureBlock pt) =
      mkObject [ "kind" .= String "TChainSelectionForFutureBlock"
               , "block" .= forMachine dtal pt ]

  asMetrics (ChainDB.SwitchedToAFork _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM ["density"] (fromRational density)
        , IntM    ["slots"] (fromIntegral slots)
        , IntM    ["blocks"] (fromIntegral blocks)
        , IntM    ["slotInEpoch"] (fromIntegral slotInEpoch)
        , IntM    ["epoch"] (fromIntegral (unEpochNo epoch))
        ]
  asMetrics (ChainDB.AddedToCurrentChain _warnings newTipInfo _oldChain newChain) =
    let ChainInformation { slots, blocks, density, epoch, slotInEpoch } =
          chainInformation newTipInfo newChain 0
    in  [ DoubleM ["density"] (fromRational density)
        , IntM    ["slotNum"] (fromIntegral slots)
        , IntM    ["blockNum"] (fromIntegral blocks)
        , IntM    ["slotInEpoch"] (fromIntegral slotInEpoch)
        , IntM    ["epoch"] (fromIntegral (unEpochNo epoch))
        ]
  asMetrics _ = []

data ChainInformation = ChainInformation
  { slots                :: Word64
  , blocks               :: Word64
  , density              :: Rational
    -- ^ the actual number of blocks created over the maximum expected number
    -- of blocks that could be created over the span of the last @k@ blocks.
  , epoch                :: EpochNo
    -- ^ In which epoch is the tip of the current chain
  , slotInEpoch          :: Word64
    -- ^ Relative slot number of the tip of the current chain within the
    -- epoch.
  , blocksUncoupledDelta :: Int64
    -- ^ The net change in number of blocks forged since last restart not on the
    -- current chain.
  }

chainInformation
  :: forall blk. HasHeader (Header blk)
  => ChainDB.NewTipInfo blk
  -> AF.AnchoredFragment (Header blk)
  -> Int64
  -> ChainInformation
chainInformation newTipInfo frag blocksUncoupledDelta = ChainInformation
    { slots = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    , blocks = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    , density = fragmentChainDensity frag
    , epoch = ChainDB.newTipEpoch newTipInfo
    , slotInEpoch = ChainDB.newTipSlotInEpoch newTipInfo
    , blocksUncoupledDelta = blocksUncoupledDelta
    }

fragmentChainDensity ::
  HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b


instance ( HasHeader (Header blk)
         , LedgerSupportsProtocol blk
         , ConvertRawHash (Header blk)
         , ConvertRawHash blk
         , LogFormatting (RealPoint blk))
         => LogFormatting (ChainDB.TraceValidationEvent blk) where
  forHuman (ChainDB.InvalidBlock err pt) =
      "Invalid block " <> renderRealPointAsPhrase pt <> ": " <> showT err
  forHuman (ChainDB.InvalidCandidate c) =
      "Invalid candidate " <> renderPointAsPhrase (AF.headPoint c)
  forHuman (ChainDB.ValidCandidate c) =
      "Valid candidate " <> renderPointAsPhrase (AF.headPoint c)
  forHuman (ChainDB.CandidateContainsFutureBlocks c hdrs) =
      "Candidate contains blocks from near future:  " <>
        renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
        Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)
  forHuman (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
      "Candidate contains blocks from future exceeding clock skew limit: " <>
        renderPointAsPhrase (AF.headPoint c) <> ", slots " <>
        Text.intercalate ", " (map (renderPoint . headerPoint) hdrs)

  forMachine dtal  (ChainDB.InvalidBlock err pt) =
          mkObject [ "kind" .= String "InvalidBlock"
                   , "block" .= forMachine dtal pt
                   , "error" .= showT err ]
  forMachine dtal  (ChainDB.InvalidCandidate c) =
          mkObject [ "kind" .= String "InvalidCandidate"
                   , "block" .= renderPointForDetails dtal (AF.headPoint c) ]
  forMachine dtal  (ChainDB.ValidCandidate c) =
          mkObject [ "kind" .= String "ValidCandidate"
                   , "block" .= renderPointForDetails dtal (AF.headPoint c) ]
  forMachine dtal  (ChainDB.CandidateContainsFutureBlocks c hdrs) =
          mkObject [ "kind" .= String "CandidateContainsFutureBlocks"
                   , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                   , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]
  forMachine dtal  (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew c hdrs) =
          mkObject [ "kind" .= String "CandidateContainsFutureBlocksExceedingClockSkew"
                   , "block"   .= renderPointForDetails dtal (AF.headPoint c)
                   , "headers" .= map (renderPointForDetails dtal . headerPoint) hdrs ]



instance (StandardHash blk, ConvertRawHash blk)
          => LogFormatting (LedgerDB.TraceReplayEvent blk (Point blk)) where
  forHuman (LedgerDB.ReplayFromGenesis _replayTo) =
      "Replaying ledger from genesis"
  forHuman (LedgerDB.ReplayFromSnapshot snap tip' _replayTo) =
      "Replaying ledger from snapshot " <> showT snap <> " at " <>
        renderRealPointAsPhrase tip'
  forHuman (LedgerDB.ReplayedBlock pt _ledgerEvents replayTo) =
      "Replayed block: slot " <> showT (realPointSlot pt) <> " of " <> showT (pointSlot replayTo)

  forMachine _dtal (LedgerDB.ReplayFromGenesis _replayTo) =
      mkObject [ "kind" .= String "ReplayFromGenesis" ]
  forMachine dtal (LedgerDB.ReplayFromSnapshot snap tip' _replayTo) =
      mkObject [ "kind" .= String "ReplayFromSnapshot"
               , "snapshot" .= forMachine dtal snap
               , "tip" .= show tip' ]
  forMachine _dtal (LedgerDB.ReplayedBlock pt _ledgerEvents replayTo) =
      mkObject [ "kind" .= String "ReplayedBlock"
               , "slot" .= unSlotNo (realPointSlot pt)
               , "tip"  .= withOrigin 0 unSlotNo (pointSlot replayTo) ]

instance ( StandardHash blk
         , ConvertRawHash blk)
         => LogFormatting (LedgerDB.TraceEvent blk) where
  forHuman (LedgerDB.TookSnapshot snap pt) =
      "Took ledger snapshot " <> showT snap <>
        " at " <> renderRealPointAsPhrase pt
  forHuman (LedgerDB.DeletedSnapshot snap) =
      "Deleted old snapshot " <> showT snap
  forHuman (LedgerDB.InvalidSnapshot snap failure) =
      "Invalid snapshot " <> showT snap <> showT failure

  forMachine dtals (LedgerDB.TookSnapshot snap pt) =
    mkObject [ "kind" .= String "TookSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "tip" .= show pt ]
  forMachine dtals (LedgerDB.DeletedSnapshot snap) =
    mkObject [ "kind" .= String "DeletedSnapshot"
             , "snapshot" .= forMachine dtals snap ]
  forMachine dtals (LedgerDB.InvalidSnapshot snap failure) =
    mkObject [ "kind" .= String "TraceLedgerEvent.InvalidSnapshot"
             , "snapshot" .= forMachine dtals snap
             , "failure" .= show failure ]


instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceCopyToImmutableDBEvent blk) where
  forHuman (ChainDB.CopiedBlockToImmutableDB pt) =
      "Copied block " <> renderPointAsPhrase pt <> " to the ImmDB"
  forHuman ChainDB.NoBlocksToCopyToImmutableDB  =
      "There are no blocks to copy to the ImmDB"

  forMachine dtals (ChainDB.CopiedBlockToImmutableDB pt) =
      mkObject [ "kind" .= String "CopiedBlockToImmutableDB"
               , "slot" .= forMachine dtals pt ]
  forMachine _dtals ChainDB.NoBlocksToCopyToImmutableDB =
      mkObject [ "kind" .= String "NoBlocksToCopyToImmutableDB" ]

instance LogFormatting (ChainDB.TraceGCEvent blk) where
  forHuman (ChainDB.PerformedGC slot) =
      "Performed a garbage collection for " <> condenseT slot
  forHuman (ChainDB.ScheduledGC slot _difft) =
      "Scheduled a garbage collection for " <> condenseT slot

  forMachine dtals (ChainDB.PerformedGC slot) =
      mkObject [ "kind" .= String "PerformedGC"
               , "slot" .= forMachine dtals slot ]
  forMachine dtals (ChainDB.ScheduledGC slot difft) =
      mkObject $ [ "kind" .= String "TraceGCEvent.ScheduledGC"
                 , "slot" .= forMachine dtals slot ] <>
                 [ "difft" .= String ((Text.pack . show) difft) | dtals >= DDetailed]

instance ConvertRawHash blk
          => LogFormatting (ChainDB.TraceOpenEvent blk) where
  forHuman (ChainDB.OpenedDB immTip tip') =
          "Opened db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.ClosedDB immTip tip') =
          "Closed db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and tip " <> renderPointAsPhrase tip'
  forHuman (ChainDB.OpenedImmutableDB immTip chunk) =
          "Opened imm db with immutable tip at " <> renderPointAsPhrase immTip <>
          " and chunk " <> showT chunk
  forHuman ChainDB.OpenedVolatileDB = "Opened vol db"
  forHuman ChainDB.OpenedLgrDB = "Opened lgr db"

  forMachine dtal (ChainDB.OpenedDB immTip tip')=
    mkObject [ "kind" .= String "OpenedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.ClosedDB immTip tip') =
    mkObject [ "kind" .= String "TraceOpenEvent.ClosedDB"
             , "immtip" .= forMachine dtal immTip
             , "tip" .= forMachine dtal tip' ]
  forMachine dtal (ChainDB.OpenedImmutableDB immTip epoch) =
    mkObject [ "kind" .= String "OpenedImmutableDB"
             , "immtip" .= forMachine dtal immTip
             , "epoch" .= String ((Text.pack . show) epoch) ]
  forMachine _dtal ChainDB.OpenedVolatileDB =
      mkObject [ "kind" .= String "OpenedVolatileDB" ]
  forMachine _dtal ChainDB.OpenedLgrDB =
      mkObject [ "kind" .= String "OpenedLgrDB" ]



instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.TraceIteratorEvent blk) where
  forHuman (ChainDB.UnknownRangeRequested ev') = forHuman ev'
  forHuman (ChainDB.BlockMissingFromVolatileDB realPt) =
      "This block is no longer in the VolatileDB because it has been garbage\
         \ collected. It might now be in the ImmDB if it was part of the\
         \ current chain. Block: " <> renderRealPoint realPt
  forHuman (ChainDB.StreamFromImmutableDB sFrom sTo) =
      "Stream only from the ImmDB. StreamFrom:" <> showT sFrom <>
        " StreamTo: " <> showT sTo
  forHuman (ChainDB.StreamFromBoth sFrom sTo pts) =
      "Stream from both the VolatileDB and the ImmDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.StreamFromVolatileDB sFrom sTo pts) =
      "Stream only from the VolatileDB."
        <> " StreamFrom: " <> showT sFrom <> " StreamTo: " <> showT sTo
        <> " Points: " <> showT (map renderRealPoint pts)
  forHuman (ChainDB.BlockWasCopiedToImmutableDB pt) =
      "This block has been garbage collected from the VolatileDB is now\
        \ found and streamed from the ImmDB. Block: " <> renderRealPoint pt
  forHuman (ChainDB.BlockGCedFromVolatileDB pt) =
      "This block no longer in the VolatileDB and isn't in the ImmDB\
        \ either; it wasn't part of the current chain. Block: " <> renderRealPoint pt
  forHuman ChainDB.SwitchBackToVolatileDB = "SwitchBackToVolatileDB"

  forMachine _dtal (ChainDB.UnknownRangeRequested unkRange) =
    mkObject [ "kind" .= String "UnknownRangeRequested"
             , "range" .= String (showT unkRange)
             ]
  forMachine _dtal (ChainDB.StreamFromVolatileDB streamFrom streamTo realPt) =
    mkObject [ "kind" .= String "StreamFromVolatileDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.StreamFromImmutableDB streamFrom streamTo) =
    mkObject [ "kind" .= String "StreamFromImmutableDB"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             ]
  forMachine _dtal (ChainDB.StreamFromBoth streamFrom streamTo realPt) =
    mkObject [ "kind" .= String "StreamFromBoth"
             , "from" .= String (showT streamFrom)
             , "to" .= String (showT streamTo)
             , "point" .= String (Text.pack . show $ map renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockMissingFromVolatileDB realPt) =
    mkObject [ "kind" .= String "BlockMissingFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockWasCopiedToImmutableDB realPt) =
    mkObject [ "kind" .= String "BlockWasCopiedToImmutableDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.BlockGCedFromVolatileDB realPt) =
    mkObject [ "kind" .= String "BlockGCedFromVolatileDB"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal ChainDB.SwitchBackToVolatileDB =
    mkObject ["kind" .= String "SwitchBackToVolatileDB"
             ]

instance  ( StandardHash blk
          , ConvertRawHash blk
          ) => LogFormatting (ChainDB.UnknownRange blk) where
  forHuman (ChainDB.MissingBlock realPt) =
      "The block at the given point was not found in the ChainDB."
        <> renderRealPoint realPt
  forHuman (ChainDB.ForkTooOld streamFrom) =
      "The requested range forks off too far in the past"
        <> showT streamFrom

  forMachine _dtal (ChainDB.MissingBlock realPt) =
    mkObject [ "kind"  .= String "MissingBlock"
             , "point" .= String (renderRealPoint realPt)
             ]
  forMachine _dtal (ChainDB.ForkTooOld streamFrom) =
    mkObject [ "kind" .= String "ForkTooOld"
             , "from" .= String (showT streamFrom)
             ]

instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftValidationErr c) where
  forMachine _dtal (PBFT.PBftInvalidSignature text) =
    mkObject
      [ "kind" .= String "PBftInvalidSignature"
      , "error" .= String text
      ]
  forMachine _dtal (PBFT.PBftNotGenesisDelegate vkhash _ledgerView) =
    mkObject
      [ "kind" .= String "PBftNotGenesisDelegate"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftExceededSignThreshold vkhash numForged) =
    mkObject
      [ "kind" .= String "PBftExceededSignThreshold"
      , "vk" .= String (Text.pack $ show vkhash)
      , "numForged" .= String (Text.pack (show numForged))
      ]
  forMachine _dtal PBFT.PBftInvalidSlot =
    mkObject
      [ "kind" .= String "PBftInvalidSlot"
      ]

instance (Show (PBFT.PBftVerKeyHash c))
      => LogFormatting (PBFT.PBftCannotForge c) where
  forMachine _dtal (PBFT.PBftCannotForgeInvalidDelegation vkhash) =
    mkObject
      [ "kind" .= String "PBftCannotForgeInvalidDelegation"
      , "vk" .= String (Text.pack $ show vkhash)
      ]
  forMachine _dtal (PBFT.PBftCannotForgeThresholdExceeded numForged) =
    mkObject
      [ "kind" .= String "PBftCannotForgeThresholdExceeded"
      , "numForged" .= numForged
      ]

instance (ConvertRawHash blk, LedgerSupportsProtocol blk)
  => LogFormatting (ChainDB.TraceInitChainSelEvent blk) where
  forHuman (ChainDB.InitChainSelValidation v) = forHuman v

  forMachine dtal (ChainDB.InitChainSelValidation v) =
    kindContext "InitChainSelValidation" $ forMachine dtal v

  asMetrics (ChainDB.InitChainSelValidation v) = asMetrics v

instance (ConvertRawHash blk, StandardHash blk) => LogFormatting (ChainDB.TraceFollowerEvent blk) where
  forHuman ChainDB.NewFollower = "A new Follower was created"
  forHuman (ChainDB.FollowerNoLongerInMem _rrs) =
    "The follower was in the 'FollowerInMem' state but its point is no longer on\
    \ the in-memory chain fragment, so it has to switch to the\
    \ 'FollowerInImmutableDB' state"
  forHuman (ChainDB.FollowerSwitchToMem point slot) =
    "The follower was in the 'FollowerInImmutableDB' state and is switched to\
    \ the 'FollowerInMem' state. Point: " <> showT point <> " slot: " <> showT slot
  forHuman (ChainDB.FollowerNewImmIterator point slot) =
    "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
    \ exhausted while the ImmDB has grown, so we open a new iterator to\
    \ stream these blocks too. Point: " <> showT point <> " slot: " <> showT slot

  forMachine _dtal ChainDB.NewFollower =
      mkObject [ "kind" .= String "NewFollower" ]
  forMachine _dtal (ChainDB.FollowerNoLongerInMem _) =
      mkObject [ "kind" .= String "FollowerNoLongerInMem" ]
  forMachine _dtal (ChainDB.FollowerSwitchToMem _ _) =
      mkObject [ "kind" .= String "FollowerSwitchToMem" ]
  forMachine _dtal (ChainDB.FollowerNewImmIterator _ _) =
      mkObject [ "kind" .= String "FollowerNewImmIterator" ]

instance (ConvertRawHash blk, StandardHash blk)
  => LogFormatting (ImmDB.TraceEvent blk) where
    forMachine _dtal ImmDB.NoValidLastLocation =
      mkObject [ "kind" .= String "NoValidLastLocation" ]
    forMachine _dtal (ImmDB.ValidatedLastLocation chunkNo immTip) =
      mkObject [ "kind" .= String "ValidatedLastLocation"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "immTip" .= String (renderTipHash immTip)
               , "blockNo" .= String (renderTipBlockNo immTip)
               ]
    forMachine _dtal (ImmDB.ValidatingChunk chunkNo) =
      mkObject [ "kind" .= String "ValidatingChunk"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.MissingChunkFile chunkNo) =
      mkObject [ "kind" .= String "MissingChunkFile"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrRead readIncErr)) =
      mkObject [ "kind" .= String "ChunkErrRead"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "error" .= String (showT readIncErr)
               ]
    forMachine _dtal (ImmDB.InvalidChunkFile chunkNo
                        (ImmDB.ChunkErrHashMismatch hashPrevBlock prevHashOfBlock)) =
      mkObject [ "kind" .= String "ChunkErrHashMismatch"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "hashPrevBlock" .= String (Text.decodeLatin1
                                    . toRawHash (Proxy @blk) $ hashPrevBlock)
               , "prevHashOfBlock" .= String (renderChainHash (Text.decodeLatin1
                                    . toRawHash (Proxy @blk)) prevHashOfBlock)
               ]
    forMachine dtal (ImmDB.InvalidChunkFile chunkNo (ImmDB.ChunkErrCorrupt pt)) =
      mkObject [ "kind" .= String "ChunkErrCorrupt"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               , "block" .= String (renderPointForDetails dtal pt)
               ]
    forMachine _dtal (ImmDB.ChunkFileDoesntFit expectPrevHash actualPrevHash) =
      mkObject [ "kind" .= String "ChunkFileDoesntFit"
               , "expectedPrevHash" .= String (renderChainHash (Text.decodeLatin1
                                        . toRawHash (Proxy @blk)) expectPrevHash)
               , "actualPrevHash" .= String (renderChainHash (Text.decodeLatin1
                                        . toRawHash (Proxy @blk)) actualPrevHash)
               ]
    forMachine _dtal (ImmDB.MissingPrimaryIndex chunkNo) =
      mkObject [ "kind" .= String "MissingPrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.MissingSecondaryIndex chunkNo) =
      mkObject [ "kind" .= String "MissingSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.InvalidPrimaryIndex chunkNo) =
      mkObject [ "kind" .= String "InvalidPrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.InvalidSecondaryIndex chunkNo) =
      mkObject [ "kind" .= String "InvalidSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.RewritePrimaryIndex chunkNo) =
      mkObject [ "kind" .= String "RewritePrimaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.RewriteSecondaryIndex chunkNo) =
      mkObject [ "kind" .= String "RewriteSecondaryIndex"
               , "chunkNo" .= String (renderChunkNo chunkNo)
               ]
    forMachine _dtal (ImmDB.Migrating txt) =
      mkObject [ "kind" .= String "Migrating"
               , "info" .= String txt
               ]
    forMachine _dtal (ImmDB.DeletingAfter immTipWithInfo) =
      mkObject [ "kind" .= String "DeletingAfter"
               , "immTipHash" .= String (renderWithOrigin renderTipHash immTipWithInfo)
               , "immTipBlockNo" .= String (renderWithOrigin renderTipBlockNo immTipWithInfo)
               ]
    forMachine _dtal ImmDB.DBAlreadyClosed =
      mkObject [ "kind" .= String "DBAlreadyClosed" ]
    forMachine _dtal ImmDB.DBClosed =
      mkObject [ "kind" .= String "DBClosed" ]
    forMachine dtal (ImmDB.TraceCacheEvent cacheEv) =
      kindContext "TraceCacheEvent" $ forMachine dtal cacheEv


instance LogFormatting ImmDB.TraceCacheEvent where
    forMachine _dtal (ImmDB.TraceCurrentChunkHit chunkNo nbPastChunksInCache) =
          mkObject [ "kind" .= String "TraceCurrentChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkHit chunkNo nbPastChunksInCache) =
          mkObject [ "kind" .= String "TracePastChunkHit"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkMiss chunkNo nbPastChunksInCache) =
          mkObject [ "kind" .= String "TracePastChunkMiss"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunkEvict chunkNo nbPastChunksInCache) =
          mkObject [ "kind" .= String "TracePastChunkEvict"
                   , "chunkNo" .= String (renderChunkNo chunkNo)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]
    forMachine _dtal (ImmDB.TracePastChunksExpired chunkNos nbPastChunksInCache) =
          mkObject [ "kind" .= String "TracePastChunksExpired"
                   , "chunkNos" .= String (Text.pack . show $ map renderChunkNo chunkNos)
                   , "noPastChunks" .= String (showT nbPastChunksInCache)
                   ]

instance StandardHash blk => LogFormatting (VolDB.TraceEvent blk) where
    forMachine _dtal VolDB.DBAlreadyClosed =
      mkObject [ "kind" .= String "DBAlreadyClosed"]
    forMachine _dtal VolDB.DBAlreadyOpen =
      mkObject [ "kind" .= String "DBAlreadyOpen"]
    forMachine _dtal (VolDB.BlockAlreadyHere blockId) =
      mkObject [ "kind" .= String "BlockAlreadyHere"
               , "blockId" .= String (showT blockId)
               ]
    forMachine _dtal (VolDB.TruncateCurrentFile fsPath) =
      mkObject [ "kind" .= String "TruncateCurrentFile"
               , "file" .= String (showT fsPath)
               ]
    forMachine _dtal (VolDB.Truncate pErr fsPath blockOffset) =
      mkObject [ "kind" .= String "Truncate"
               , "parserError" .= String (showT pErr)
               , "file" .= String (showT fsPath)
               , "blockOffset" .= String (showT blockOffset)
               ]
    forMachine _dtal (VolDB.InvalidFileNames fsPaths) =
      mkObject [ "kind" .= String "InvalidFileNames"
               , "files" .= String (Text.pack . show $ map show fsPaths)
               ]

instance ( ConvertRawHash blk
         , StandardHash blk
         , LogFormatting (LedgerError blk)
         , LogFormatting (RealPoint blk)
         , LogFormatting (OtherHeaderEnvelopeError blk)
         , LogFormatting (ExtValidationError blk)
         , LogFormatting (ValidationErr (BlockProtocol blk))
         )
      => LogFormatting (ChainDB.InvalidBlockReason blk) where
  forMachine dtal (ChainDB.ValidationError extvalerr) =
    mkObject
      [ "kind" .= String "ValidationError"
      , "error" .= forMachine dtal extvalerr
      ]
  forMachine dtal (ChainDB.InFutureExceedsClockSkew point) =
    mkObject
      [ "kind" .= String "InFutureExceedsClockSkew"
      , "point" .= forMachine dtal point
      ]
