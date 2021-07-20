{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Cardano.TraceDispatcher.Consensus.ForgingThreadStats
  ( ForgingStats (..)
  , ForgeThreadStats (..)
  , forgeThreadStats
  , docForgeStats
  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding ((:.:), All, concat)
import           Data.Aeson (Value (..), (.=))

import qualified Data.Map.Strict as Map

import           Cardano.Slotting.Slot (SlotNo (..))
import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck
                     (ForgeTracerType)
import           Cardano.TraceDispatcher.Render (showT)
import           Ouroboros.Consensus.Node.Tracers
import           Ouroboros.Consensus.Shelley.Node ()


-- | Per-forging-thread statistics.
data ForgeThreadStats = ForgeThreadStats
  { ftsNodeCannotForgeNum :: !Int
  , ftsNodeIsLeaderNum    :: !Int
  , ftsBlocksForgedNum    :: !Int
  , ftsSlotsMissedNum     :: !Int
    -- ^ Potentially missed slots.  Note that this is not the same as the number
    -- of missed blocks, since this includes all occurences of not reaching a
    -- leadership check decision, whether or not leadership was possible or not.
    --
    -- Also note that when the aggregate total for this metric is reported in the
    -- multi-pool case, it can be much larger than the actual number of slots
    -- occuring since node start, for it is a sum total for all threads.
  , ftsLastSlot           :: !Int
  }

instance LogFormatting ForgeThreadStats where
  forHuman ForgeThreadStats {..} =
    "Node cannot forge "  <> showT ftsNodeCannotForgeNum
    <> " node is leader " <> showT ftsNodeIsLeaderNum
    <> " blocks forged "  <> showT ftsBlocksForgedNum
    <> " slots missed "   <> showT ftsSlotsMissedNum
    <> " last slot "      <> showT ftsLastSlot
  forMachine _dtal ForgeThreadStats {..} =
    mkObject [ "kind" .= String "ForgeThreadStats"
             , "nodeCannotForgeNum" .= String (show ftsNodeCannotForgeNum)
             , "nodeIsLeaderNum"    .= String (show ftsNodeIsLeaderNum)
             , "blocksForgedNum"    .= String (show ftsBlocksForgedNum)
             , "slotsMissed"        .= String (show ftsSlotsMissedNum)
             , "lastSlot"           .= String (show ftsLastSlot)
             ]
  asMetrics ForgeThreadStats {..} =
    [ IntM ["nodeCannotForgeNum"] (fromIntegral ftsNodeCannotForgeNum)
    , IntM ["nodeIsLeaderNum"]    (fromIntegral ftsNodeIsLeaderNum)
    , IntM ["blocksForgedNum"]    (fromIntegral ftsBlocksForgedNum)
    , IntM ["slotsMissed"]        (fromIntegral ftsSlotsMissedNum)
    , IntM ["lastSlot"]           (fromIntegral ftsLastSlot)
    ]


emptyForgeThreadStats :: ForgeThreadStats
emptyForgeThreadStats = ForgeThreadStats 0 0 0 0 0

-- | This structure stores counters of blockchain-related events,
--   per individual thread in fsStats.
data ForgingStats
  = ForgingStats
  { fsStats              :: !(Map ThreadId ForgeThreadStats)
  , fsNodeCannotForgeNum :: !Int
  , fsNodeIsLeaderNum    :: !Int
  , fsBlocksForgedNum    :: !Int
  , fsSlotsMissedNum     :: !Int
  }

instance LogFormatting ForgingStats where
  forHuman ForgingStats {..} =
    "Node cannot forge "  <> showT fsNodeCannotForgeNum
    <> " node is leader " <> showT fsNodeIsLeaderNum
    <> " blocks forged "  <> showT fsBlocksForgedNum
    <> " slots missed "   <> showT fsSlotsMissedNum
  forMachine _dtal ForgingStats {..} =
    mkObject [ "kind" .= String "ForgingStats"
             , "nodeCannotForgeNum" .= String (show fsNodeCannotForgeNum)
             , "nodeIsLeaderNum"    .= String (show fsNodeIsLeaderNum)
             , "blocksForgedNum"    .= String (show fsBlocksForgedNum)
             , "slotsMissed"        .= String (show fsSlotsMissedNum)
             ]
  asMetrics ForgingStats {..} =
    [ IntM ["nodeCannotForgeNum"] (fromIntegral fsNodeCannotForgeNum)
    , IntM ["nodeIsLeaderNum"]    (fromIntegral fsNodeIsLeaderNum)
    , IntM ["blocksForgedNum"]    (fromIntegral fsBlocksForgedNum)
    , IntM ["slotsMissed"]        (fromIntegral fsSlotsMissedNum)
    ]

emptyForgingStats :: ForgingStats
emptyForgingStats = ForgingStats mempty 0 0 0 0

forgeThreadStats :: Trace IO (Folding (ForgeTracerType blk) ForgingStats)
  -> IO (Trace IO (ForgeTracerType blk))
forgeThreadStats = foldMTraceM calculateThreadStats emptyForgingStats

calculateThreadStats :: MonadIO m
  => ForgingStats
  -> LoggingContext
  -> ForgeTracerType blk
  -> m ForgingStats
calculateThreadStats stats _context
    (Left (TraceLabelCreds _ (TraceNodeCannotForge {}))) = do
      mapThreadStats
        stats
        (\fts -> (fts { ftsNodeCannotForgeNum = ftsNodeCannotForgeNum fts + 1}
                      , Nothing))
        (\fs _ ->  (fs  { fsNodeCannotForgeNum  = fsNodeCannotForgeNum fs + 1 }))
calculateThreadStats stats _context
    (Left (TraceLabelCreds _ (TraceNodeIsLeader (SlotNo slot')))) = do
      let slot = fromIntegral slot'
      mapThreadStats
        stats
        (\fts -> (fts { ftsNodeIsLeaderNum = ftsNodeIsLeaderNum fts + 1
                   , ftsLastSlot = slot}, Nothing))
        (\fs _ ->  (fs  { fsNodeIsLeaderNum  = fsNodeIsLeaderNum fs + 1 }))
calculateThreadStats stats _context
    (Left (TraceLabelCreds _ (TraceForgedBlock {}))) = do
      mapThreadStats
        stats
        (\fts -> (fts { ftsBlocksForgedNum = ftsBlocksForgedNum fts + 1}
                      , Nothing))
        (\fs _ ->  (fs  { fsBlocksForgedNum  = fsBlocksForgedNum fs + 1 }))
calculateThreadStats stats _context
    (Left (TraceLabelCreds _ (TraceNodeNotLeader (SlotNo slot')))) = do
      let slot = fromIntegral slot'
      mapThreadStats
        stats
        (\fts ->
          if ftsLastSlot fts == 0 || succ (ftsLastSlot fts) == slot
            then (fts { ftsLastSlot = slot }, Nothing)
            else
              let missed = (slot - ftsLastSlot fts)
              in (fts { ftsLastSlot = slot
                      , ftsSlotsMissedNum = ftsSlotsMissedNum fts + missed}
                 , Just missed))
        (\fs mbMissed -> case mbMissed of
                            Nothing -> fs
                            Just missed -> (fs { fsSlotsMissedNum =
                              fsSlotsMissedNum fs + missed}))
calculateThreadStats stats _context _message = pure stats

mapThreadStats ::
     MonadIO m
  => ForgingStats
  -> (ForgeThreadStats -> (ForgeThreadStats, Maybe a))
  -> (ForgingStats -> Maybe a -> ForgingStats)
  -> m ForgingStats
mapThreadStats fs@ForgingStats { fsStats } f1 f2 = do
  tid <- liftIO $ myThreadId
  let threadStats   = case Map.lookup tid fsStats of
                        Nothing       -> emptyForgeThreadStats
                        Just vs       -> vs
      (newStats, w) = f1 threadStats
  pure $ f2 (fs {fsStats = Map.insert tid newStats fsStats}) w

docForgeStats :: Documented ForgeThreadStats
docForgeStats = Documented [
    DocMsg
      emptyForgeThreadStats
      [(["nodeCannotForgeNum"],
        "How many times this node could not forge?")
      ,(["nodeIsLeaderNum"],
        "How many times this node was leader?")
      ,(["blocksForgedNum"],
        "How many blocks did forge in this node?")
      ,(["slotsMissed"],
        "How many slots were missed in this node?")
      ]
      "nodeCannotForgeNum shows how many times this node could not forge.\
      \\nnodeIsLeaderNum shows how many times this node was leader.\
      \\nblocksForgedNum shows how many blocks did forge in this node.\
      \\nslotsMissed shows how many slots were missed in this node."
  ]
