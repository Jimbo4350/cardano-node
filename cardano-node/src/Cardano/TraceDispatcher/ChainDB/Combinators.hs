
module Cardano.TraceDispatcher.ChainDB.Combinators
  ( severityChainDB
  , namesForChainDBTraceEvents
  ) where

import           Cardano.Logging
import           Cardano.Prelude

import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl as VolDb

severityChainDB :: ChainDB.TraceEvent blk -> SeverityS
severityChainDB (ChainDB.TraceAddBlockEvent v)          = gsTraceAddBlockEvent v
severityChainDB (ChainDB.TraceFollowerEvent v)          = gsTraceFollowerEvent v
severityChainDB (ChainDB.TraceCopyToImmutableDBEvent v) = gsTraceCopyToImmutableDBEvent v
severityChainDB (ChainDB.TraceGCEvent v)                = gsTraceGCEvent v
severityChainDB (ChainDB.TraceInitChainSelEvent v)      = gsTraceInitChainSelEvent v
severityChainDB (ChainDB.TraceOpenEvent v)              = gsTraceOpenEvent v
severityChainDB (ChainDB.TraceIteratorEvent v)          = gsTraceIteratorEvent v
severityChainDB (ChainDB.TraceLedgerEvent v)            = gsTraceLedgerEvent v
severityChainDB (ChainDB.TraceLedgerReplayEvent v)      = gsTraceLedgerReplayEvent v
severityChainDB (ChainDB.TraceImmutableDBEvent v)       = gsTraceImmutableDBEvent v
severityChainDB (ChainDB.TraceVolatileDBEvent v)        = gsTraceVolatileDBEvent v

gsTraceAddBlockEvent :: ChainDB.TraceAddBlockEvent blk -> SeverityS
gsTraceAddBlockEvent ChainDB.IgnoreBlockOlderThanK {} = Info
gsTraceAddBlockEvent ChainDB.IgnoreBlockAlreadyInVolatileDB {} = Info
gsTraceAddBlockEvent ChainDB.IgnoreInvalidBlock {} = Info
gsTraceAddBlockEvent ChainDB.AddedBlockToQueue {} = Debug
gsTraceAddBlockEvent ChainDB.BlockInTheFuture {} = Info
gsTraceAddBlockEvent ChainDB.AddedBlockToVolatileDB {} = Debug
gsTraceAddBlockEvent ChainDB.TryAddToCurrentChain {} = Debug
gsTraceAddBlockEvent ChainDB.TrySwitchToAFork {} = Info
gsTraceAddBlockEvent ChainDB.StoreButDontChange {} = Debug
gsTraceAddBlockEvent (ChainDB.AddedToCurrentChain events _ _ _) =
      maximumDef Notice (map gsLedgerEvent events)
gsTraceAddBlockEvent (ChainDB.SwitchedToAFork events _ _ _) =
      maximumDef Notice (map gsLedgerEvent events)
gsTraceAddBlockEvent (ChainDB.AddBlockValidation ev') = gsTraceValidationEvent ev'
gsTraceAddBlockEvent ChainDB.ChainSelectionForFutureBlock{} = Debug

gsTraceValidationEvent :: ChainDB.TraceValidationEvent blk -> SeverityS
gsTraceValidationEvent ChainDB.InvalidBlock {} = Error
gsTraceValidationEvent ChainDB.InvalidCandidate {} = Error
gsTraceValidationEvent ChainDB.ValidCandidate {} = Info
gsTraceValidationEvent ChainDB.CandidateContainsFutureBlocks {} = Debug
gsTraceValidationEvent ChainDB.CandidateContainsFutureBlocksExceedingClockSkew{} = Error

gsTraceFollowerEvent :: ChainDB.TraceFollowerEvent blk -> SeverityS
gsTraceFollowerEvent ChainDB.NewFollower {}            = Debug
gsTraceFollowerEvent ChainDB.FollowerNoLongerInMem {}  = Debug
gsTraceFollowerEvent ChainDB.FollowerSwitchToMem {}    = Debug
gsTraceFollowerEvent ChainDB.FollowerNewImmIterator {} = Debug

gsLedgerEvent :: LedgerEvent blk -> SeverityS
gsLedgerEvent (LedgerUpdate _)  = Notice
gsLedgerEvent (LedgerWarning _) = Critical

gsTraceCopyToImmutableDBEvent :: ChainDB.TraceCopyToImmutableDBEvent blk -> SeverityS
gsTraceCopyToImmutableDBEvent ChainDB.CopiedBlockToImmutableDB {} = Debug
gsTraceCopyToImmutableDBEvent ChainDB.NoBlocksToCopyToImmutableDB = Debug

gsTraceGCEvent :: ChainDB.TraceGCEvent blk -> SeverityS
gsTraceGCEvent ChainDB.PerformedGC {} = Debug
gsTraceGCEvent ChainDB.ScheduledGC {} = Debug

gsTraceInitChainSelEvent :: ChainDB.TraceInitChainSelEvent blk -> SeverityS
gsTraceInitChainSelEvent ChainDB.InitChainSelValidation {} = Debug

gsTraceOpenEvent :: ChainDB.TraceOpenEvent blk -> SeverityS
gsTraceOpenEvent ChainDB.OpenedDB {}          = Info
gsTraceOpenEvent ChainDB.ClosedDB {}          = Info
gsTraceOpenEvent ChainDB.OpenedImmutableDB {} = Info
gsTraceOpenEvent ChainDB.OpenedVolatileDB     = Info
gsTraceOpenEvent ChainDB.OpenedLgrDB          = Info

gsTraceIteratorEvent :: ChainDB.TraceIteratorEvent blk -> SeverityS
gsTraceIteratorEvent ChainDB.StreamFromVolatileDB {} = Debug
gsTraceIteratorEvent _                               = Debug

gsTraceLedgerEvent :: LedgerDB.TraceEvent blk -> SeverityS
gsTraceLedgerEvent LedgerDB.TookSnapshot {}    = Info
gsTraceLedgerEvent LedgerDB.DeletedSnapshot {} = Debug
gsTraceLedgerEvent LedgerDB.InvalidSnapshot {} = Error

gsTraceLedgerReplayEvent :: LedgerDB.TraceReplayEvent blk replayTo -> SeverityS
gsTraceLedgerReplayEvent LedgerDB.ReplayFromGenesis {}  = Info
gsTraceLedgerReplayEvent LedgerDB.ReplayFromSnapshot {} = Info
gsTraceLedgerReplayEvent LedgerDB.ReplayedBlock {}      = Info

gsTraceImmutableDBEvent :: ImmDB.TraceEvent blk -> SeverityS
gsTraceImmutableDBEvent _ = Debug

gsTraceVolatileDBEvent :: VolDB.TraceEvent blk -> SeverityS
gsTraceVolatileDBEvent _ = Debug

namesForChainDBTraceEvents :: ChainDB.TraceEvent blk -> [Text]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreBlockOlderThanK _)) =
      ["AddBlockEvent","IgnoreBlockOlderThanK"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreBlockAlreadyInVolatileDB _)) =
      ["AddBlockEvent", "IgnoreBlockAlreadyInVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.IgnoreInvalidBlock {})) =
      ["AddBlockEvent", "IgnoreBlockAlreadyInVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedBlockToQueue {})) =
      ["AddBlockEvent", "AddedBlockToQueue"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.BlockInTheFuture {})) =
      ["AddBlockEvent","BlockInTheFuture"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedBlockToVolatileDB {})) =
      ["AddBlockEvent", "AddedBlockToVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.TryAddToCurrentChain {})) =
      ["AddBlockEvent", "TryAddToCurrentChain"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.TrySwitchToAFork {})) =
      ["AddBlockEvent", "TrySwitchToAFork"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.StoreButDontChange {})) =
      ["AddBlockEvent", "StoreButDontChange"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddedToCurrentChain {})) =
      ["AddBlockEvent", "AddedToCurrentChain"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.SwitchedToAFork {})) =
      ["AddBlockEvent", "SwitchedToAFork"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.InvalidBlock {}))) =
      ["AddBlockEvent", "AddBlockValidation", "InvalidBlock"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.InvalidCandidate {}))) =
      ["AddBlockEvent", "AddBlockValidation", "InvalidCandidate"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.ValidCandidate {}))) =
      ["AddBlockEvent", "AddBlockValidation", "ValidCandidate"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation (ChainDB.CandidateContainsFutureBlocks {}))) =
      ["AddBlockEvent", "AddBlockValidation", "CandidateContainsFutureBlocks"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.AddBlockValidation
    (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {}))) =
      ["AddBlockEvent", "AddBlockValidation",
        "CandidateContainsFutureBlocksExceedingClockSkew"]
namesForChainDBTraceEvents (ChainDB.TraceAddBlockEvent
  (ChainDB.ChainSelectionForFutureBlock {})) =
      ["AddBlockEvent", "ChainSelectionForFutureBlock"]
namesForChainDBTraceEvents (ChainDB.TraceFollowerEvent
  ChainDB.NewFollower) =
      ["FollowerEvent", "NewFollower"]
namesForChainDBTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerNoLongerInMem {})) =
      ["FollowerEvent", "FollowerNoLongerInMem"]
namesForChainDBTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerSwitchToMem {})) =
      ["FollowerEvent", "FollowerSwitchToMem"]
namesForChainDBTraceEvents (ChainDB.TraceFollowerEvent
  (ChainDB.FollowerNewImmIterator {})) =
      ["FollowerEvent", "FollowerNewImmIterator"]
namesForChainDBTraceEvents (ChainDB.TraceCopyToImmutableDBEvent
  (ChainDB.CopiedBlockToImmutableDB {})) =
      ["CopyToImmutableDBEvent", "CopiedBlockToImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceCopyToImmutableDBEvent
  (ChainDB.NoBlocksToCopyToImmutableDB)) =
      ["CopyToImmutableDBEvent", "NoBlocksToCopyToImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceGCEvent
  (ChainDB.ScheduledGC {})) =
      ["GCEvent", "NoBlocksToCopyToImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceGCEvent
  (ChainDB.PerformedGC {})) =
      ["GCEvent", "NoBlocksToCopyToImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.InvalidBlock {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "InvalidBlock"]
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.InvalidCandidate {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "InvalidCandidate"]
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.ValidCandidate {}))) =
      ["InitChainSelEvent", "InitChainSelValidation", "ValidCandidate"]
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation (ChainDB.CandidateContainsFutureBlocks {}))) =
      ["InitChainSelEvent", "InitChainSelValidation",
        "CandidateContainsFutureBlocks"]
namesForChainDBTraceEvents (ChainDB.TraceInitChainSelEvent
  (ChainDB.InitChainSelValidation
      (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew {}))) =
      ["InitChainSelEvent", "InitChainSelValidation",
        "CandidateContainsFutureBlocksExceedingClockSkew"]
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.OpenedDB {})) =
      ["OpenEvent", "OpenedDB"]
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.ClosedDB {})) =
      ["OpenEvent", "ClosedDB"]
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent
  (ChainDB.OpenedImmutableDB {})) =
      ["OpenEvent", "OpenedImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent
  ChainDB.OpenedVolatileDB) =
      ["OpenEvent", "OpenedVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceOpenEvent
  ChainDB.OpenedLgrDB) =
      ["OpenEvent", "OpenedLgrDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.UnknownRangeRequested {})) =
      ["IteratorEvent", "UnknownRangeRequested"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromVolatileDB {})) =
      ["IteratorEvent", "StreamFromVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromImmutableDB {})) =
      ["IteratorEvent", "StreamFromImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.StreamFromBoth {})) =
      ["IteratorEvent", "StreamFromBoth"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockMissingFromVolatileDB {})) =
      ["IteratorEvent", "BlockMissingFromVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockWasCopiedToImmutableDB {})) =
      ["IteratorEvent", "BlockWasCopiedToImmutableDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  (ChainDB.BlockGCedFromVolatileDB {})) =
      ["IteratorEvent", "BlockGCedFromVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceIteratorEvent
  ChainDB.SwitchBackToVolatileDB) =
      ["IteratorEvent", "SwitchBackToVolatileDB"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.InvalidSnapshot {})) =
      ["TraceLedgerEvent", "InvalidSnapshot"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.TookSnapshot {})) =
      ["TraceLedgerEvent", "TookSnapshot"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerEvent
  (LedgerDB.DeletedSnapshot {})) =
      ["TraceLedgerEvent", "DeletedSnapshot"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayFromGenesis {})) =
      ["TraceLedgerEvent", "ReplayFromGenesis"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayFromSnapshot {})) =
      ["TraceLedgerEvent", "ReplayFromSnapshot"]
namesForChainDBTraceEvents (ChainDB.TraceLedgerReplayEvent
  (LedgerDB.ReplayedBlock {})) =
      ["TraceLedgerEvent", "ReplayedBlock"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  ImmDB.NoValidLastLocation) =
      ["ImmutableDBEvent", "NoValidLastLocation"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ValidatedLastLocation {})) =
      ["ImmutableDBEvent", "ValidatedLastLocation"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ValidatingChunk {})) =
      ["ImmutableDBEvent", "ValidatingChunk"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingChunkFile {})) =
      ["ImmutableDBEvent", "MissingChunkFile"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidChunkFile {})) =
      ["ImmutableDBEvent", "InvalidChunkFile"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.ChunkFileDoesntFit {})) =
      ["ImmutableDBEvent", "ChunkFileDoesntFit"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingPrimaryIndex {})) =
      ["ImmutableDBEvent", "MissingPrimaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.MissingSecondaryIndex {})) =
      ["ImmutableDBEvent", "MissingSecondaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidPrimaryIndex {})) =
      ["ImmutableDBEvent", "InvalidPrimaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.InvalidSecondaryIndex {})) =
      ["ImmutableDBEvent", "InvalidSecondaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.RewritePrimaryIndex {})) =
      ["ImmutableDBEvent", "RewritePrimaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.RewriteSecondaryIndex {})) =
      ["ImmutableDBEvent", "RewriteSecondaryIndex"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.Migrating {})) =
      ["ImmutableDBEvent", "Migrating"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.DeletingAfter {})) =
      ["ImmutableDBEvent", "DeletingAfter"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  ImmDB.DBAlreadyClosed) =
      ["ImmutableDBEvent", "DBAlreadyClosed"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent ImmDB.DBClosed) =
      ["ImmutableDBEvent", "DBClosed"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TraceCurrentChunkHit {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TraceCurrentChunkHit"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkHit {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkHit"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkMiss {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkMiss"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunkEvict {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkEvict"]
namesForChainDBTraceEvents (ChainDB.TraceImmutableDBEvent
  (ImmDB.TraceCacheEvent (ImmDB.TracePastChunksExpired {}))) =
      ["ImmutableDBEvent", "CacheEvent", "TracePastChunkEvict"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  VolDb.DBAlreadyClosed) =
    ["VolatileDbEvent", "DBAlreadyClosed"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  VolDb.DBAlreadyOpen) =
    ["VolatileDbEvent", "TruncateCurrentFile"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.Truncate {})) =
    ["VolatileDbEvent", "Truncate"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.InvalidFileNames {})) =
    ["VolatileDBEvent", "InvalidFileNames"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.BlockAlreadyHere {})) =
    ["VolatileDBEvent", "BlockAlreadyHere"]
namesForChainDBTraceEvents (ChainDB.TraceVolatileDBEvent
  (VolDb.TruncateCurrentFile {})) =
    ["VolatileDBEvent", "TruncateCurrentFile"]
