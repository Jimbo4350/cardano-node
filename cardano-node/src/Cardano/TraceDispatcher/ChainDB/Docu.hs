{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.ChainDB.Docu
  ( docChainDBTraceEvent
  ) where


import           Control.Monad.Class.MonadTime (Time (..))

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)
import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.Fragment.Diff (ChainDiff (..))
import           Ouroboros.Consensus.HeaderValidation
import           Ouroboros.Consensus.Ledger.Extended (ExtValidationError (..))
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import qualified Ouroboros.Consensus.Storage.ChainDB.Impl.Types as ChainDB
import           Ouroboros.Consensus.Storage.FS.API.Types (FsPath, mkFsPath)
import qualified Ouroboros.Consensus.Storage.ImmutableDB as ImmDB
import qualified Ouroboros.Consensus.Storage.ImmutableDB.Impl.Types as ImmDB
import qualified Ouroboros.Consensus.Storage.LedgerDB.OnDisk as LedgerDB
import qualified Ouroboros.Consensus.Storage.VolatileDB as VolDB
import qualified Ouroboros.Consensus.Storage.VolatileDB.Impl.Types as VolDB
import qualified Ouroboros.Network.AnchoredFragment as AF

----------- Prototype objects for docu generation

protoRealPoint :: RealPoint blk
protoRealPoint = undefined

protoPoint :: Point blk
protoPoint = undefined

protoHeaderDiff :: ChainDiff (HeaderFields blk)
protoHeaderDiff = undefined

protoValidationError :: ChainDB.InvalidBlockReason blk
protoValidationError = undefined

protoNTI :: ChainDB.NewTipInfo blk
protoNTI = ChainDB.NewTipInfo protoRealPoint (EpochNo 1) 1 protoRealPoint

protoAFH :: AF.AnchoredFragment (Header blk)
protoAFH =  undefined

protoExtValidationError :: ExtValidationError blk
protoExtValidationError = ExtValidationErrorHeader (HeaderEnvelopeError (UnexpectedSlotNo 1 2))

protoFollowerRollState :: ChainDB.FollowerRollState blk
protoFollowerRollState = undefined

protoWoSlotNo :: WithOrigin SlotNo
protoWoSlotNo = undefined

protoTime :: Time
protoTime = undefined

protoChunkNo :: ImmDB.ChunkNo
protoChunkNo = undefined

protoStreamFrom :: ChainDB.StreamFrom blk
protoStreamFrom = undefined

protoStreamTo :: ChainDB.StreamTo blk
protoStreamTo = undefined

protoUnknownRange :: ChainDB.UnknownRange blk
protoUnknownRange = undefined

protoDiskSnapshot :: LedgerDB.DiskSnapshot
protoDiskSnapshot = undefined

protoInitFailure :: LedgerDB.InitFailure blk
protoInitFailure = undefined

protoTip :: ImmDB.Tip blk
protoTip = undefined

protoChunkFileError :: ImmDB.ChunkFileError blk
protoChunkFileError = undefined

protoChainHash :: ChainHash blk
protoChainHash = undefined

protoWithOriginTip :: WithOrigin (ImmDB.Tip blk)
protoWithOriginTip = undefined

protoParseError :: VolDB.ParseError blk
protoParseError = undefined

protoFsPath :: FsPath
protoFsPath = mkFsPath [""]

protoBlockOffset :: VolDB.BlockOffset
protoBlockOffset = undefined

----------- Documentation

docChainDBTraceEvent :: Documented (ChainDB.TraceEvent blk)
docChainDBTraceEvent = Documented [
    DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreBlockOlderThanK protoRealPoint))
      []
      "A block with a 'BlockNo' more than @k@ back than the current tip\
      \ was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreBlockAlreadyInVolatileDB
          protoRealPoint))
      []
      "A block that is already in the Volatile DB was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.IgnoreInvalidBlock
          protoRealPoint protoValidationError))
      []
      "A block that is already in the Volatile DB was ignored."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedBlockToQueue
          protoRealPoint 1))
      []
      "The block was added to the queue and will be added to the ChainDB by\
      \ the background thread. The size of the queue is included.."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.BlockInTheFuture
          protoRealPoint 1))
      []
      "The block is from the future, i.e., its slot number is greater than\
      \ the current slot (the second argument)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedBlockToVolatileDB
          protoRealPoint 1 ChainDB.IsEBB))
      []
      "A block was added to the Volatile DB"
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.TryAddToCurrentChain
          protoRealPoint))
      []
      "The block fits onto the current chain, we'll try to use it to extend\
      \ our chain."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.TrySwitchToAFork
          protoRealPoint protoHeaderDiff))
      []
      "The block fits onto some fork, we'll try to switch to that fork (if\
      \ it is preferable to our chain)"
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.StoreButDontChange
          protoRealPoint))
      []
      "The block fits onto some fork, we'll try to switch to that fork (if\
      \ it is preferable to our chain)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddedToCurrentChain [] protoNTI protoAFH protoAFH))
      [(["density"],
        "The actual number of blocks created over the maximum expected number\
        \ of blocks that could be created over the span of the last @k@ blocks.")
      , (["slots"],
        "Number of slots in this chain fragment.")
      , (["blocks"],
        "Number of blocks in this chain fragment.")
      , (["slotInEpoch"],
        "Relative slot number of the tip of the current chain within the\
        \epoch..")
      , (["epoch"],
        "In which epoch is the tip of the current chain.")
      ]
      "The new block fits onto the current chain (first\
      \ fragment) and we have successfully used it to extend our (new) current\
      \ chain (second fragment)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.SwitchedToAFork [] protoNTI protoAFH protoAFH))
      [(["density"],
        "The actual number of blocks created over the maximum expected number\
        \ of blocks that could be created over the span of the last @k@ blocks.")
      , (["slots"],
        "Number of slots in this chain fragment.")
      , (["blocks"],
        "Number of blocks in this chain fragment.")
      , (["slotInEpoch"],
        "Relative slot number of the tip of the current chain within the\
        \epoch..")
      , (["epoch"],
        "In which epoch is the tip of the current chain.")
      ]
      "The new block fits onto some fork and we have switched to that fork\
      \ (second fragment), as it is preferable to our (previous) current chain\
      \ (first fragment)."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.InvalidBlock protoExtValidationError protoRealPoint)))
      []
      "An event traced during validating performed while adding a block.\
      \ A point was found to be invalid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.InvalidCandidate protoAFH)))
      []
      "An event traced during validating performed while adding a block.\
      \ A candidate chain was invalid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.ValidCandidate protoAFH)))
      []
      "An event traced during validating performed while adding a block.\
      \ A candidate chain was valid."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocks protoAFH [])))
      []
      "An event traced during validating performed while adding a block.\
      \ Candidate contains headers from the future which do no exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.AddBlockValidation
          (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew protoAFH [])))
      []
      "An event traced during validating performed while adding a block.\
      \ Candidate contains headers from the future which exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceAddBlockEvent
        (ChainDB.ChainSelectionForFutureBlock protoRealPoint))
      []
      "Run chain selection for a block that was previously from the future.\
      \ This is done for all blocks from the future each time a new block is\
      \ added."
  , DocMsg
      (ChainDB.TraceFollowerEvent ChainDB.NewFollower)
      []
      "A new follower was created."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerNoLongerInMem protoFollowerRollState))
      []
      "The follower was in the 'FollowerInImmutableDB' state and is switched to\
      \ the 'FollowerInMem' state."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerSwitchToMem protoPoint protoWoSlotNo))
      []
      "The follower was in the 'FollowerInImmutableDB' state and is switched to\
      \ the 'FollowerInMem' state."
  , DocMsg
      (ChainDB.TraceFollowerEvent
        (ChainDB.FollowerNewImmIterator protoPoint protoWoSlotNo))
      []
      "The follower is in the 'FollowerInImmutableDB' state but the iterator is\
      \ exhausted while the ImmDB has grown, so we open a new iterator to\
      \ stream these blocks too."
  , DocMsg
      (ChainDB.TraceCopyToImmutableDBEvent
        (ChainDB.CopiedBlockToImmutableDB protoPoint))
      []
      "A block was successfully copied to the ImmDB."
  , DocMsg
      (ChainDB.TraceCopyToImmutableDBEvent
        (ChainDB.NoBlocksToCopyToImmutableDB))
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceGCEvent
        (ChainDB.ScheduledGC 1 protoTime))
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceGCEvent
        (ChainDB.PerformedGC 1))
      []
      "There are no block to copy to the ImmDB."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.InvalidBlock protoExtValidationError protoRealPoint)))
      []
      "A point was found to be invalid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.InvalidCandidate protoAFH)))
      []
      "A candidate chain was invalid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.ValidCandidate protoAFH)))
      []
      "A candidate chain was valid."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.CandidateContainsFutureBlocks protoAFH [])))
      []
      "Candidate contains headers from the future which do not exceed the\
      \ clock skew."
  , DocMsg
      (ChainDB.TraceInitChainSelEvent
        (ChainDB.InitChainSelValidation
          (ChainDB.CandidateContainsFutureBlocksExceedingClockSkew protoAFH [])))
      []
      "Candidate contains headers from the future which exceed the\
      \ clock skew, making them invalid."

  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.OpenedDB protoPoint protoPoint))
      []
      "The ChainDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.ClosedDB protoPoint protoPoint))
      []
      "The ChainDB was closed."
  , DocMsg
      (ChainDB.TraceOpenEvent
        (ChainDB.OpenedImmutableDB protoPoint protoChunkNo))
      []
      "The ImmDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        ChainDB.OpenedVolatileDB)
      []
      "The VolatileDB was opened."
  , DocMsg
      (ChainDB.TraceOpenEvent
        ChainDB.OpenedLgrDB)
      []
      "The LedgerDB was opened."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.UnknownRangeRequested protoUnknownRange))
      []
      "An unknown range was requested, see 'UnknownRange'."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromVolatileDB protoStreamFrom protoStreamTo [protoRealPoint]))
      []
      "Stream only from the VolatileDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromImmutableDB protoStreamFrom protoStreamTo))
      []
      "Stream only from the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.StreamFromBoth protoStreamFrom protoStreamTo [protoRealPoint]))
      []
      "Stream from both the VolatileDB and the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockMissingFromVolatileDB protoRealPoint))
      []
      "A block is no longer in the VolatileDB because it has been garbage\
      \ collected. It might now be in the ImmDB if it was part of the\
      \ current chain."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockWasCopiedToImmutableDB protoRealPoint))
      []
      "A block that has been garbage collected from the VolatileDB is now\
      \ found and streamed from the ImmDB."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        (ChainDB.BlockGCedFromVolatileDB protoRealPoint))
      []
      "A block is no longer in the VolatileDB and isn't in the ImmDB\
      \ either; it wasn't part of the current chain."
  , DocMsg
      (ChainDB.TraceIteratorEvent
        ChainDB.SwitchBackToVolatileDB)
      []
      "We have streamed one or more blocks from the ImmDB that were part\
      \ of the VolatileDB when initialising the iterator. Now, we have to look\
      \ back in the VolatileDB again because the ImmDB doesn't have the\
      \ next block we're looking for."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.InvalidSnapshot protoDiskSnapshot protoInitFailure))
      []
      "An on disk snapshot was skipped because it was invalid."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.TookSnapshot protoDiskSnapshot protoRealPoint))
      []
      "A snapshot was written to disk."
  , DocMsg
      (ChainDB.TraceLedgerEvent
        (LedgerDB.DeletedSnapshot protoDiskSnapshot))
      []
      "An old or invalid on-disk snapshot was deleted."

  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayFromGenesis protoPoint))
      []
      "There were no LedgerDB snapshots on disk, so we're replaying all\
      \ blocks starting from Genesis against the initial ledger.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayFromSnapshot protoDiskSnapshot protoRealPoint protoPoint))
      []
      "There was a LedgerDB snapshot on disk corresponding to the given tip.\
      \ We're replaying more recent blocks against it.\
      \ The @replayTo@ parameter corresponds to the block at the tip of the\
      \ ImmDB, i.e., the last block to replay."
  , DocMsg
      (ChainDB.TraceLedgerReplayEvent
        (LedgerDB.ReplayedBlock protoRealPoint [] protoPoint))
      []
      "We replayed the given block (reference) on the genesis snapshot\
      \ during the initialisation of the LedgerDB.\
      \ \
      \ The @blockInfo@ parameter corresponds replayed block and the @replayTo@\
      \ parameter corresponds to the block at the tip of the ImmDB, i.e.,\
      \ the last block to replay."

  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.NoValidLastLocation)
      []
      "No valid last location was found"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ValidatedLastLocation protoChunkNo protoTip))
      []
      "The last location was validatet"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ValidatingChunk protoChunkNo))
      []
      "The chunk was validatet"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingChunkFile protoChunkNo))
      []
      "Chunk file is missing"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidChunkFile protoChunkNo protoChunkFileError))
      []
      "Chunk file is invalid"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.ChunkFileDoesntFit protoChainHash protoChainHash))
      []
      "The hash of the last block in the previous epoch doesn't match the\
      \ previous hash of the first block in the current epoch"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingPrimaryIndex protoChunkNo))
      []
      "The primary index is missing."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.MissingSecondaryIndex protoChunkNo))
      []
      "The secondary index is missing."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidPrimaryIndex protoChunkNo))
      []
      "The primary index is invalid."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.InvalidSecondaryIndex protoChunkNo))
      []
      "The secondary index is invalid."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.RewritePrimaryIndex protoChunkNo))
      []
      "The primary index gets rewritten."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.RewriteSecondaryIndex protoChunkNo))
      []
      "The secondary index gets rewritten."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.Migrating ""))
      []
      "Performing a migration of the on-disk files."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.DeletingAfter protoWithOriginTip))
      []
      "Delete after"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.DBAlreadyClosed)
      []
      "The immutable DB is already closed"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent ImmDB.DBClosed)
      []
      "Closing the immutable DB"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TraceCurrentChunkHit protoChunkNo 1)))
      []
      "Current chunk found in the cache."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkHit protoChunkNo 1)))
      []
      "Past chunk found in the cache"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkMiss protoChunkNo 1)))
      []
      "Past chunk was not found in the cache"
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunkEvict protoChunkNo 1)))
      []
      "The least recently used past chunk was evicted because the cache\
      \ was full."
  , DocMsg
      (ChainDB.TraceImmutableDBEvent
        (ImmDB.TraceCacheEvent
          (ImmDB.TracePastChunksExpired [protoChunkNo] 1)))
      []
      "Past chunks were expired from the cache because they haven't been\
      \ used for a while."

  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.DBAlreadyClosed))
      []
      "When closing the DB it was found itis closed already."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.DBAlreadyOpen))
      []
      "TODO Doc"
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.Truncate protoParseError protoFsPath protoBlockOffset))
      []
      "Truncates a file up to offset because of the error."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.InvalidFileNames [protoFsPath]))
      []
      "Reports a list of invalid file paths."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.BlockAlreadyHere undefined))
      []
      "A block was found to be already in the DB."
  , DocMsg
      (ChainDB.TraceVolatileDBEvent
        (VolDB.TruncateCurrentFile protoFsPath))
      []
      "TODO Doc"
  ]
