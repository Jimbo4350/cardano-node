{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Consensus.Docu
  ( docChainSyncClientEvent
  , docChainSyncServerEvent
  , docBlockFetchDecision
  , docBlockFetchClient
  , docBlockFetchServer
  , docTxInbound
  , docTxOutbound
  , docLocalTxSubmissionServer
  , docMempool
  , docForge
  , docForgeStateInfo
  , docBlockchainTime
  , docKeepAliveClient
  ) where

import           Cardano.Logging
import           Cardano.Prelude
import           Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import           Data.Time.Clock

import           Ouroboros.Consensus.Block
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types
                     (SystemStart (..))
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Forecast (OutsideForecastRange)
import           Ouroboros.Consensus.HardFork.History (PastHorizonException)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId, Validated)
import           Ouroboros.Consensus.Mempool.API (MempoolSize (..),
                     TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey

import           Ouroboros.Network.Block
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision,
                     FetchDecline (..))
import           Ouroboros.Network.BlockFetch.DeltaQ
                     (PeerFetchInFlightLimits (..), PeerGSV)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.Mux (ControlMessage)
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Cardano.TraceDispatcher.Consensus.Combinators
                     (TraceStartLeadershipCheckPlus (..))
import           Cardano.TraceDispatcher.Era.Byron ()
import           Cardano.TraceDispatcher.Era.Shelley ()



protoHeader :: Header blk
protoHeader = undefined

protoPoint :: Point blk
protoPoint = Point Origin

protoPointH :: Point (Header blk)
protoPointH = Point Origin

protoOurTipBlock :: Our (Tip blk)
protoOurTipBlock = undefined

protoTheirTipBlock :: Their (Tip blk)
protoTheirTipBlock = undefined

protoChainSyncClientException :: ChainSyncClientException
protoChainSyncClientException = undefined

protoChainSyncClientResult :: ChainSyncClientResult
protoChainSyncClientResult = undefined

protoChainUpdate :: ChainUpdate blk a
protoChainUpdate = undefined

protoTip :: Tip blk
protoTip = undefined

protoRemotePeer :: remotePeer
protoRemotePeer = undefined

protoFetchDecline :: FetchDecision [Point (Header blk)]
protoFetchDecline = Left FetchDeclineChainNotPlausible

_protoFetchResult :: FetchDecision [Point (Header blk)]
_protoFetchResult = Right [protoPointH]

protoFetchRequest :: BlockFetch.FetchRequest (Header blk)
protoFetchRequest = undefined

protoPeerFetchInFlight :: BlockFetch.PeerFetchInFlight (Header blk)
protoPeerFetchInFlight = undefined

protoPeerFetchInFlightLimits :: PeerFetchInFlightLimits
protoPeerFetchInFlightLimits = PeerFetchInFlightLimits 10 10

protoPeerFetchStatus :: BlockFetch.PeerFetchStatus (Header blk)
protoPeerFetchStatus = undefined

protoChainRange :: BlockFetch.ChainRange (Point header)
protoChainRange = undefined

protoNominalDiffTime :: NominalDiffTime
protoNominalDiffTime = nominalDay

protoDiffTime :: DiffTime
protoDiffTime = secondsToDiffTime 100

protoProcessedTxCount :: ProcessedTxCount
protoProcessedTxCount = ProcessedTxCount 2 1

protoTx :: tx
protoTx = undefined

protoTxId :: txId
protoTxId = undefined

protoGenTxId :: GenTxId txId
protoGenTxId = undefined

protoControlMessage :: ControlMessage
protoControlMessage = undefined

protoGenTx :: GenTx blk
protoGenTx = undefined

protoValidatedGenTx :: Validated (GenTx blk)
protoValidatedGenTx = undefined

protoMempoolSize :: MempoolSize
protoMempoolSize = undefined

protoTxt :: Text
protoTxt = "info"

protoSlotNo :: SlotNo
protoSlotNo = SlotNo 1

protoBlockNo :: BlockNo
protoBlockNo = BlockNo 2

protoBlk :: blk
protoBlk = undefined

protoOutsideForecastRange :: OutsideForecastRange
protoOutsideForecastRange = undefined

protoInvalidBlockReason :: InvalidBlockReason blk
protoInvalidBlockReason = undefined

protoKESInfo :: HotKey.KESInfo
protoKESInfo = HotKey.KESInfo undefined undefined undefined

protoUTCTime :: UTCTime
protoUTCTime = UTCTime (fromOrdinalDate 2021 100) protoDiffTime

protoSystemStart :: SystemStart
protoSystemStart = SystemStart protoUTCTime

protoPastHorizonException :: PastHorizonException
protoPastHorizonException = undefined

protoPeerGSV :: PeerGSV
protoPeerGSV = undefined

-- Not working because of non-injective type families
-- protoApplyTxErr :: ApplyTxErr blk
-- protoApplyTxErr = undefined

-- protoForgeStateUpdateError :: ForgeStateUpdateError blk
-- protoForgeStateUpdateError = undefined

-- protoCannotForge :: CannotForge blk
-- protoCannotForge = undefined

--------------------

docChainSyncClientEvent ::
  Documented (BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk))
docChainSyncClientEvent = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (TraceDownloadedHeader protoHeader))
      []
      "While following a candidate chain, we rolled forward by downloading a\
      \ header."
  , DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (TraceRolledBack protoPoint))
      []
      "While following a candidate chain, we rolled back to the given point."
  , DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (TraceFoundIntersection protoPoint protoOurTipBlock protoTheirTipBlock))
      []
      "We found an intersection between our chain fragment and the\
      \ candidate's chain."
  , DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (TraceException protoChainSyncClientException))
      []
      "An exception was thrown by the Chain Sync Client."
  , DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (TraceTermination protoChainSyncClientResult))
      []
      "The client has terminated."
  ]

docChainSyncServerEvent :: Documented (TraceChainSyncServerEvent blk)
docChainSyncServerEvent = Documented [
    DocMsg
      (TraceChainSyncServerRead protoTip protoChainUpdate)
      []
      "A server read has occured, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncServerReadBlocked protoTip protoChainUpdate)
      []
      "A server read has blocked, either for an add block or a rollback"
    , DocMsg
      (TraceChainSyncRollForward protoPoint)
      [(["ChainSync","RollForward"], "TODO Doc")]
      "Roll forward to the given point."
    , DocMsg
      (TraceChainSyncRollBackward protoPoint)
      []
      "TODO Doc"
  ]

docBlockFetchDecision ::
  Documented ([BlockFetch.TraceLabelPeer remotePeer (FetchDecision [Point (Header blk)])])
docBlockFetchDecision = Documented [
    DocMsg
      [BlockFetch.TraceLabelPeer protoRemotePeer protoFetchDecline]
      [(["connectedPeers"], "Number of connected peers")]
      "Throughout the decision making process we accumulate reasons to decline\
      \ to fetch any blocks. This message carries the intermediate and final\
      \ results."
  ]


docBlockFetchClient ::
  Documented (BlockFetch.TraceLabelPeer remotePeer (BlockFetch.TraceFetchClientState (Header blk)))
docBlockFetchClient = Documented [
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.AddedFetchRequest
          protoFetchRequest
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "The block fetch decision thread has added a new fetch instruction\
      \ consisting of one or more individual request ranges."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.AcknowledgedFetchRequest
          protoFetchRequest))
      []
      "Mark the point when the fetch client picks up the request added\
      \ by the block fetch decision thread. Note that this event can happen\
      \ fewer times than the 'AddedFetchRequest' due to fetch request merging."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.StartedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "Mark the start of receiving a streaming batch of blocks. This will\
      \ be followed by one or more 'CompletedBlockFetch' and a final\
      \ 'CompletedFetchBatch'"
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.CompletedBlockFetch
          protoPointH
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus
          protoNominalDiffTime))
      []
      "Mark the completion of of receiving a single block within a\
      \ streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.CompletedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "Mark the successful end of receiving a streaming batch of blocks."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.RejectedFetchBatch
          protoChainRange
          protoPeerFetchInFlight
          protoPeerFetchInFlightLimits
          protoPeerFetchStatus))
      []
      "If the other peer rejects our request then we have this event\
      \ instead of 'StartedFetchBatch' and 'CompletedFetchBatch'."
  ,
    DocMsg
      (BlockFetch.TraceLabelPeer protoRemotePeer
        (BlockFetch.ClientTerminating 1))
      []
      "The client is terminating.  Log the number of outstanding\
      \ requests."
  ]

docBlockFetchServer ::
  Documented (TraceBlockFetchServerEvent blk)
docBlockFetchServer = Documented [
    DocMsg
      (TraceBlockFetchServerSendBlock protoPoint)
      [(["served","block","count"], "TODO Doc")]
      "The server sent a block to the peer."
  ]


docTxInbound ::
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionInbound txid tx))
docTxInbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionCollected 1))
    [ (["submissions", "submitted", "count"], "TODO Doc")]
    "Number of transactions just about to be inserted."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionProcessed protoProcessedTxCount))
    [ (["submissions", "accepted", "count"], "TODO Doc")
    , (["submissions", "rejected", "count"], "TODO Doc")
    ]
    "Just processed transaction pass/fail breakdown."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      TraceTxInboundTerminated)
    []
    "Server received 'MsgDone'."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxInboundCanRequestMoreTxs 1))
    []
    "There are no replies in flight, but we do know some more txs we\
    \ can ask for, so lets ask for them and more txids."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxInboundCannotRequestMoreTxs 1))
    []
    "There's no replies in flight, and we have no more txs we can\
    \ ask for so the only remaining thing to do is to ask for more\
    \ txids. Since this is the only thing to do now, we make this a\
    \ blocking call."
  ]

docTxOutbound :: forall remotePeer txid tx.
  Documented (BlockFetch.TraceLabelPeer remotePeer
    (TraceTxSubmissionOutbound txid tx))
docTxOutbound = Documented [
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionOutboundRecvMsgRequestTxs [protoTxId]))
    []
    "The IDs of the transactions requested."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceTxSubmissionOutboundSendMsgReplyTxs [protoTx]))
    []
    "The transactions to be sent in the response."
  ,
    DocMsg
    (BlockFetch.TraceLabelPeer protoRemotePeer
      (TraceControlMessage protoControlMessage))
    []
    "TODO Doc"
  ]

docLocalTxSubmissionServer :: Documented (TraceLocalTxSubmissionServerEvent blk)
docLocalTxSubmissionServer = Documented [
    DocMsg
    (TraceReceivedTx protoGenTx)
    []
    "A transaction was received."
  ]

docMempool :: forall blk. Documented (TraceEventMempool blk)
docMempool = Documented [
    DocMsg
      (TraceMempoolAddedTx protoValidatedGenTx protoMempoolSize protoMempoolSize)
      [ (["txsInMempool"],"Transactions in mempool")
      , (["mempoolBytes"], "Byte size of the mempool")
      ]
      "New, valid transaction that was added to the Mempool."
  , DocMsg
      (TraceMempoolRejectedTx protoGenTx (undefined :: ApplyTxErr blk) protoMempoolSize)
      [ (["txsInMempool"],"Transactions in mempool")
      , (["mempoolBytes"], "Byte size of the mempool")
      ]
      "New, invalid transaction thas was rejected and thus not added to\
      \ the Mempool."
  , DocMsg
      (TraceMempoolRemoveTxs [protoValidatedGenTx] protoMempoolSize)
      [ (["txsInMempool"],"Transactions in mempool")
      , (["mempoolBytes"], "Byte size of the mempool")
      ]
      "Previously valid transactions that are no longer valid because of\
      \ changes in the ledger state. These transactions have been removed\
      \ from the Mempool."
  , DocMsg
      (TraceMempoolManuallyRemovedTxs [protoGenTxId] [protoValidatedGenTx] protoMempoolSize)
      [ (["txsInMempool"],"Transactions in mempool")
      , (["mempoolBytes"], "Byte size of the mempool")
      , (["txsProcessedNum"], "TODO Doc")
      ]
      "Transactions that have been manually removed from the Mempool."
  ]


docForge :: Documented (Either (TraceLabelCreds (TraceForgeEvent blk))
                               (TraceLabelCreds TraceStartLeadershipCheckPlus))
docForge = Documented [
    DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceStartLeadershipCheck protoSlotNo)))
      [(["aboutToLeadSlotLast"], "TODO Doc")]
      "Start of the leadership check."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceSlotIsImmutable protoSlotNo protoPoint protoBlockNo)))
      [(["slotIsImmutable"], "TODO Doc")]
      "Leadership check failed: the tip of the ImmutableDB inhabits the\
      \  current slot\
      \ \
      \  This might happen in two cases.\
      \ \
      \   1. the clock moved backwards, on restart we ignored everything from the\
      \      VolatileDB since it's all in the future, and now the tip of the\
      \      ImmutableDB points to a block produced in the same slot we're trying\
      \      to produce a block in\
      \ \
      \   2. k = 0 and we already adopted a block from another leader of the same\
      \      slot.\
      \ \
      \  We record both the current slot number as well as the tip of the\
      \  ImmutableDB.\
      \ \
      \ See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceBlockFromFuture protoSlotNo protoSlotNo)))
      [(["blockFromFuture"], "TODO Doc")]
      "Leadership check failed: the current chain contains a block from a slot\
      \  /after/ the current slot\
      \ \
      \  This can only happen if the system is under heavy load.\
      \ \
      \  We record both the current slot number as well as the slot number of the\
      \  block at the tip of the chain.\
      \ \
      \  See also <https://github.com/input-output-hk/ouroboros-network/issues/1462>"
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceBlockContext protoSlotNo protoBlockNo protoPoint)))
      [(["blockContext"], "TODO Doc")]
      "We found out to which block we are going to connect the block we are about\
      \  to forge.\
      \ \
      \  We record the current slot number, the block number of the block to\
      \  connect to and its point.\
      \ \
      \  Note that block number of the block we will try to forge is one more than\
      \  the recorded block number."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceNoLedgerState protoSlotNo protoPoint)))
      [(["couldNotForgeSlotLast"], "TODO Doc")]
      "Leadership check failed: we were unable to get the ledger state for the\
      \  point of the block we want to connect to\
      \ \
      \  This can happen if after choosing which block to connect to the node\
      \  switched to a different fork. We expect this to happen only rather\
      \  rarely, so this certainly merits a warning; if it happens a lot, that\
      \  merits an investigation.\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceLedgerState protoSlotNo protoPoint)))
      [(["ledgerState"], "TODO Doc")]
      "We obtained a ledger state for the point of the block we want to\
      \  connect to\
      \ \
      \  We record both the current slot number as well as the point of the block\
      \  we attempt to connect the new block to (that we requested the ledger\
      \  state for)."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceNoLedgerView protoSlotNo protoOutsideForecastRange)))
      [(["couldNotForgeSlotLast"], "TODO Doc")]
      "Leadership check failed: we were unable to get the ledger view for the\
      \  current slot number\
      \ \
      \  This will only happen if there are many missing blocks between the tip of\
      \  our chain and the current slot.\
      \ \
      \  We record also the failure returned by 'forecastFor'."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceLedgerView protoSlotNo)))
      [(["ledgerView"], "TODO Doc")]
      "We obtained a ledger view for the current slot number\
      \ \
      \  We record the current slot number."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceForgeStateUpdateError protoSlotNo undefined)))
      [ (["operationalCertificateStartKESPeriod"], "TODO Doc")
      , (["operationalCertificateExpiryKESPeriod"], "TODO Doc")
      , (["currentKESPeriod"], "TODO Doc")
      , (["remainingKESPeriods"], "TODO Doc")
      ]
      "Updating the forge state failed.\
      \ \
      \  For example, the KES key could not be evolved anymore.\
      \ \
      \  We record the error returned by 'updateForgeState'."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceNodeCannotForge protoSlotNo undefined)))
      [(["nodeCannotForge"], "TODO Doc")]
      "We did the leadership check and concluded that we should lead and forge\
      \  a block, but cannot.\
      \ \
      \  This should only happen rarely and should be logged with warning severity.\
      \ \
      \  Records why we cannot forge a block."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceNodeNotLeader protoSlotNo)))
      [(["nodeNotLeader"], "TODO Doc")]
      "We did the leadership check and concluded we are not the leader\
      \ \
      \  We record the current slot number"
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceNodeIsLeader protoSlotNo)))
      [(["nodeIsLeader"], "TODO Doc")]
      "We did the leadership check and concluded we /are/ the leader\
      \ \
      \  The node will soon forge; it is about to read its transactions from the\
      \  Mempool. This will be followed by TraceForgedBlock."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceForgedBlock protoSlotNo protoPoint protoBlk protoMempoolSize)))
      [(["forgedSlotLast"], "TODO Doc")]
      "We forged a block\
      \ \
      \  We record the current slot number, the point of the predecessor, the block\
      \  itself, and the total size of the mempool snapshot at the time we produced\
      \  the block (which may be significantly larger than the block, due to\
      \  maximum block size)\
      \ \
      \  This will be followed by one of three messages:\
      \ \
      \  * TraceAdoptedBlock (normally)\
      \  * TraceDidntAdoptBlock (rarely)\
      \  * TraceForgedInvalidBlock (hopefully never -- this would indicate a bug)"
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceDidntAdoptBlock protoSlotNo protoBlk)))
      [(["notAdoptedSlotLast"], "TODO Doc")]
      "We did not adopt the block we produced, but the block was valid. We\
      \  must have adopted a block that another leader of the same slot produced\
      \  before we got the chance of adopting our own block. This is very rare,\
      \  this warrants a warning."
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceForgedInvalidBlock protoSlotNo protoBlk protoInvalidBlockReason)))
      [(["forgedInvalidSlotLast"], "TODO Doc")]
      "We forged a block that is invalid according to the ledger in the\
      \  ChainDB. This means there is an inconsistency between the mempool\
      \  validation and the ledger validation. This is a serious error!"
  , DocMsg
      (Left (TraceLabelCreds protoTxt
        (TraceAdoptedBlock protoSlotNo protoBlk [protoValidatedGenTx])))
      [(["adoptedSlotLast"], "TODO Doc")]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."
  , DocMsg
      (Right (TraceLabelCreds protoTxt
        (TraceStartLeadershipCheckPlus protoSlotNo 0 0 0.0)))
      [ (["aboutToLeadSlotLast"], "TODO Doc")
      , (["utxoSize"], "TODO Doc")
      , (["delegMapSize"], "TODO Doc")
      ]
      "We adopted the block we produced, we also trace the transactions\
      \  that were adopted."

  ]

docForgeStateInfo :: Documented (TraceLabelCreds (HotKey.KESInfo))
docForgeStateInfo = Documented [
    DocMsg
      (TraceLabelCreds protoTxt protoKESInfo)
      []
      "kesStartPeriod \
      \\nkesEndPeriod is kesStartPeriod + tpraosMaxKESEvo\
      \\nkesEvolution is the current evolution or /relative period/."
    ]

docBlockchainTime :: Documented (TraceBlockchainTimeEvent t)
docBlockchainTime = Documented [
    DocMsg
      (TraceStartTimeInTheFuture protoSystemStart protoNominalDiffTime)
      []
      "The start time of the blockchain time is in the future\
      \\
      \ We have to block (for 'NominalDiffTime') until that time comes."
  , DocMsg
      (TraceCurrentSlotUnknown (undefined :: t) protoPastHorizonException)
      []
      "Current slot is not yet known\
      \\
      \ This happens when the tip of our current chain is so far in the past that\
      \ we cannot translate the current wallclock to a slot number, typically\
      \ during syncing. Until the current slot number is known, we cannot\
      \ produce blocks. Seeing this message during syncing therefore is\
      \ normal and to be expected.\
      \\
      \ We record the current time (the time we tried to translate to a 'SlotNo')\
      \ as well as the 'PastHorizonException', which provides detail on the\
      \ bounds between which we /can/ do conversions. The distance between the\
      \ current time and the upper bound should rapidly decrease with consecutive\
      \ 'TraceCurrentSlotUnknown' messages during syncing."
  , DocMsg
      (TraceSystemClockMovedBack (undefined :: t) (undefined :: t))
      []
      "The system clock moved back an acceptable time span, e.g., because of\
      \ an NTP sync.\
      \\
      \ The system clock moved back such that the new current slot would be\
      \ smaller than the previous one. If this is within the configured limit, we\
      \ trace this warning but *do not change the current slot*. The current slot\
      \ never decreases, but the current slot may stay the same longer than\
      \ expected.\
      \\
      \ When the system clock moved back more than the configured limit, we shut\
      \ down with a fatal exception."
  ]

docKeepAliveClient :: Documented (TraceKeepAliveClient peer)
docKeepAliveClient = Documented [
    DocMsg
      (AddSample (undefined :: peer) protoDiffTime protoPeerGSV)
      []
      "TODO Doc"
  ]
