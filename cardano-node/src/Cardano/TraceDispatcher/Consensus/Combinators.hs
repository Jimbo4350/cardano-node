
module Cardano.TraceDispatcher.Consensus.Combinators
  (
    severityChainSyncClientEvent
  , namesForChainSyncClientEvent

  , severityChainSyncServerEvent
  , namesForChainSyncServerEvent

  , severityBlockFetchDecision
  , namesForBlockFetchDecision

  , severityBlockFetchClient
  , namesForBlockFetchClient

  , severityBlockFetchServer
  , namesForBlockFetchServer

  , severityTxInbound
  , namesForTxInbound

  , severityTxOutbound
  , namesForTxOutbound

  , severityLocalTxSubmissionServer
  , namesForLocalTxSubmissionServer

  , severityMempool
  , namesForMempool

  , TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  , severityForge
  , namesForForge

  , namesForBlockchainTime
  , severityBlockchainTime

  , namesForKeepAliveClient
  , severityKeepAliveClient

  ) where


import           Cardano.Logging
import           Cardano.Prelude
import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck

import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.TxSubmission.Inbound
import           Ouroboros.Network.TxSubmission.Outbound

import           Ouroboros.Consensus.Block (Point)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import           Ouroboros.Consensus.Node.Tracers

severityChainSyncClientEvent ::
  BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk) -> SeverityS
severityChainSyncClientEvent (BlockFetch.TraceLabelPeer _ e) =
    severityChainSyncClientEvent' e

namesForChainSyncClientEvent ::
  BlockFetch.TraceLabelPeer peer (TraceChainSyncClientEvent blk) -> [Text]
namesForChainSyncClientEvent (BlockFetch.TraceLabelPeer _ e) =
    namesForChainSyncClientEvent' e


severityChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> SeverityS
severityChainSyncClientEvent' TraceDownloadedHeader {}  = Info
severityChainSyncClientEvent' TraceFoundIntersection {} = Info
severityChainSyncClientEvent' TraceRolledBack {}        = Notice
severityChainSyncClientEvent' TraceException {}         = Warning
severityChainSyncClientEvent' TraceTermination {}       = Notice

namesForChainSyncClientEvent' :: TraceChainSyncClientEvent blk -> [Text]
namesForChainSyncClientEvent' TraceDownloadedHeader {} =
      ["DownloadedHeader"]
namesForChainSyncClientEvent' TraceFoundIntersection {} =
      ["FoundIntersection"]
namesForChainSyncClientEvent' TraceRolledBack {} =
      ["RolledBack"]
namesForChainSyncClientEvent' TraceException {} =
      ["Exception"]
namesForChainSyncClientEvent' TraceTermination {} =
      ["Termination"]

severityChainSyncServerEvent :: TraceChainSyncServerEvent blk -> SeverityS
severityChainSyncServerEvent TraceChainSyncServerRead        {} = Info
severityChainSyncServerEvent TraceChainSyncServerReadBlocked {} = Info
severityChainSyncServerEvent TraceChainSyncRollForward       {} = Info
severityChainSyncServerEvent TraceChainSyncRollBackward      {} = Info

namesForChainSyncServerEvent :: TraceChainSyncServerEvent blk -> [Text]
namesForChainSyncServerEvent TraceChainSyncServerRead        {} =
      ["ServerRead"]
namesForChainSyncServerEvent TraceChainSyncServerReadBlocked {} =
      ["ServerReadBlocked"]
namesForChainSyncServerEvent TraceChainSyncRollForward       {} =
      ["RollForward"]
namesForChainSyncServerEvent TraceChainSyncRollBackward      {} =
      ["RollBackward"]

severityBlockFetchDecision ::
     [BlockFetch.TraceLabelPeer peer (FetchDecision [Point header])]
  -> SeverityS
severityBlockFetchDecision []  = Info
severityBlockFetchDecision l   = maximum $
  map (\(BlockFetch.TraceLabelPeer _ a) -> fetchDecisionSeverity a) l
    where
      fetchDecisionSeverity :: FetchDecision a -> SeverityS
      fetchDecisionSeverity fd =
        case fd of
          Left FetchDeclineChainNotPlausible     -> Debug
          Left FetchDeclineChainNoIntersection   -> Notice
          Left FetchDeclineAlreadyFetched        -> Debug
          Left FetchDeclineInFlightThisPeer      -> Debug
          Left FetchDeclineInFlightOtherPeer     -> Debug
          Left FetchDeclinePeerShutdown          -> Info
          Left FetchDeclinePeerSlow              -> Info
          Left FetchDeclineReqsInFlightLimit {}  -> Info
          Left FetchDeclineBytesInFlightLimit {} -> Info
          Left FetchDeclinePeerBusy {}           -> Info
          Left FetchDeclineConcurrencyLimit {}   -> Info
          Right _                                -> Info

namesForBlockFetchDecision ::
     [BlockFetch.TraceLabelPeer peer (FetchDecision [Point header])]
  -> [Text]
namesForBlockFetchDecision _ = []

severityBlockFetchClient ::
     BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> SeverityS
severityBlockFetchClient (BlockFetch.TraceLabelPeer _p bf) = severityBlockFetchClient' bf

severityBlockFetchClient' ::
     (BlockFetch.TraceFetchClientState header)
  -> SeverityS
severityBlockFetchClient' BlockFetch.AddedFetchRequest {}        = Info
severityBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {} = Info
severityBlockFetchClient' BlockFetch.StartedFetchBatch {}        = Info
severityBlockFetchClient' BlockFetch.CompletedBlockFetch {}      = Info
severityBlockFetchClient' BlockFetch.CompletedFetchBatch {}      = Info
severityBlockFetchClient' BlockFetch.RejectedFetchBatch {}       = Info
severityBlockFetchClient' BlockFetch.ClientTerminating {}        = Notice

namesForBlockFetchClient ::
    BlockFetch.TraceLabelPeer peer (BlockFetch.TraceFetchClientState header)
  -> [Text]
namesForBlockFetchClient (BlockFetch.TraceLabelPeer _p bf) = namesForBlockFetchClient' bf

namesForBlockFetchClient' ::
    BlockFetch.TraceFetchClientState header
  -> [Text]
namesForBlockFetchClient' BlockFetch.AddedFetchRequest {} =
      ["AddedFetchRequest"]
namesForBlockFetchClient' BlockFetch.AcknowledgedFetchRequest {}  =
      ["AcknowledgedFetchRequest"]
namesForBlockFetchClient' BlockFetch.StartedFetchBatch {} =
      ["StartedFetchBatch"]
namesForBlockFetchClient' BlockFetch.CompletedBlockFetch  {} =
      ["CompletedBlockFetch"]
namesForBlockFetchClient' BlockFetch.CompletedFetchBatch {} =
      ["CompletedFetchBatch"]
namesForBlockFetchClient' BlockFetch.RejectedFetchBatch  {} =
      ["RejectedFetchBatch"]
namesForBlockFetchClient' BlockFetch.ClientTerminating {} =
      ["ClientTerminating"]

severityBlockFetchServer ::
     (TraceBlockFetchServerEvent blk)
  -> SeverityS
severityBlockFetchServer _ = Info

namesForBlockFetchServer ::
     (TraceBlockFetchServerEvent blk)
  -> [Text]
namesForBlockFetchServer TraceBlockFetchServerSendBlock {} = ["SendBlock"]

severityTxInbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxInbound (BlockFetch.TraceLabelPeer _p ti) = severityTxInbound' ti

severityTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> SeverityS
severityTxInbound' _ti = Info

namesForTxInbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxInbound (BlockFetch.TraceLabelPeer _p ti) = namesForTxInbound' ti

namesForTxInbound' ::
    TraceTxSubmissionInbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxInbound' TraceTxSubmissionCollected {} =
    ["TxSubmissionCollected"]
namesForTxInbound' TraceTxSubmissionProcessed {} =
    ["TxSubmissionProcessed"]
namesForTxInbound' TraceTxInboundTerminated {}   =
    ["TxInboundTerminated"]
namesForTxInbound' TraceTxInboundCanRequestMoreTxs {} =
    ["TxInboundCanRequestMoreTxs"]
namesForTxInbound' TraceTxInboundCannotRequestMoreTxs {} =
    ["TxInboundCannotRequestMoreTxs"]

severityTxOutbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> SeverityS
severityTxOutbound (BlockFetch.TraceLabelPeer _p ti) = severityTxOutbound' ti

severityTxOutbound' ::
    TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)
  -> SeverityS
severityTxOutbound' _ti = Info

namesForTxOutbound ::
    BlockFetch.TraceLabelPeer peer (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))
  -> [Text]
namesForTxOutbound (BlockFetch.TraceLabelPeer _p ti) = namesForTxOutbound' ti

namesForTxOutbound' ::
    TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk)
  -> [Text]
namesForTxOutbound' TraceTxSubmissionOutboundRecvMsgRequestTxs {} =
    ["TxSubmissionOutboundRecvMsgRequest"]
namesForTxOutbound' TraceTxSubmissionOutboundSendMsgReplyTxs {} =
    ["TxSubmissionOutboundSendMsgReply"]
namesForTxOutbound' TraceControlMessage {} =
    ["ControlMessage"]

severityLocalTxSubmissionServer ::
     (TraceLocalTxSubmissionServerEvent blk)
  -> SeverityS
severityLocalTxSubmissionServer _ = Info

namesForLocalTxSubmissionServer ::
  (TraceLocalTxSubmissionServerEvent blk)
  -> [Text]
namesForLocalTxSubmissionServer TraceReceivedTx {} = ["ReceivedTx"]

severityMempool ::
     (TraceEventMempool blk)
  -> SeverityS
severityMempool _ = Info

-- TODO: not working with undefines because of bang patterns
namesForMempool :: TraceEventMempool blk -> [Text]
-- namesForMempool (TraceMempoolAddedTx _ _ _)            = ["AddedTx"]
-- namesForMempool TraceMempoolRejectedTx {}         = ["RejectedTx"]
-- namesForMempool TraceMempoolRemoveTxs {}          = ["RemoveTxs"]
-- namesForMempool TraceMempoolManuallyRemovedTxs {} = ["ManuallyRemovedTxs"]
namesForMempool _            = []

severityForge :: ForgeTracerType blk -> SeverityS
severityForge (Left t)  = severityForge' t
severityForge (Right t) = severityForge''' t

severityForge' :: TraceLabelCreds (TraceForgeEvent blk) -> SeverityS
severityForge' (TraceLabelCreds _t e) = severityForge'' e

severityForge'' :: TraceForgeEvent blk -> SeverityS
severityForge'' TraceStartLeadershipCheck {}  = Info
severityForge'' TraceSlotIsImmutable {}       = Error
severityForge'' TraceBlockFromFuture {}       = Error
severityForge'' TraceBlockContext {}          = Debug
severityForge'' TraceNoLedgerState {}         = Error
severityForge'' TraceLedgerState {}           = Debug
severityForge'' TraceNoLedgerView {}          = Error
severityForge'' TraceLedgerView {}            = Debug
severityForge'' TraceForgeStateUpdateError {} = Error
severityForge'' TraceNodeCannotForge {}       = Error
severityForge'' TraceNodeNotLeader {}         = Info
severityForge'' TraceNodeIsLeader {}          = Info
severityForge'' TraceForgedBlock {}           = Info
severityForge'' TraceDidntAdoptBlock {}       = Error
severityForge'' TraceForgedInvalidBlock {}    = Error
severityForge'' TraceAdoptedBlock {}          = Info

severityForge''' :: TraceLabelCreds TraceStartLeadershipCheckPlus -> SeverityS
severityForge''' _ = Info

namesForForge :: ForgeTracerType blk -> [Text]
namesForForge (Left t)  = namesForForge' t
namesForForge (Right t) = namesForForge''' t

namesForForge' :: TraceLabelCreds (TraceForgeEvent blk) -> [Text]
namesForForge' (TraceLabelCreds _t e) = namesForForge'' e

namesForForge'' :: TraceForgeEvent blk -> [Text]
namesForForge'' TraceStartLeadershipCheck {}  = ["StartLeadershipCheck"]
namesForForge'' TraceSlotIsImmutable {}       = ["SlotIsImmutable"]
namesForForge'' TraceBlockFromFuture {}       = ["BlockFromFuture"]
namesForForge'' TraceBlockContext {}          = ["BlockContext"]
namesForForge'' TraceNoLedgerState {}         = ["NoLedgerState"]
namesForForge'' TraceLedgerState {}           = ["LedgerState"]
namesForForge'' TraceNoLedgerView {}          = ["NoLedgerView"]
namesForForge'' TraceLedgerView {}            = ["LedgerView"]
namesForForge'' TraceForgeStateUpdateError {} = ["ForgeStateUpdateError"]
namesForForge'' TraceNodeCannotForge {}       = ["NodeCannotForge"]
namesForForge'' TraceNodeNotLeader {}         = ["NodeNotLeader"]
namesForForge'' TraceNodeIsLeader {}          = ["NodeIsLeader"]
namesForForge'' TraceForgedBlock {}           = ["ForgedBlock"]
namesForForge'' TraceDidntAdoptBlock {}       = ["DidntAdoptBlock"]
namesForForge'' TraceForgedInvalidBlock {}    = ["ForgedInvalidBlock"]
namesForForge'' TraceAdoptedBlock {}          = ["AdoptedBlock"]

namesForForge''' :: TraceLabelCreds TraceStartLeadershipCheckPlus -> [Text]
namesForForge''' (TraceLabelCreds _ (TraceStartLeadershipCheckPlus {}))  =
  ["StartLeadershipCheckPlus"]

namesForBlockchainTime :: TraceBlockchainTimeEvent t -> [Text]
namesForBlockchainTime TraceStartTimeInTheFuture {} = ["StartTimeInTheFuture"]
namesForBlockchainTime TraceCurrentSlotUnknown {}   = ["CurrentSlotUnknown"]
namesForBlockchainTime TraceSystemClockMovedBack {} = ["SystemClockMovedBack"]

-- TODO JNF: Confirm the severities
severityBlockchainTime :: TraceBlockchainTimeEvent t -> SeverityS
severityBlockchainTime TraceStartTimeInTheFuture {} = Warning
severityBlockchainTime TraceCurrentSlotUnknown {}   = Warning
severityBlockchainTime TraceSystemClockMovedBack {} = Warning

namesForKeepAliveClient :: TraceKeepAliveClient peer -> [Text]
namesForKeepAliveClient _ = ["KeepAliveClient"]

severityKeepAliveClient :: TraceKeepAliveClient peer -> SeverityS
severityKeepAliveClient _ = Info
