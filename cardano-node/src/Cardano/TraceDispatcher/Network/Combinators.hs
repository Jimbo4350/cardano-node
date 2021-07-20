{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.TraceDispatcher.Network.Combinators
  (
    severityTChainSync
  , namesForTChainSync

  , severityTTxSubmission
  , namesForTTxSubmission

  , severityTStateQuery
  , namesForTStateQuery

  , severityTChainSyncNode
  , namesForTChainSyncNode

  , severityTChainSyncSerialised
  , namesForTChainSyncSerialised

  , severityTBlockFetch
  , namesForTBlockFetch

  , severityTBlockFetchSerialised
  , namesForTBlockFetchSerialised

  , severityTxSubmissionNode
  , namesForTxSubmissionNode

  , severityTxSubmission2Node
  , namesForTxSubmission2Node

  , severityIPSubscription
  , namesForIPSubscription

  , severityDNSSubscription
  , namesForDNSSubscription

  , severityDNSResolver
  , namesForDNSResolver

  , severityErrorPolicy
  , namesForErrorPolicy

  , severityLocalErrorPolicy
  , namesForLocalErrorPolicy

  , severityAcceptPolicy
  , namesForAcceptPolicy

  , severityMux
  , namesForMux

  , severityHandshake
  , namesForHandshake

  , severityLocalHandshake
  , namesForLocalHandshake

  , severityDiffusionInit
  , namesForDiffusionInit


  ) where


import           Cardano.Logging
import           Cardano.Prelude
import qualified Codec.CBOR.Term as CBOR

import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Ouroboros.Network.Block (Point, Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import qualified Ouroboros.Network.Diffusion as ND
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (DnsTrace (..),
                     ErrorPolicyTrace (..), SubscriptionTrace (..),
                     TraceSendRecv (..), WithAddr (..), WithIPList (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch (..),
                     Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello,
                     Message (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TXS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..),
                     SubscriberError)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

severityTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSync (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSync :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Serialised blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSync (BlockFetch.TraceLabelPeer _ v) = "NodeToClient" : namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> SeverityS
severityTTxSubmission (BlockFetch.TraceLabelPeer _ v) = severityTTxSubmission' v
  where
    severityTTxSubmission' (TraceSendMsg msg) = severityTTxSubmission'' msg
    severityTTxSubmission' (TraceRecvMsg msg) = severityTTxSubmission'' msg

    severityTTxSubmission'' (AnyMessageAndAgency _agency msg) = severityTTxSubmission''' msg

    severityTTxSubmission''' :: Message
                                        (LTS.LocalTxSubmission tx reject) from to
                                      -> SeverityS
    severityTTxSubmission''' LTS.MsgSubmitTx {} = Info
    severityTTxSubmission''' LTS.MsgAcceptTx {} = Info
    severityTTxSubmission''' LTS.MsgRejectTx {} = Info
    severityTTxSubmission''' LTS.MsgDone {}     = Info


namesForTTxSubmission :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LTS.LocalTxSubmission (GenTx blk) (ApplyTxErr blk)))
  -> [Text]
namesForTTxSubmission (BlockFetch.TraceLabelPeer _ v) = namesTTxSubmission v
  where
    namesTTxSubmission (TraceSendMsg msg) = "Send" : namesTTxSubmission' msg
    namesTTxSubmission (TraceRecvMsg msg) = "Recieve" : namesTTxSubmission' msg

    namesTTxSubmission' (AnyMessageAndAgency _agency msg) = namesTTxSubmission'' msg

    namesTTxSubmission'' :: Message
                                    (LTS.LocalTxSubmission tx reject) from to
                                  -> [Text]
    namesTTxSubmission'' LTS.MsgSubmitTx {} = ["SubmitTx"]
    namesTTxSubmission'' LTS.MsgAcceptTx {} = ["AcceptTx"]
    namesTTxSubmission'' LTS.MsgRejectTx {} = ["RejectTx"]
    namesTTxSubmission'' LTS.MsgDone {}     = ["Done"]

severityTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> SeverityS
severityTStateQuery (BlockFetch.TraceLabelPeer _ v) = severityTStateQuery' v
  where
    severityTStateQuery' (TraceSendMsg msg) = severityTStateQuery'' msg
    severityTStateQuery' (TraceRecvMsg msg) = severityTStateQuery'' msg

    severityTStateQuery'' (AnyMessageAndAgency _agency msg) = severityTStateQuery''' msg

    severityTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> SeverityS
    severityTStateQuery''' LSQ.MsgAcquire {}   = Info
    severityTStateQuery''' LSQ.MsgAcquired {}  = Info
    severityTStateQuery''' LSQ.MsgFailure {}   = Warning
    severityTStateQuery''' LSQ.MsgQuery {}     = Info
    severityTStateQuery''' LSQ.MsgResult {}    = Info
    severityTStateQuery''' LSQ.MsgRelease {}   = Info
    severityTStateQuery''' LSQ.MsgReAcquire {} = Info
    severityTStateQuery''' LSQ.MsgDone {}      = Info

namesForTStateQuery :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (LSQ.LocalStateQuery blk (Point blk) query))
  -> [Text]
namesForTStateQuery (BlockFetch.TraceLabelPeer _ v) = namesForTStateQuery' v
  where
    namesForTStateQuery' (TraceSendMsg msg) = namesForTStateQuery'' msg
    namesForTStateQuery' (TraceRecvMsg msg) = namesForTStateQuery'' msg

    namesForTStateQuery'' (AnyMessageAndAgency _agency msg) = namesForTStateQuery''' msg

    namesForTStateQuery''' :: Message
                                    (LSQ.LocalStateQuery block point query1) from to
                                  -> [Text]

    namesForTStateQuery''' LSQ.MsgAcquire {}   = ["Acquire"]
    namesForTStateQuery''' LSQ.MsgAcquired {}  = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgFailure {}   = ["Acquired"]
    namesForTStateQuery''' LSQ.MsgQuery {}     = ["Query"]
    namesForTStateQuery''' LSQ.MsgResult {}    = ["Result"]
    namesForTStateQuery''' LSQ.MsgRelease {}   = ["Release"]
    namesForTStateQuery''' LSQ.MsgReAcquire {} = ["ReAcquire"]
    namesForTStateQuery''' LSQ.MsgDone {}      = ["Done"]

severityTChainSyncNode :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Header blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSyncNode :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (Header blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSyncNode (BlockFetch.TraceLabelPeer _ v) = "NodeToNode" : namesTChainSync v
  where

    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTChainSyncSerialised :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))) -> SeverityS
severityTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) = severityTChainSync' v
  where
    severityTChainSync' (TraceSendMsg msg) = severityTChainSync'' msg
    severityTChainSync' (TraceRecvMsg msg) = severityTChainSync'' msg

    severityTChainSync'' (AnyMessageAndAgency _agency msg) = severityTChainSync''' msg

    severityTChainSync''' :: Message
                                     (ChainSync header point tip) from to
                                   -> SeverityS
    severityTChainSync''' MsgRequestNext {}       = Info
    severityTChainSync''' MsgAwaitReply {}        = Info
    severityTChainSync''' MsgRollForward {}       = Info
    severityTChainSync''' MsgRollBackward {}      = Info
    severityTChainSync''' MsgFindIntersect {}     = Info
    severityTChainSync''' MsgIntersectFound {}    = Info
    severityTChainSync''' MsgIntersectNotFound {} = Info
    severityTChainSync''' MsgDone {}              = Info

namesForTChainSyncSerialised :: BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk))) -> [Text]
namesForTChainSyncSerialised (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTChainSync v
  where
    namesTChainSync (TraceSendMsg msg) = "Send" : namesTChainSync' msg
    namesTChainSync (TraceRecvMsg msg) = "Recieve" : namesTChainSync' msg

    namesTChainSync' (AnyMessageAndAgency _agency msg) = namesTChainSync'' msg

    namesTChainSync'' :: Message (ChainSync header point tip) from to
                               -> [Text]
    namesTChainSync'' MsgRequestNext {}       = ["RequestNext"]
    namesTChainSync'' MsgAwaitReply {}        = ["AwaitReply"]
    namesTChainSync'' MsgRollForward {}       = ["RollForward"]
    namesTChainSync'' MsgRollBackward {}      = ["RollBackward"]
    namesTChainSync'' MsgFindIntersect {}     = ["FindIntersect"]
    namesTChainSync'' MsgIntersectFound {}    = ["IntersectFound"]
    namesTChainSync'' MsgIntersectNotFound {} = ["IntersectNotFound"]
    namesTChainSync'' MsgDone {}              = ["Done"]

severityTBlockFetch :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch blk (Point blk))) -> SeverityS
severityTBlockFetch (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessageAndAgency _agency msg) = severityTBlockFetch''' msg

    severityTBlockFetch''' :: Message (BlockFetch x (Point blk)) from to
                                   -> SeverityS
    severityTBlockFetch''' MsgRequestRange {} = Info
    severityTBlockFetch''' MsgStartBatch {}   = Info
    severityTBlockFetch''' MsgNoBlocks {}     = Info
    severityTBlockFetch''' MsgBlock {}        = Info
    severityTBlockFetch''' MsgBatchDone {}    = Info
    severityTBlockFetch''' MsgClientDone {}   = Info

namesForTBlockFetch :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch blk (Point blk))) -> [Text]
namesForTBlockFetch (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Recieve" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessageAndAgency _agency msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]

severityTBlockFetchSerialised :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))) -> SeverityS
severityTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) = severityTBlockFetch' v
  where
    severityTBlockFetch' (TraceSendMsg msg) = severityTBlockFetch'' msg
    severityTBlockFetch' (TraceRecvMsg msg) = severityTBlockFetch'' msg

    severityTBlockFetch'' (AnyMessageAndAgency _agency msg) = severityTBlockFetch''' msg

    severityTBlockFetch''' :: Message (BlockFetch x (Point blk)) from to
                                   -> SeverityS
    severityTBlockFetch''' MsgRequestRange {} = Info
    severityTBlockFetch''' MsgStartBatch {}   = Info
    severityTBlockFetch''' MsgNoBlocks {}     = Info
    severityTBlockFetch''' MsgBlock {}        = Info
    severityTBlockFetch''' MsgBatchDone {}    = Info
    severityTBlockFetch''' MsgClientDone {}   = Info

namesForTBlockFetchSerialised :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (BlockFetch (Serialised blk) (Point blk))) -> [Text]
namesForTBlockFetchSerialised (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTBlockFetch v
  where
    namesTBlockFetch (TraceSendMsg msg) = "Send" : namesTBlockFetch' msg
    namesTBlockFetch (TraceRecvMsg msg) = "Recieve" : namesTBlockFetch' msg

    namesTBlockFetch' (AnyMessageAndAgency _agency msg) = namesTBlockFetch'' msg

    namesTBlockFetch'' :: Message (BlockFetch x (Point blk)) from to
                               -> [Text]
    namesTBlockFetch'' MsgRequestRange {} = ["RequestRange"]
    namesTBlockFetch'' MsgStartBatch {}   = ["StartBatch"]
    namesTBlockFetch'' MsgNoBlocks {}     = ["NoBlocks"]
    namesTBlockFetch'' MsgBlock {}        = ["Block"]
    namesTBlockFetch'' MsgBatchDone {}    = ["BatchDone"]
    namesTBlockFetch'' MsgClientDone {}   = ["ClientDone"]

severityTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessageAndAgency _agency msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (TXS.TxSubmission (GenTxId blk) (GenTx blk))
          from
          to
     -> SeverityS
    severityTxSubNode'' TXS.MsgRequestTxIds {} = Info
    severityTxSubNode'' TXS.MsgReplyTxIds {}   = Info
    severityTxSubNode'' TXS.MsgRequestTxs {}   = Info
    severityTxSubNode'' TXS.MsgReplyTxs {}     = Info
    severityTxSubNode'' TXS.MsgDone {}         = Info


namesForTxSubmissionNode :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmissionNode (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Recieve" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessageAndAgency _agency msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (TXS.TxSubmission (GenTxId blk) (GenTx blk))
          from
          to
      -> [Text]
    namesTxSubNode'' TXS.MsgRequestTxIds {} = ["RequestTxIds"]
    namesTxSubNode'' TXS.MsgReplyTxIds {}   = ["ReplyTxIds"]
    namesTxSubNode'' TXS.MsgRequestTxs {}   = ["RequestTxs"]
    namesTxSubNode'' TXS.MsgReplyTxs {}     = ["ReplyTxs"]
    namesTxSubNode'' TXS.MsgDone {}         = ["Done"]

severityTxSubmission2Node :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> SeverityS
severityTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) = severityTxSubNode v
  where
    severityTxSubNode (TraceSendMsg msg) = severityTxSubNode' msg
    severityTxSubNode (TraceRecvMsg msg) = severityTxSubNode' msg

    severityTxSubNode' (AnyMessageAndAgency _agency msg) = severityTxSubNode'' msg

    severityTxSubNode'' ::
        Message
          (Hello (TXS.TxSubmission (GenTxId blk) (GenTx blk)) stIdle)
          from
          to
     -> SeverityS
    severityTxSubNode'' MsgHello {}                      = Debug
    severityTxSubNode'' (MsgTalk TXS.MsgRequestTxIds {}) = Info
    severityTxSubNode'' (MsgTalk TXS.MsgReplyTxIds {})   = Info
    severityTxSubNode'' (MsgTalk TXS.MsgRequestTxs {})   = Info
    severityTxSubNode'' (MsgTalk TXS.MsgReplyTxs {})     = Info
    severityTxSubNode'' (MsgTalk TXS.MsgDone {})         = Info

namesForTxSubmission2Node :: BlockFetch.TraceLabelPeer peer
  (TraceSendRecv (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))) -> [Text]
namesForTxSubmission2Node (BlockFetch.TraceLabelPeer _ v) =
  "NodeToNode" : namesTxSubNode v
  where
    namesTxSubNode (TraceSendMsg msg) = "Send" : namesTxSubNode' msg
    namesTxSubNode (TraceRecvMsg msg) = "Recieve" : namesTxSubNode' msg

    namesTxSubNode' (AnyMessageAndAgency _agency msg) = namesTxSubNode'' msg

    namesTxSubNode'' ::
         Message
          (Hello (TXS.TxSubmission (GenTxId blk) (GenTx blk)) stIdle)
          from
          to
      -> [Text]
    namesTxSubNode'' MsgHello {}                      = ["MsgHello"]
    namesTxSubNode'' (MsgTalk TXS.MsgRequestTxIds {}) = ["RequestTxIds"]
    namesTxSubNode'' (MsgTalk TXS.MsgReplyTxIds {})   = ["ReplyTxIds"]
    namesTxSubNode'' (MsgTalk TXS.MsgRequestTxs {})   = ["RequestTxs"]
    namesTxSubNode'' (MsgTalk TXS.MsgReplyTxs {})     = ["ReplyTxs"]
    namesTxSubNode'' (MsgTalk TXS.MsgDone {})         = ["Done"]

severityIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityIPSubscription WithIPList {..} = case wilEvent of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess         -> Info
      ConnectSuccessLast     -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Error
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Notice
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Info
    SubscriptionTraceConnectionExist {} -> Notice
    SubscriptionTraceUnsupportedRemoteAddr {} -> Error
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info

namesForSubscription ::
     SubscriptionTrace Socket.SockAddr
  -> [Text]
namesForSubscription SubscriptionTraceConnectStart {} = ["ConnectStart"]
namesForSubscription SubscriptionTraceConnectEnd {} = ["ConnectEnd"]
namesForSubscription SubscriptionTraceConnectException {} = ["ConnectException"]
namesForSubscription SubscriptionTraceSocketAllocationException {} = ["SocketAllocationException"]
namesForSubscription SubscriptionTraceTryConnectToPeer {}  = ["TryConnectToPeer"]
namesForSubscription SubscriptionTraceSkippingPeer {} = ["SkippingPeer"]
namesForSubscription SubscriptionTraceSubscriptionRunning = ["SubscriptionRunning"]
namesForSubscription SubscriptionTraceSubscriptionWaiting {} = ["SubscriptionWaiting"]
namesForSubscription SubscriptionTraceSubscriptionFailed = ["SubscriptionFailed"]
namesForSubscription SubscriptionTraceSubscriptionWaitingNewConnection {} = ["SubscriptionWaitingNewConnection"]
namesForSubscription SubscriptionTraceStart {} = ["Start"]
namesForSubscription SubscriptionTraceRestart {} = ["Restart"]
namesForSubscription SubscriptionTraceConnectionExist {} = ["ConnectionExist"]
namesForSubscription SubscriptionTraceUnsupportedRemoteAddr {} = ["UnsupportedRemoteAddr"]
namesForSubscription SubscriptionTraceMissingLocalAddress = ["MissingLocalAddress"]
namesForSubscription SubscriptionTraceApplicationException {} = ["ApplicationException"]
namesForSubscription SubscriptionTraceAllocateSocket {} = ["AllocateSocket"]
namesForSubscription SubscriptionTraceCloseSocket {} = ["CloseSocket"]

namesForIPSubscription ::
     WithIPList (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForIPSubscription(WithIPList _ _ e) = "IP" : namesForSubscription e

namesForDNSSubscription ::
     NtN.WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> [Text]
namesForDNSSubscription(NtN.WithDomainName _ e) = "DNS" : namesForSubscription e

severityDNSSubscription ::
     NtN.WithDomainName (SubscriptionTrace Socket.SockAddr)
  -> SeverityS
severityDNSSubscription NtN.WithDomainName {..} = case wdnEvent of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Info
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Debug
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Debug
    SubscriptionTraceRestart {} -> Debug
    SubscriptionTraceConnectionExist {} -> Info
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing                   -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug

severityDNSResolver :: NtN.WithDomainName DnsTrace -> SeverityS
severityDNSResolver (NtN.WithDomainName _ ev) = case ev of
    DnsTraceLookupException {}  -> Error
    DnsTraceLookupAError {}     -> Error
    DnsTraceLookupAAAAError {}  -> Error
    DnsTraceLookupIPv6First     -> Debug
    DnsTraceLookupIPv4First     -> Debug
    DnsTraceLookupAResult {}    -> Debug
    DnsTraceLookupAAAAResult {} -> Debug

namesForDNSResolver :: NtN.WithDomainName DnsTrace -> [Text]
namesForDNSResolver (NtN.WithDomainName _ ev) = case ev of
    DnsTraceLookupException {}  -> ["LookupException"]
    DnsTraceLookupAError {}     -> ["LookupAError"]
    DnsTraceLookupAAAAError {}  -> ["LookupAAAAError"]
    DnsTraceLookupIPv6First     -> ["LookupIPv6First"]
    DnsTraceLookupIPv4First     -> ["LookupIPv4First"]
    DnsTraceLookupAResult {}    -> ["LookupAResult"]
    DnsTraceLookupAAAAResult {} -> ["LookupAAAAResult"]

severityErrorPolicy :: WithAddr Socket.SockAddr ErrorPolicyTrace -> SeverityS
severityErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {}               -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {}                -> Error
    ErrorPolicyResumePeer {}                    -> Debug
    ErrorPolicyKeepSuspended {}                 -> Debug
    ErrorPolicyResumeConsumer {}                -> Debug
    ErrorPolicyResumeProducer {}                -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {}  -> Error
    ErrorPolicyAcceptException {}               -> Error

namesForErrorPolicy :: WithAddr Socket.SockAddr ErrorPolicyTrace -> [Text]
namesForErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> ["SuspendPeer"]
    ErrorPolicySuspendConsumer {}               -> ["SuspendConsumer"]
    ErrorPolicyLocalNodeError {}                -> ["LocalNodeError"]
    ErrorPolicyResumePeer {}                    -> ["ResumePeer"]
    ErrorPolicyKeepSuspended {}                 -> ["KeepSuspended"]
    ErrorPolicyResumeConsumer {}                -> ["ResumeConsumer"]
    ErrorPolicyResumeProducer {}                -> ["ResumeProducer"]
    ErrorPolicyUnhandledApplicationException {} -> ["UnhandledApplicationException"]
    ErrorPolicyUnhandledConnectionException {}  -> ["UnhandledConnectionException"]
    ErrorPolicyAcceptException {}               -> ["AcceptException"]

severityLocalErrorPolicy :: WithAddr NtC.LocalAddress ErrorPolicyTrace -> SeverityS
severityLocalErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {}               -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {}                -> Error
    ErrorPolicyResumePeer {}                    -> Debug
    ErrorPolicyKeepSuspended {}                 -> Debug
    ErrorPolicyResumeConsumer {}                -> Debug
    ErrorPolicyResumeProducer {}                -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {}  -> Error
    ErrorPolicyAcceptException {}               -> Error

namesForLocalErrorPolicy :: WithAddr NtC.LocalAddress ErrorPolicyTrace -> [Text]
namesForLocalErrorPolicy (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {}                   -> ["SuspendPeer"]
    ErrorPolicySuspendConsumer {}               -> ["SuspendConsumer"]
    ErrorPolicyLocalNodeError {}                -> ["LocalNodeError"]
    ErrorPolicyResumePeer {}                    -> ["ResumePeer"]
    ErrorPolicyKeepSuspended {}                 -> ["KeepSuspended"]
    ErrorPolicyResumeConsumer {}                -> ["ResumeConsumer"]
    ErrorPolicyResumeProducer {}                -> ["ResumeProducer"]
    ErrorPolicyUnhandledApplicationException {} -> ["UnhandledApplicationException"]
    ErrorPolicyUnhandledConnectionException {}  -> ["UnhandledConnectionException"]
    ErrorPolicyAcceptException {}               -> ["AcceptException"]

severityAcceptPolicy :: NtN.AcceptConnectionsPolicyTrace -> SeverityS
severityAcceptPolicy NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
severityAcceptPolicy NtN.ServerTraceAcceptConnectionHardLimit {}    = Warning

namesForAcceptPolicy :: NtN.AcceptConnectionsPolicyTrace -> [Text]
namesForAcceptPolicy NtN.ServerTraceAcceptConnectionRateLimiting {} =
    ["ConectionRateLimiting"]
namesForAcceptPolicy NtN.ServerTraceAcceptConnectionHardLimit {} =
    ["ConnectionHardLimit"]

severityMux :: WithMuxBearer peer MuxTrace -> SeverityS
severityMux (WithMuxBearer _ mt) = severityMux' mt

severityMux' :: MuxTrace -> SeverityS
severityMux' MuxTraceRecvHeaderStart {}       = Debug
severityMux' MuxTraceRecvHeaderEnd {}         = Debug
severityMux' MuxTraceRecvStart {}             = Debug
severityMux' MuxTraceRecvEnd {}               = Debug
severityMux' MuxTraceSendStart {}             = Debug
severityMux' MuxTraceSendEnd                  = Debug
severityMux' MuxTraceState {}                 = Info
severityMux' MuxTraceCleanExit {}             = Notice
severityMux' MuxTraceExceptionExit {}         = Notice
severityMux' MuxTraceChannelRecvStart {}      = Debug
severityMux' MuxTraceChannelRecvEnd {}        = Debug
severityMux' MuxTraceChannelSendStart {}      = Debug
severityMux' MuxTraceChannelSendEnd {}        = Debug
severityMux' MuxTraceHandshakeStart           = Debug
severityMux' MuxTraceHandshakeClientEnd {}    = Info
severityMux' MuxTraceHandshakeServerEnd       = Debug
severityMux' MuxTraceHandshakeClientError {}  = Error
severityMux' MuxTraceHandshakeServerError {}  = Error
severityMux' MuxTraceRecvDeltaQObservation {} = Debug
severityMux' MuxTraceRecvDeltaQSample {}      = Debug
severityMux' MuxTraceSDUReadTimeoutException  = Notice
severityMux' MuxTraceSDUWriteTimeoutException = Notice
severityMux' MuxTraceStartEagerly {}          = Debug
severityMux' MuxTraceStartOnDemand {}         = Debug
severityMux' MuxTraceStartedOnDemand {}       = Debug
severityMux' MuxTraceTerminating {}           = Debug
severityMux' MuxTraceShutdown {}              = Debug

namesForMux :: WithMuxBearer peer MuxTrace -> [Text]
namesForMux (WithMuxBearer _ mt) = namesForMux' mt

namesForMux' :: MuxTrace -> [Text]
namesForMux' MuxTraceRecvHeaderStart {}       = ["RecvHeaderStart"]
namesForMux' MuxTraceRecvHeaderEnd {}         = ["RecvHeaderEnd"]
namesForMux' MuxTraceRecvStart {}             = ["RecvStart"]
namesForMux' MuxTraceRecvEnd {}               = ["RecvEnd"]
namesForMux' MuxTraceSendStart {}             = ["SendStart"]
namesForMux' MuxTraceSendEnd                  = ["SendEnd"]
namesForMux' MuxTraceState {}                 = ["State"]
namesForMux' MuxTraceCleanExit {}             = ["CleanExit"]
namesForMux' MuxTraceExceptionExit {}         = ["ExceptionExit"]
namesForMux' MuxTraceChannelRecvStart {}      = ["ChannelRecvStart"]
namesForMux' MuxTraceChannelRecvEnd {}        = ["ChannelRecvEnd"]
namesForMux' MuxTraceChannelSendStart {}      = ["ChannelSendStart"]
namesForMux' MuxTraceChannelSendEnd {}        = ["ChannelSendEnd"]
namesForMux' MuxTraceHandshakeStart           = ["HandshakeStart "]
namesForMux' MuxTraceHandshakeClientEnd {}    = ["HandshakeClientEnd"]
namesForMux' MuxTraceHandshakeServerEnd       = ["HandshakeServerEnd"]
namesForMux' MuxTraceHandshakeClientError {}  = ["HandshakeClientError"]
namesForMux' MuxTraceHandshakeServerError {}  = ["HandshakeServerError"]
namesForMux' MuxTraceRecvDeltaQObservation {} = ["RecvDeltaQObservation"]
namesForMux' MuxTraceRecvDeltaQSample {}      = ["RecvDeltaQSample"]
namesForMux' MuxTraceSDUReadTimeoutException  = ["SDUReadTimeoutException"]
namesForMux' MuxTraceSDUWriteTimeoutException = ["SDUWriteTimeoutException"]
namesForMux' MuxTraceStartEagerly {}          = ["StartEagerly"]
namesForMux' MuxTraceStartOnDemand {}         = ["StartOnDemand"]
namesForMux' MuxTraceStartedOnDemand {}       = ["StartedOnDemand"]
namesForMux' MuxTraceTerminating {}           = ["Terminating"]
namesForMux' MuxTraceShutdown {}              = ["Shutdown"]

severityHandshake :: NtN.HandshakeTr -> SeverityS
severityHandshake (WithMuxBearer _ e) = severityHandshake' e

severityHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> SeverityS
severityHandshake' (TraceSendMsg m) = severityHandshake'' m
severityHandshake' (TraceRecvMsg m) = severityHandshake'' m

severityHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
severityHandshake'' (AnyMessageAndAgency _agency msg) = severityHandshake''' msg

severityHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
severityHandshake''' HS.MsgProposeVersions {} = Info
severityHandshake''' HS.MsgAcceptVersion {}   = Info
severityHandshake''' HS.MsgRefuse {}          = Info

namesForHandshake :: NtN.HandshakeTr -> [Text]
namesForHandshake (WithMuxBearer _ e) = namesForHandshake' e

namesForHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> [Text]
namesForHandshake' (TraceSendMsg m) = namesForHandshake'' m
namesForHandshake' (TraceRecvMsg m) = namesForHandshake'' m

namesForHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForHandshake'' (AnyMessageAndAgency _agency msg) = namesForHandshake''' msg

namesForHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForHandshake''' HS.MsgRefuse {}          = ["Refuse"]

severityLocalHandshake :: NtC.HandshakeTr -> SeverityS
severityLocalHandshake (WithMuxBearer _ e) = severityLocalHandshake' e

severityLocalHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> SeverityS
severityLocalHandshake' (TraceSendMsg m) = severityLocalHandshake'' m
severityLocalHandshake' (TraceRecvMsg m) = severityLocalHandshake'' m

severityLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> SeverityS
severityLocalHandshake'' (AnyMessageAndAgency _agency msg) = severityLocalHandshake''' msg

severityLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> SeverityS
severityLocalHandshake''' HS.MsgProposeVersions {} = Info
severityLocalHandshake''' HS.MsgAcceptVersion {}   = Info
severityLocalHandshake''' HS.MsgRefuse {}          = Info

namesForLocalHandshake :: NtC.HandshakeTr -> [Text]
namesForLocalHandshake (WithMuxBearer _ e) = namesForLocalHandshake' e

namesForLocalHandshake' ::
     TraceSendRecv (HS.Handshake nt CBOR.Term)
  -> [Text]
namesForLocalHandshake' (TraceSendMsg m) = namesForLocalHandshake'' m
namesForLocalHandshake' (TraceRecvMsg m) = namesForLocalHandshake'' m

namesForLocalHandshake'' :: AnyMessageAndAgency (HS.Handshake nt CBOR.Term) -> [Text]
namesForLocalHandshake'' (AnyMessageAndAgency _agency msg) = namesForLocalHandshake''' msg

namesForLocalHandshake''' :: Message (HS.Handshake nt CBOR.Term) from to -> [Text]
namesForLocalHandshake''' HS.MsgProposeVersions {} = ["ProposeVersions"]
namesForLocalHandshake''' HS.MsgAcceptVersion {}   = ["AcceptVersion"]
namesForLocalHandshake''' HS.MsgRefuse {}          = ["Refuse"]

severityDiffusionInit :: ND.DiffusionInitializationTracer -> SeverityS
severityDiffusionInit ND.RunServer {}                         = Info
severityDiffusionInit ND.RunLocalServer {}                    = Info
severityDiffusionInit ND.UsingSystemdSocket {}                = Info
severityDiffusionInit ND.CreateSystemdSocketForSnocketPath {} = Info
severityDiffusionInit ND.CreatedLocalSocket {}                = Info
severityDiffusionInit ND.ConfiguringLocalSocket {}            = Info
severityDiffusionInit ND.ListeningLocalSocket {}              = Info
severityDiffusionInit ND.LocalSocketUp  {}                    = Info
severityDiffusionInit ND.CreatingServerSocket {}              = Info
severityDiffusionInit ND.ConfiguringServerSocket {}           = Info
severityDiffusionInit ND.ListeningServerSocket {}             = Info
severityDiffusionInit ND.ServerSocketUp {}                    = Info
severityDiffusionInit ND.UnsupportedLocalSystemdSocket {}     = Info
severityDiffusionInit ND.UnsupportedReadySocketCase {}        = Info
severityDiffusionInit ND.DiffusionErrored {}                  = Info

namesForDiffusionInit  :: ND.DiffusionInitializationTracer -> [Text]
namesForDiffusionInit  ND.RunServer {}                         =
  ["RunServer"]
namesForDiffusionInit  ND.RunLocalServer {}                    =
  ["RunLocalServer"]
namesForDiffusionInit  ND.UsingSystemdSocket {}                =
  ["UsingSystemdSocket"]
namesForDiffusionInit  ND.CreateSystemdSocketForSnocketPath {} =
  ["CreateSystemdSocketForSnocketPath"]
namesForDiffusionInit  ND.CreatedLocalSocket {}                =
  ["CreatedLocalSocket"]
namesForDiffusionInit  ND.ConfiguringLocalSocket {}            =
  ["ConfiguringLocalSocket"]
namesForDiffusionInit  ND.ListeningLocalSocket {}              =
  ["ListeningLocalSocket"]
namesForDiffusionInit  ND.LocalSocketUp  {}                    =
  ["LocalSocketUp"]
namesForDiffusionInit  ND.CreatingServerSocket {}              =
  ["CreatingServerSocket"]
namesForDiffusionInit  ND.ConfiguringServerSocket {}           =
  ["ConfiguringServerSocket"]
namesForDiffusionInit  ND.ListeningServerSocket {}             =
  ["ListeningServerSocket"]
namesForDiffusionInit  ND.ServerSocketUp {}                    =
  ["ServerSocketUp"]
namesForDiffusionInit  ND.UnsupportedLocalSystemdSocket {}     =
  ["UnsupportedLocalSystemdSocket"]
namesForDiffusionInit  ND.UnsupportedReadySocketCase {}        =
  ["UnsupportedReadySocketCase"]
namesForDiffusionInit  ND.DiffusionErrored {}                  =
  ["DiffusionErrored"]
