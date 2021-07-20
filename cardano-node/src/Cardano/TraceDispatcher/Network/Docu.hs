{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Network.Docu
  ( docTChainSync
  , docTTxSubmission
  , docTStateQuery
  , docTBlockFetch
  , docTTxSubmissionNode
  , docTTxSubmission2Node
  , docIPSubscription
  , docDNSSubscription
  , docDNSResolver
  , docErrorPolicy
  , docLocalErrorPolicy
  , docAcceptPolicy
  , docMux
  , docHandshake
  , docLocalHandshake
  , docDiffusionInit
  ) where

import           Cardano.Prelude
import qualified Codec.CBOR.Term as CBOR
import           Control.Monad.Class.MonadTime
import qualified Data.Map as Map
import           Data.Time.Clock (secondsToDiffTime)
import qualified Network.DNS as DNS
import           Network.Mux (MiniProtocolNum (..), MuxBearerState (..),
                     MuxTrace (..), WithMuxBearer (..))
import           Network.Mux.Types (MiniProtocolDir (..), MuxSDUHeader (..),
                     RemoteClockModel (..))
import qualified Network.Socket as Socket
import           Unsafe.Coerce
import           System.IO.Unsafe (unsafePerformIO)

import           Cardano.Logging
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)

import           Ouroboros.Network.Block (Point, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersion (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync (..),
                     Message (..))
import qualified Ouroboros.Network.Protocol.Handshake.Type as HS
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Message (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as TXS
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Snocket (FileDescriptor,
                     LocalAddress (..), socketFileDescriptor)
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (ConnectResult (..),
                     LocalAddresses (..), SubscriptionTrace (..))


protoHeader :: header
protoHeader = undefined

protoPoint :: Point blk
protoPoint = undefined

protoTip :: Tip blk
protoTip = undefined

protoPeer :: peer
protoPeer = unsafeCoerce (NtN.ConnectionId protoSockAddr protoSockAddr)

protoStok :: stok
protoStok = undefined

protoTx :: tx
protoTx = undefined

protoAcquireFailure :: LSQ.AcquireFailure
protoAcquireFailure = undefined

protoChainRange :: ChainRange point
protoChainRange = undefined

protoTokBlockingStyle :: TXS.TokBlockingStyle blocking
protoTokBlockingStyle = undefined

protoBlockingReplyList :: TXS.BlockingReplyList blocking (txid, TXS.TxSizeInBytes)
protoBlockingReplyList = undefined

protoTxId :: txid
protoTxId = undefined

protoLocalAdresses :: LocalAddresses addr
protoLocalAdresses = LocalAddresses Nothing Nothing Nothing

protoRes :: ConnectResult
protoRes = ConnectSuccess

protoDiffTime :: DiffTime
protoDiffTime = secondsToDiffTime 1

protoException :: NoMethodError
protoException = undefined

protoDomain :: DNS.Domain
protoDomain = "www.example.org"

protoDNSError :: DNS.DNSError
protoDNSError = undefined

protoLocalAdress :: LocalAddress
protoLocalAdress = LocalAddress "loopback"

protoMuxSDUHeader :: MuxSDUHeader
protoMuxSDUHeader = MuxSDUHeader {
      mhTimestamp = RemoteClockModel 1
    , mhNum       = MiniProtocolNum 1
    , mhDir       = InitiatorDir
    , mhLength    = 1
    }

protoTime :: Time
protoTime = undefined

protoMuxBearerState :: MuxBearerState
protoMuxBearerState = Mature

protoMiniProtocolNum :: MiniProtocolNum
protoMiniProtocolNum = MiniProtocolNum 1

protoMiniProtocolDir :: MiniProtocolDir
protoMiniProtocolDir = InitiatorDir

protoSomeException :: SomeException
protoSomeException = SomeException (AssertionFailed "just fooled")

protoNodeToClientVersion :: NodeToClientVersion
protoNodeToClientVersion = NodeToClientV_8

protoNodeToNodeVersion :: NtN.NodeToNodeVersion
protoNodeToNodeVersion = NtN.NodeToNodeV_1

protoCBORTerm :: CBOR.Term
protoCBORTerm = undefined

protoRefuseReason :: HS.RefuseReason NtN.NodeToNodeVersion
protoRefuseReason = HS.Refused protoNodeToNodeVersion "hello"

protoLocalRefuseReason :: HS.RefuseReason NtC.NodeToClientVersion
protoLocalRefuseReason = HS.Refused protoNodeToClientVersion "hello"

protoSockAddr :: Socket.SockAddr
protoSockAddr = Socket.SockAddrUnix "loopback"

protoLocalAddress :: LocalAddress
protoLocalAddress = LocalAddress "loopback"

protoFilePath :: FilePath
protoFilePath = "loopback"

protoFileDescriptor :: FileDescriptor
protoFileDescriptor =
  unsafePerformIO $ do
    sock <- Socket.mkSocket 111
    socketFileDescriptor sock



------------------------------------

docTChainSync :: Documented (BlockFetch.TraceLabelPeer peer (TraceSendRecv
    (ChainSync x (Point blk) (Tip blk))))
docTChainSync = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgRequestNext)))
        []
        "Request the next update from the producer. The response can be a roll\
        \forward, a roll back or wait."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Acknowledge the request but require the consumer to wait for the next\
        \update. This means that the consumer is synced with the producer, and\
        \the producer is waiting for its own chain state to change."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok MsgAwaitReply)))
        []
        "Tell the consumer to extend their chain with the given header.\
        \ \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgRollForward protoHeader protoTip))))
        []
        "Tell the consumer to extend their chain with the given header.\
        \ \
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgRollBackward protoPoint protoTip))))
        []
        "Tell the consumer to roll back to a given point on their chain.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgFindIntersect [protoPoint]))))
        []
        "Ask the producer to try to find an improved intersection point between\
        \the consumer and producer's chains. The consumer sends a sequence of\
        \points and it is up to the producer to find the first intersection point\
        \on its chain and send it back to the consumer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgIntersectFound protoPoint protoTip))))
        []
        "The reply to the consumer about an intersection found.\
        \The consumer can decide weather to send more points.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            (MsgIntersectNotFound protoTip))))
        []
        "The reply to the consumer that no intersection was found: none of the\
        \points the consumer supplied are on the producer chain.\
        \\
        \The message also tells the consumer about the head point of the producer."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok
            MsgDone)))
        []
        "We have to explain to the framework what our states mean, in terms of\
        \which party has agency in each state.\
        \ \
        \Idle states are where it is for the client to send a message,\
        \busy states are where the server is expected to send a reply."
  ]

docTTxSubmission :: Documented
   (BlockFetch.TraceLabelPeer
      localPeer
      (TraceSendRecv
         (LTS.LocalTxSubmission
            (GenTx blk) (ApplyTxErr blk))))
docTTxSubmission = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LTS.MsgSubmitTx protoTx))))
        []
        "The client submits a single transaction and waits a reply."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LTS.MsgAcceptTx)))
        []
        "The server can reply to inform the client that it has accepted the\
        \transaction."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LTS.MsgRejectTx undefined))))
        []
        "The server can reply to inform the client that it has rejected the\
        \transaction. A reason for the rejection is included."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LTS.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTStateQuery :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (LSQ.LocalStateQuery blk (Point blk) query)))
docTStateQuery = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok (LSQ.MsgAcquire Nothing))))
        []
        "The client requests that the state as of a particular recent point on\
        \the server's chain (within K of the tip) be made available to query,\
        \and waits for confirmation or failure.\
        \\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg (AnyMessageAndAgency protoStok LSQ.MsgAcquired)))
        []
        "The server can confirm that it has the state at the requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgFailure protoAcquireFailure))))
        []
        "The server can report that it cannot obtain the state for the\
        \requested point."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgQuery (undefined :: query result)))))
        []
        "The client can perform queries on the current acquired state."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgResult (undefined :: query result) (undefined :: result)))))
        []
        "The server must reply with the queries."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              LSQ.MsgRelease)))
        []
        "The client can instruct the server to release the state. This lets\
        \the server free resources."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (LSQ.MsgReAcquire Nothing))))
        []
        "This is like 'MsgAcquire' but for when the client already has a\
        \state. By moveing to another state directly without a 'MsgRelease' it\
        \enables optimisations on the server side (e.g. moving to the state for\
        \the immediate next block).\
        \\
        \Note that failure to re-acquire is equivalent to 'MsgRelease',\
        \rather than keeping the exiting acquired state.\
        \\
        \From 'NodeToClient_V8' onwards if the point is not specified, current tip\
        \will be acquired.  For previous versions of the protocol 'point' must be\
        \given."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              LSQ.MsgDone)))
        []
        "The client can terminate the protocol."
  ]

docTBlockFetch :: Documented
      (BlockFetch.TraceLabelPeer peer
       (TraceSendRecv
         (BlockFetch x (Point blk))))
docTBlockFetch = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgRequestRange protoChainRange))))
        []
        "Request range of blocks."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              MsgStartBatch)))
        []
        "Start block streaming."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              MsgNoBlocks)))
        []
        "Respond that there are no blocks."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgBlock undefined))))
        []
        "Stream a single block."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              MsgBatchDone)))
        []
        "End of block streaming."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              MsgClientDone)))
        []
        "Client termination message."
  ]

docTTxSubmissionNode :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission (GenTxId blk) (GenTx blk))))
docTTxSubmissionNode = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (TXS.MsgRequestTxIds protoTokBlockingStyle 1 1))))
        []
        "Request a non-empty list of transaction identifiers from the client,\
        \and confirm a number of outstanding transaction identifiers.\
        \\
        \With 'TokBlocking' this is a a blocking operation: the response will\
        \always have at least one transaction identifier, and it does not expect\
        \a prompt response: there is no timeout. This covers the case when there\
        \is nothing else to do but wait. For example this covers leaf nodes that\
        \rarely, if ever, create and submit a transaction.\
        \\
        \With 'TokNonBlocking' this is a non-blocking operation: the response\
        \may be an empty list and this does expect a prompt response. This\
        \covers high throughput use cases where we wish to pipeline, by\
        \interleaving requests for additional transaction identifiers with\
        \requests for transactions, which requires these requests not block.\
        \\
        \The request gives the maximum number of transaction identifiers that\
        \can be accepted in the response. This must be greater than zero in the\
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers\
        \acknowledged or the number requested must be non-zero. In either case,\
        \the number requested must not put the total outstanding over the fixed\
        \protocol limit.\
        \\
        \The request also gives the number of outstanding transaction\
        \identifiers that can now be acknowledged. The actual transactions\
        \to acknowledge are known to the peer based on the FIFO order in which\
        \they were provided.\
        \\
        \There is no choice about when to use the blocking case versus the\
        \non-blocking case, it depends on whether there are any remaining\
        \unacknowledged transactions (after taking into account the ones\
        \acknowledged in this message):\
        \\
        \* The blocking case must be used when there are zero remaining\
        \  unacknowledged transactions.\
        \\
        \* The non-blocking case must be used when there are non-zero remaining\
        \  unacknowledged transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (TXS.MsgReplyTxIds protoBlockingReplyList))))
        []
        "Reply with a list of transaction identifiers for available\
        \transactions, along with the size of each transaction.\
        \\
        \The list must not be longer than the maximum number requested.\
        \\
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while\
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty.\
        \\
        \These transactions are added to the notional FIFO of outstanding\
        \transaction identifiers for the protocol.\
        \\
        \The order in which these transaction identifiers are returned must be\
        \the order in which they are submitted to the mempool, to preserve\
        \dependent transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (TXS.MsgRequestTxs [protoTxId]))))
        []
        "Request one or more transactions corresponding to the given \
        \transaction identifiers. \
        \\
        \While it is the responsibility of the replying peer to keep within \
        \pipelining in-flight limits, the sender must also cooperate by keeping \
        \the total requested across all in-flight requests within the limits. \
        \\
        \It is an error to ask for transaction identifiers that were not \
        \previously announced (via 'MsgReplyTxIds'). \
        \\
        \It is an error to ask for transaction identifiers that are not \
        \outstanding or that were already asked for."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (TXS.MsgReplyTxs [protoTx]))))
        []
        "Reply with the requested transactions, or implicitly discard.\
        \\
        \Transactions can become invalid between the time the transaction \
        \identifier was sent and the transaction being requested. Invalid \
        \(including committed) transactions do not need to be sent.\
        \\
        \Any transaction identifiers requested but not provided in this reply \
        \should be considered as if this peer had never announced them. (Note \
        \that this is no guarantee that the transaction is invalid, it may still \
        \be valid and available from another peer)."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              TXS.MsgDone)))
        []
        "Termination message, initiated by the client when the server is \
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]

docTTxSubmission2Node :: Documented
  (BlockFetch.TraceLabelPeer peer
    (TraceSendRecv
      (TXS.TxSubmission2 (GenTxId blk) (GenTx blk))))
docTTxSubmission2Node = Documented [
      DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              MsgHello)))
        []
        "Client side hello message."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgTalk (TXS.MsgRequestTxIds protoTokBlockingStyle 1 1)))))
        []
        "Request a non-empty list of transaction identifiers from the client, \
        \and confirm a number of outstanding transaction identifiers. \
        \\
        \With 'TokBlocking' this is a a blocking operation: the response will \
        \always have at least one transaction identifier, and it does not expect \
        \a prompt response: there is no timeout. This covers the case when there \
        \is nothing else to do but wait. For example this covers leaf nodes that \
        \rarely, if ever, create and submit a transaction. \
        \\
        \With 'TokNonBlocking' this is a non-blocking operation: the response \
        \may be an empty list and this does expect a prompt response. This \
        \covers high throughput use cases where we wish to pipeline, by \
        \interleaving requests for additional transaction identifiers with \
        \requests for transactions, which requires these requests not block. \
        \\
        \The request gives the maximum number of transaction identifiers that \
        \can be accepted in the response. This must be greater than zero in the \
        \'TokBlocking' case. In the 'TokNonBlocking' case either the numbers \
        \acknowledged or the number requested must be non-zero. In either case, \
        \the number requested must not put the total outstanding over the fixed \
        \protocol limit. \
        \\
        \The request also gives the number of outstanding transaction \
        \identifiers that can now be acknowledged. The actual transactions \
        \to acknowledge are known to the peer based on the FIFO order in which \
        \they were provided. \
        \\
        \There is no choice about when to use the blocking case versus the \
        \non-blocking case, it depends on whether there are any remaining \
        \unacknowledged transactions (after taking into account the ones \
        \acknowledged in this message): \
        \\
        \* The blocking case must be used when there are zero remaining \
        \  unacknowledged transactions. \
        \\
        \* The non-blocking case must be used when there are non-zero remaining \
        \  unacknowledged transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgTalk (TXS.MsgReplyTxIds protoBlockingReplyList)))))
        []
        "Reply with a list of transaction identifiers for available\
        \transactions, along with the size of each transaction.\
        \\
        \The list must not be longer than the maximum number requested.\
        \\
        \In the 'StTxIds' 'StBlocking' state the list must be non-empty while\
        \in the 'StTxIds' 'StNonBlocking' state the list may be empty.\
        \\
        \These transactions are added to the notional FIFO of outstanding\
        \transaction identifiers for the protocol.\
        \\
        \The order in which these transaction identifiers are returned must be\
        \the order in which they are submitted to the mempool, to preserve\
        \dependent transactions."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgTalk (TXS.MsgRequestTxs [protoTxId])))))
        []
        "Request one or more transactions corresponding to the given \
        \transaction identifiers. \
        \\
        \While it is the responsibility of the replying peer to keep within\
        \pipelining in-flight limits, the sender must also cooperate by keeping\
        \the total requested across all in-flight requests within the limits.\
        \\
        \It is an error to ask for transaction identifiers that were not\
        \previously announced (via 'MsgReplyTxIds').\
        \\
        \It is an error to ask for transaction identifiers that are not\
        \outstanding or that were already asked for."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgTalk (TXS.MsgReplyTxs [protoTx])))))
        []
        "Reply with the requested transactions, or implicitly discard.\
        \\
        \Transactions can become invalid between the time the transaction\
        \identifier was sent and the transaction being requested. Invalid\
        \(including committed) transactions do not need to be sent.\
        \\
        \Any transaction identifiers requested but not provided in this reply\
        \should be considered as if this peer had never announced them. (Note\
        \that this is no guarantee that the transaction is invalid, it may still\
        \be valid and available from another peer)."
    , DocMsg
        (BlockFetch.TraceLabelPeer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (MsgTalk TXS.MsgDone))))
        []
        "Termination message, initiated by the client when the server is\
        \making a blocking call for more transaction identifiers."
  --TODO: Can't use 'MsgKThxBye' because NodeToNodeV_2 is not introduced yet.
  ]

docIPSubscription :: Documented (WithIPList (SubscriptionTrace Socket.SockAddr))
docIPSubscription = Documented $ map withIPList (undoc docSubscription)
  where
    withIPList (DocMsg v nl comment) =
      DocMsg (WithIPList protoLocalAdresses [] v) nl ("IP Subscription: " <> comment)

docDNSSubscription :: Documented (WithDomainName (SubscriptionTrace Socket.SockAddr))
docDNSSubscription = Documented $ map withDomainName (undoc docSubscription)
  where
    withDomainName (DocMsg v nl comment) =
      DocMsg (WithDomainName protoDomain v) nl ("DNS Subscription: " <> comment)

docSubscription :: Documented (SubscriptionTrace Socket.SockAddr)
docSubscription = Documented [
      DocMsg
        (SubscriptionTraceConnectStart protoSockAddr)
        []
        "Connection Attempt Start with destination."
    , DocMsg
        (SubscriptionTraceConnectEnd protoSockAddr protoRes)
        []
        "Connection Attempt end with destination and outcome."
    , DocMsg
        (SubscriptionTraceSocketAllocationException protoSockAddr protoException)
        []
        "Socket Allocation Exception with destination and the exception."
    , DocMsg
        (SubscriptionTraceConnectException protoSockAddr protoException)
        []
        "Connection Attempt Exception with destination and exception."
    , DocMsg
        (SubscriptionTraceTryConnectToPeer protoSockAddr)
        []
        "Trying to connect to peer with address."
    , DocMsg
        (SubscriptionTraceSkippingPeer protoSockAddr)
        []
        "Skipping peer with address."
    , DocMsg
        SubscriptionTraceSubscriptionRunning
        []
        "Required subscriptions started."
    , DocMsg
        (SubscriptionTraceSubscriptionWaiting 1)
        []
        "Waiting on address with active connections."
    , DocMsg
        SubscriptionTraceSubscriptionFailed
        []
        "Failed to start all required subscriptions."
    , DocMsg
        (SubscriptionTraceSubscriptionWaitingNewConnection protoDiffTime)
        []
        "Waiting delay time before attempting a new connection."
    , DocMsg
        (SubscriptionTraceStart 1)
        []
        "Starting Subscription Worker with a valency."
    , DocMsg
        (SubscriptionTraceRestart protoDiffTime 1 2)
        []
        "Restarting Subscription after duration with desired valency and\
        \ current valency."
    , DocMsg
        (SubscriptionTraceConnectionExist protoSockAddr)
        []
        "Connection exists to destination."
    , DocMsg
        (SubscriptionTraceUnsupportedRemoteAddr protoSockAddr)
        []
        "Unsupported remote target address."
    , DocMsg
        SubscriptionTraceMissingLocalAddress
        []
        "Missing local address."
    , DocMsg
        (SubscriptionTraceApplicationException protoSockAddr protoException)
        []
        "Application Exception occured."
    , DocMsg
        (SubscriptionTraceAllocateSocket protoSockAddr)
        []
        "Allocate socket to address."
    , DocMsg
        (SubscriptionTraceCloseSocket protoSockAddr)
        []
        "Closed socket to address."
  ]

docDNSResolver :: Documented (WithDomainName DnsTrace)
docDNSResolver = Documented [
      DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupException protoSomeException))
        []
        "A DNS lookup exception occured."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAError protoDNSError))
        []
        "A lookup failed with an error."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAAAAError protoDNSError))
        []
        "AAAA lookup failed with an error."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv4First)
        []
        "Returning IPv4 address first."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName protoDomain
          DnsTraceLookupIPv6First)
        []
        "Returning IPv6 address first."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAResult [protoSockAddr]))
        []
        "Lookup A result."
    , DocMsg
        (WithDomainName protoDomain
          (DnsTraceLookupAAAAResult [protoSockAddr]))
        []
        "Lookup AAAA result."
    ]

docErrorPolicy :: Documented (WithAddr Socket.SockAddr ErrorPolicyTrace)
docErrorPolicy = docErrorPolicy' protoSockAddr

docLocalErrorPolicy :: Documented (WithAddr LocalAddress ErrorPolicyTrace)
docLocalErrorPolicy = docErrorPolicy' protoLocalAdress

docErrorPolicy' :: adr -> Documented (WithAddr adr ErrorPolicyTrace)
docErrorPolicy' adr = Documented [
      DocMsg
        (WithAddr adr
          (ErrorPolicySuspendPeer Nothing protoDiffTime protoDiffTime))
        []
        "Suspending peer with a given exception."
    , DocMsg
        (WithAddr adr
          (ErrorPolicySuspendConsumer Nothing protoDiffTime))
        []
        "Suspending consumer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyLocalNodeError undefined))
        []
        "caught a local exception."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumePeer)
        []
        "Resume a peer (both consumer and producer)."
    , DocMsg
        (WithAddr adr
          ErrorPolicyKeepSuspended)
        []
        "Consumer was suspended until producer will resume."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeConsumer)
        []
        "Resume consumer."
    , DocMsg
        (WithAddr adr
          ErrorPolicyResumeProducer)
        []
        "Resume producer."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledApplicationException undefined))
        []
        "An application throwed an exception, which was not handled."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyUnhandledConnectionException undefined))
        []
        "'connect' throwed an exception, which was not handled by any\
        \ 'ErrorPolicy'."
    , DocMsg
        (WithAddr adr
          (ErrorPolicyAcceptException undefined))
        []
        "'accept' throwed an exception."
    ]

docAcceptPolicy :: Documented NtN.AcceptConnectionsPolicyTrace
docAcceptPolicy = Documented [
      DocMsg
        (NtN.ServerTraceAcceptConnectionRateLimiting protoDiffTime 2)
        []
        "Rate limiting accepting connections,\
        \ delaying next accept for given time, currently serving n connections."
      , DocMsg
        (NtN.ServerTraceAcceptConnectionHardLimit 2)
        []
        "Hard rate limit reached,\
        \ waiting until the number of connections drops below n."
  ]

docMux :: Documented (WithMuxBearer peer MuxTrace)
docMux = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          MuxTraceRecvHeaderStart)
        []
        "Bearer receive header start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvHeaderEnd protoMuxSDUHeader))
        []
        "Bearer receive header end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvDeltaQObservation protoMuxSDUHeader protoTime))
        []
        "Bearer DeltaQ observation."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvDeltaQSample 1.0 1 1 1.0 1.0 1.0 1.0 ""))
        []
        "Bearer DeltaQ sample."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvStart 1))
        []
        "Bearer receive start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceRecvEnd 1))
        []
        "Bearer receive end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceSendStart protoMuxSDUHeader))
        []
        "Bearer send start."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSendEnd)
        []
        "Bearer send end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceState protoMuxBearerState))
        []
        "State."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceCleanExit protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Miniprotocol terminated cleanly."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceExceptionExit
              protoMiniProtocolNum protoMiniProtocolDir protoSomeException))
        []
        "Miniprotocol terminated with exception."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelRecvStart protoMiniProtocolNum))
        []
        "Channel receive start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelRecvEnd protoMiniProtocolNum 1))
        []
        "Channel receive end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelSendStart protoMiniProtocolNum 1))
        []
        "Channel send start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceChannelSendEnd protoMiniProtocolNum))
        []
        "Channel send end."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceHandshakeStart)
        []
        "Handshake start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeClientEnd protoDiffTime))
        []
        "Handshake client end."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeClientError protoSomeException protoDiffTime))
        []
        "Handshake client error."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceHandshakeServerError protoSomeException))
        []
        "Handshake server error."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSDUReadTimeoutException)
        []
        "Timed out reading SDU."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceSDUWriteTimeoutException)
        []
        "Timed out writing SDU."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartEagerly protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Eagerly started."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartOnDemand protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Preparing to start."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceStartedOnDemand protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Started on demand."
    , DocMsg
        (WithMuxBearer protoPeer
          (MuxTraceTerminating protoMiniProtocolNum protoMiniProtocolDir))
        []
        "Terminating."
    , DocMsg
        (WithMuxBearer protoPeer
          MuxTraceShutdown)
        []
        "Mux shutdown."
  ]

docHandshake :: Documented NtN.HandshakeTr
docHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgProposeVersions Map.empty))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgAcceptVersion protoNodeToNodeVersion protoCBORTerm))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgRefuse protoRefuseReason))))
        []
        "It refuses to run any version."
    ]

docLocalHandshake :: Documented NtC.HandshakeTr
docLocalHandshake = Documented [
      DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgProposeVersions Map.empty))))
        []
        "Propose versions together with version parameters.  It must be\
        \ encoded to a sorted list.."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgAcceptVersion protoNodeToClientVersion protoCBORTerm))))
        []
        "The remote end decides which version to use and sends chosen version.\
        \The server is allowed to modify version parameters."
    , DocMsg
        (WithMuxBearer protoPeer
          (TraceSendMsg
            (AnyMessageAndAgency protoStok
              (HS.MsgRefuse protoLocalRefuseReason))))
        []
        "It refuses to run any version."
    ]

docDiffusionInit :: Documented ND.DiffusionInitializationTracer
docDiffusionInit = Documented [
    DocMsg
      (ND.RunServer protoSockAddr)
      []
      "RunServer TODO"
  , DocMsg
      (ND.RunLocalServer protoLocalAddress)
      []
      "RunLocalServer TODO"
  , DocMsg
      (ND.UsingSystemdSocket protoFilePath)
      []
      "UsingSystemdSocket TODO"
  , DocMsg
      (ND.CreateSystemdSocketForSnocketPath protoFilePath)
      []
      "CreateSystemdSocketForSnocketPath TODO"
  , DocMsg
      (ND.CreatedLocalSocket protoFilePath)
      []
      "CreatedLocalSocket TODO"
  , DocMsg
      (ND.ConfiguringLocalSocket protoFilePath protoFileDescriptor)
      []
      "ConfiguringLocalSocket TODO"
  , DocMsg
      (ND.ListeningLocalSocket protoFilePath protoFileDescriptor)
      []
      "ListeningLocalSocket TODO"
  , DocMsg
      (ND.LocalSocketUp protoFilePath protoFileDescriptor)
      []
      "LocalSocketUp TODO"
  , DocMsg
      (ND.CreatingServerSocket protoSockAddr)
      []
      "CreatingServerSocket TODO"
  , DocMsg
      (ND.ConfiguringServerSocket protoSockAddr)
      []
      "ConfiguringServerSocket TODO"
  , DocMsg
      (ND.ListeningServerSocket protoSockAddr)
      []
      "ListeningServerSocket TODO"
  , DocMsg
      (ND.ServerSocketUp protoSockAddr)
      []
      "ServerSocketUp TODO"
  , DocMsg
      (ND.UnsupportedLocalSystemdSocket protoSockAddr)
      []
      "UnsupportedLocalSystemdSocket TODO"
  , DocMsg
      ND.UnsupportedReadySocketCase
      []
      "UnsupportedReadySocketCase TODO"
  , DocMsg
      (ND.DiffusionErrored protoSomeException)
      []
      "DiffusionErrored TODO"
  ]
