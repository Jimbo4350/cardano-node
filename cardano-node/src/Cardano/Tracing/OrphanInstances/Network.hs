{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Tracing.OrphanInstances.Network () where

import           Cardano.Prelude hiding (group, show)
import           Prelude (String, show, id)

import           Control.Monad.Class.MonadTime (DiffTime, Time (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.IP as IP
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import           Data.Text (pack)

import           Network.TypedProtocol.Core (ClientHasAgency,
                     PeerHasAgency (..), ServerHasAgency)


import           Network.Mux (MuxTrace (..), WithMuxBearer (..), MiniProtocolNum (..))
import qualified Network.Socket as Socket (SockAddr (..))

import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.Render
import           Cardano.Node.Configuration.TopologyP2P (UseLedger (..))

import           Ouroboros.Consensus.Block (ConvertRawHash (..), getHeader)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxs (..), txId)
import           Ouroboros.Consensus.Node.Run (RunNode, estimateBlockSize)
import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch.ClientState (TraceFetchClientState,
                     TraceLabelPeer (..))
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision (FetchDecision, FetchDecline (..))
import           Ouroboros.Network.ConnectionId (ConnectionId (..))
import           Ouroboros.Network.ConnectionManager.Types ( AbstractState(..),
                    ConnectionManagerTrace(..) , ConnectionManagerCounters(..),
                    AbstractState(..) , ConnectionManagerCounters(..) , OperationResult(..))
import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace(..))
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.DeltaQ (GSV (..), PeerGSV (..))
import           Ouroboros.Network.Driver.Limits (ProtocolLimitFailure (..))
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import           Ouroboros.Network.NodeToClient (NodeToClientVersionData(..), NodeToClientVersion)
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..), TraceSendRecv (..),
                     WithAddr (..), NodeToNodeVersionData (..), NodeToNodeVersion,
                     RemoteAddress)
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.PeerSelection.Governor ( PeerSelectionState (..),
                     PeerSelectionTargets (..), DebugPeerSelection (..),
                     TracePeerSelection(..), PeerSelectionCounters(..))
import qualified Ouroboros.Network.PeerSelection.KnownPeers as KnownPeers
import           Ouroboros.Network.PeerSelection.LedgerPeers
import           Ouroboros.Network.PeerSelection.LocalRootPeers (toMap, toGroupSets, LocalRootPeers)
import qualified Ouroboros.Network.PeerSelection.EstablishedPeers as EstablishedPeers
import           Ouroboros.Network.PeerSelection.RootPeersDNS ( TraceLocalRootPeers(..),
                     TracePublicRootPeers(..))
import           Ouroboros.Network.PeerSelection.PeerStateActions
                     ( PeerSelectionActionsTrace(..)
                     )
import           Ouroboros.Network.PeerSelection.Types (PeerStatus (..))
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch, Message (..))
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import qualified Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import           Ouroboros.Network.Protocol.Trans.Hello.Type (Hello)
import qualified Ouroboros.Network.Protocol.Trans.Hello.Type as Hello
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (LocalTxSubmission)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LocalTxSub
import           Ouroboros.Network.Protocol.TxSubmission.Type (Message (..), TxSubmission)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription (ConnectResult (..), DnsTrace (..),
                     SubscriberError (..), SubscriptionTrace (..), WithDomainName (..),
                     WithIPList (..))
import           Ouroboros.Network.TxSubmission.Inbound (TraceTxSubmissionInbound (..), ProcessedTxCount(..))
import           Ouroboros.Network.TxSubmission.Outbound (TraceTxSubmissionOutbound (..))
import Ouroboros.Network.Server2 ( ServerTrace, ServerTrace(..) )
import           Ouroboros.Network.Magic (NetworkMagic(..))
import qualified Ouroboros.Network.Server2 as Server
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace(..),
                     RemoteSt(..))
import qualified Ouroboros.Network.InboundGovernor as InboundGovernor
import           Ouroboros.Network.InboundGovernor.State (InboundGovernorCounters(..))
import           Ouroboros.Network.Protocol.Handshake (HandshakeException(..),
                   HandshakeProtocolError (..), RefuseReason (..))
import           Ouroboros.Network.RethrowPolicy (ErrorCommand (..))

import qualified Ouroboros.Network.Diffusion as ND

{- HLINT ignore "Use record patterns" -}

--
-- * instances of @HasPrivacyAnnotation@ and @HasSeverityAnnotation@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance HasPrivacyAnnotation (ND.InitializationTracer ntnAddr ntcAddr)
instance HasSeverityAnnotation (ND.InitializationTracer ntnAddr ntcAddr) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (NtC.HandshakeTr LocalAddress NodeToClientVersion)
instance HasSeverityAnnotation (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (NtN.HandshakeTr RemoteAddress NodeToNodeVersion)
instance HasSeverityAnnotation (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation NtN.AcceptConnectionsPolicyTrace
instance HasSeverityAnnotation NtN.AcceptConnectionsPolicyTrace where
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionRateLimiting {} = Info
  getSeverityAnnotation NtN.ServerTraceAcceptConnectionHardLimit {} = Warning


instance HasPrivacyAnnotation (TraceFetchClientState header)
instance HasSeverityAnnotation (TraceFetchClientState header) where
  getSeverityAnnotation BlockFetch.AddedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.AcknowledgedFetchRequest {} = Info
  getSeverityAnnotation BlockFetch.StartedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.CompletedBlockFetch {} = Info
  getSeverityAnnotation BlockFetch.CompletedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.RejectedFetchBatch {} = Info
  getSeverityAnnotation BlockFetch.ClientTerminating {} = Notice


instance HasPrivacyAnnotation (TraceSendRecv a)
instance HasSeverityAnnotation (TraceSendRecv a) where
  getSeverityAnnotation _ = Debug


instance HasPrivacyAnnotation a => HasPrivacyAnnotation (TraceLabelPeer peer a)
instance HasSeverityAnnotation a => HasSeverityAnnotation (TraceLabelPeer peer a) where
  getSeverityAnnotation (TraceLabelPeer _p a) = getSeverityAnnotation a


instance HasPrivacyAnnotation [TraceLabelPeer peer (FetchDecision [Point header])]
instance HasSeverityAnnotation [TraceLabelPeer peer (FetchDecision [Point header])] where
  getSeverityAnnotation [] = Debug
  getSeverityAnnotation xs =
      maximum $ map (\(TraceLabelPeer _ a) -> fetchDecisionSeverity a) xs
    where
      fetchDecisionSeverity :: FetchDecision a -> Severity
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


instance HasPrivacyAnnotation (TraceTxSubmissionInbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionInbound txid tx) where
  getSeverityAnnotation TraceTxSubmissionCollected {} = Debug
  getSeverityAnnotation TraceTxSubmissionProcessed {} = Debug
  getSeverityAnnotation TraceTxInboundTerminated = Notice
  getSeverityAnnotation TraceTxInboundCannotRequestMoreTxs {} = Debug
  getSeverityAnnotation TraceTxInboundCanRequestMoreTxs {} = Debug


instance HasPrivacyAnnotation (TraceTxSubmissionOutbound txid tx)
instance HasSeverityAnnotation (TraceTxSubmissionOutbound txid tx) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation (TraceKeepAliveClient remotePeer)
instance HasSeverityAnnotation (TraceKeepAliveClient remotePeer) where
  getSeverityAnnotation _ = Info


instance HasPrivacyAnnotation TraceLedgerPeers
instance HasSeverityAnnotation TraceLedgerPeers where
  getSeverityAnnotation ev =
    case ev of
      PickedPeer {}                  -> Debug
      PickedPeers {}                 -> Info
      FetchingNewLedgerState {}      -> Info
      DisabledLedgerPeers {}         -> Info
      TraceUseLedgerAfter {}         -> Info
      WaitingOnRequest {}            -> Debug
      RequestForPeers {}             -> Debug
      ReusingLedgerState {}          -> Debug
      FallingBackToBootstrapPeers {} -> Info


instance HasPrivacyAnnotation (WithAddr addr ErrorPolicyTrace)
instance HasSeverityAnnotation (WithAddr addr ErrorPolicyTrace) where
  getSeverityAnnotation (WithAddr _ ev) = case ev of
    ErrorPolicySuspendPeer {} -> Warning -- peer misbehaved
    ErrorPolicySuspendConsumer {} -> Notice -- peer temporarily not useful
    ErrorPolicyLocalNodeError {} -> Error
    ErrorPolicyResumePeer {} -> Debug
    ErrorPolicyKeepSuspended {} -> Debug
    ErrorPolicyResumeConsumer {} -> Debug
    ErrorPolicyResumeProducer {} -> Debug
    ErrorPolicyUnhandledApplicationException {} -> Error
    ErrorPolicyUnhandledConnectionException {} -> Error
    ErrorPolicyAcceptException {} -> Error


instance HasPrivacyAnnotation (WithDomainName DnsTrace)
instance HasSeverityAnnotation (WithDomainName DnsTrace) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    DnsTraceLookupException {} -> Error
    DnsTraceLookupAError {} -> Error
    DnsTraceLookupAAAAError {} -> Error
    DnsTraceLookupIPv6First -> Debug
    DnsTraceLookupIPv4First -> Debug
    DnsTraceLookupAResult {} -> Debug
    DnsTraceLookupAAAAResult {} -> Debug


instance HasPrivacyAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr))
instance HasSeverityAnnotation (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  getSeverityAnnotation (WithDomainName _ ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
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
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance HasPrivacyAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr))
instance HasSeverityAnnotation (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  getSeverityAnnotation (WithIPList _ _ ev) = case ev of
    SubscriptionTraceConnectStart _ -> Info
    SubscriptionTraceConnectEnd _ connectResult -> case connectResult of
      ConnectSuccess -> Info
      ConnectSuccessLast -> Notice
      ConnectValencyExceeded -> Warning
    SubscriptionTraceConnectException _ e ->
        case fromException $ SomeException e of
             Just (_::SubscriberError) -> Debug
             Nothing -> Error
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
             Nothing -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Info


instance HasPrivacyAnnotation (Identity (SubscriptionTrace LocalAddress))
instance HasSeverityAnnotation (Identity (SubscriptionTrace LocalAddress)) where
  getSeverityAnnotation (Identity ev) = case ev of
    SubscriptionTraceConnectStart {} -> Notice
    SubscriptionTraceConnectEnd {} -> Notice
    SubscriptionTraceConnectException {} -> Error
    SubscriptionTraceSocketAllocationException {} -> Error
    SubscriptionTraceTryConnectToPeer {} -> Notice
    SubscriptionTraceSkippingPeer {} -> Info
    SubscriptionTraceSubscriptionRunning -> Notice
    SubscriptionTraceSubscriptionWaiting {} -> Debug
    SubscriptionTraceSubscriptionFailed -> Warning
    SubscriptionTraceSubscriptionWaitingNewConnection {} -> Debug
    SubscriptionTraceStart {} -> Notice
    SubscriptionTraceRestart {} -> Notice
    SubscriptionTraceConnectionExist {} -> Debug
    SubscriptionTraceUnsupportedRemoteAddr {} -> Warning
    SubscriptionTraceMissingLocalAddress -> Warning
    SubscriptionTraceApplicationException {} -> Error
    SubscriptionTraceAllocateSocket {} -> Debug
    SubscriptionTraceCloseSocket {} -> Debug


instance Transformable Text IO (Identity (SubscriptionTrace LocalAddress)) where
  trTransformer = trStructuredText
instance HasTextFormatter (Identity (SubscriptionTrace LocalAddress)) where
  formatText a _ = pack (show a)


instance ToObject (Identity (SubscriptionTrace LocalAddress)) where
  toObject _verb (Identity ev) =
    mkObject [ "kind" .= ("SubscriptionTrace" :: String)
             , "event" .= show ev
             ]


instance HasPrivacyAnnotation (WithMuxBearer peer MuxTrace)
instance HasSeverityAnnotation (WithMuxBearer peer MuxTrace) where
  getSeverityAnnotation (WithMuxBearer _ ev) = case ev of
    MuxTraceRecvHeaderStart -> Debug
    MuxTraceRecvHeaderEnd {} -> Debug
    MuxTraceRecvStart {} -> Debug
    MuxTraceRecvEnd {} -> Debug
    MuxTraceSendStart {} -> Debug
    MuxTraceSendEnd -> Debug
    MuxTraceState {} -> Info
    MuxTraceCleanExit {} -> Notice
    MuxTraceExceptionExit {} -> Notice
    MuxTraceChannelRecvStart {} -> Debug
    MuxTraceChannelRecvEnd {} -> Debug
    MuxTraceChannelSendStart {} -> Debug
    MuxTraceChannelSendEnd {} -> Debug
    MuxTraceHandshakeStart -> Debug
    MuxTraceHandshakeClientEnd {} -> Info
    MuxTraceHandshakeServerEnd -> Debug
    MuxTraceHandshakeClientError {} -> Error
    MuxTraceHandshakeServerError {} -> Error
    MuxTraceRecvDeltaQObservation {} -> Debug
    MuxTraceRecvDeltaQSample {} -> Debug
    MuxTraceSDUReadTimeoutException -> Notice
    MuxTraceSDUWriteTimeoutException -> Notice
    MuxTraceStartEagerly _ _ -> Info
    MuxTraceStartOnDemand _ _ -> Info
    MuxTraceStartedOnDemand _ _ -> Info
    MuxTraceShutdown -> Debug
    MuxTraceTerminating {} -> Debug

instance HasPrivacyAnnotation (TraceLocalRootPeers RemoteAddress exception)
instance HasSeverityAnnotation (TraceLocalRootPeers RemoteAddress exception) where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation TracePublicRootPeers
instance HasSeverityAnnotation TracePublicRootPeers where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (TracePeerSelection addr)
instance HasSeverityAnnotation (TracePeerSelection addr) where
  getSeverityAnnotation ev =
    case ev of
      TraceLocalRootPeersChanged {} -> Notice
      TraceTargetsChanged        {} -> Notice
      TracePublicRootsRequest    {} -> Info
      TracePublicRootsResults    {} -> Info
      TracePublicRootsFailure    {} -> Error
      TraceGossipRequests        {} -> Debug
      TraceGossipResults         {} -> Debug
      TraceForgetColdPeers       {} -> Info
      TracePromoteColdPeers      {} -> Info
      TracePromoteColdLocalPeers {} -> Info
      TracePromoteColdFailed     {} -> Error
      TracePromoteColdDone       {} -> Info
      TracePromoteWarmPeers      {} -> Info
      TracePromoteWarmLocalPeers {} -> Info
      TracePromoteWarmFailed     {} -> Error
      TracePromoteWarmDone       {} -> Info
      TraceDemoteWarmPeers       {} -> Info
      TraceDemoteWarmFailed      {} -> Error
      TraceDemoteWarmDone        {} -> Info
      TraceDemoteHotPeers        {} -> Info
      TraceDemoteLocalHotPeers   {} -> Info
      TraceDemoteHotFailed       {} -> Error
      TraceDemoteHotDone         {} -> Info
      TraceDemoteAsynchronous    {} -> Info
      TraceGovernorWakeup        {} -> Info
      TraceChurnWait             {} -> Info
      TraceChurnMode             {} -> Info

instance HasPrivacyAnnotation (DebugPeerSelection addr conn)
instance HasSeverityAnnotation (DebugPeerSelection addr conn) where
  getSeverityAnnotation _ = Debug

instance HasPrivacyAnnotation (PeerSelectionActionsTrace Socket.SockAddr)
instance HasSeverityAnnotation (PeerSelectionActionsTrace Socket.SockAddr) where
  getSeverityAnnotation ev =
   case ev of
     PeerStatusChanged {}       -> Info
     PeerStatusChangeFailure {} -> Error
     PeerMonitoringError {}     -> Error
     PeerMonitoringResult {}    -> Debug

instance HasPrivacyAnnotation PeerSelectionCounters
instance HasSeverityAnnotation PeerSelectionCounters where
  getSeverityAnnotation _ = Info

instance HasPrivacyAnnotation (ConnectionManagerTrace addr connTrace)
instance HasSeverityAnnotation (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  getSeverityAnnotation ev =
    case ev of
      TrIncludeConnection {}        -> Debug
      TrUnregisterConnection {}     -> Debug
      TrConnect {}                  -> Debug
      TrConnectError {}             -> Info
      TrTerminatingConnection {}    -> Debug
      TrTerminatedConnection {}     -> Debug
      TrConnectionHandler _ ev'     ->
        case ev' of
          TrHandshakeSuccess {}     -> Info
          TrHandshakeClientError {} -> Error
          TrHandshakeServerError {} -> Info
          TrError _ _ ShutdownNode  -> Critical
          TrError _ _ ShutdownPeer  -> Info

      TrShutdown                    -> Info
      TrConnectionExists {}         -> Info
      TrForbiddenConnection {}      -> Info
      TrImpossibleConnection {}     -> Info
      TrConnectionFailure {}        -> Info
      TrConnectionNotFound {}       -> Debug
      TrForbiddenOperation {}       -> Info

      TrPruneConnections {}         -> Notice
      TrConnectionCleanup {}        -> Info
      TrConnectionTimeWait {}       -> Info
      TrConnectionTimeWaitDone {}   -> Info
      TrConnectionManagerCounters {} -> Info
      TrState {}                    -> Info

instance HasPrivacyAnnotation (ServerTrace addr)
instance HasSeverityAnnotation (ServerTrace addr) where
  getSeverityAnnotation ev =
    case ev of
      Server.TrAcceptConnection {}                      -> Debug
      Server.TrAcceptError {}                           -> Error
      Server.TrAcceptPolicyTrace {}                     -> Notice
      Server.TrServerStarted {}                         -> Notice
      Server.TrServerStopped {}                         -> Notice
      Server.TrServerError {}                           -> Critical

instance HasPrivacyAnnotation (InboundGovernorTrace addr)
instance HasSeverityAnnotation (InboundGovernorTrace addr) where
  getSeverityAnnotation ev =
    case ev of
      InboundGovernor.TrNewConnection {}           -> Debug
      InboundGovernor.TrResponderRestarted {}      -> Debug
      InboundGovernor.TrResponderStartFailure {}   -> Error
      InboundGovernor.TrResponderErrored {}        -> Info
      InboundGovernor.TrResponderStarted {}        -> Debug
      InboundGovernor.TrResponderTerminated {}     -> Debug
      InboundGovernor.TrPromotedToWarmRemote {}    -> Info
      InboundGovernor.TrPromotedToHotRemote {}     -> Info
      InboundGovernor.TrDemotedToColdRemote {}     -> Info
      InboundGovernor.TrWaitIdleRemote {}          -> Debug
      InboundGovernor.TrMuxCleanExit {}            -> Debug
      InboundGovernor.TrMuxErrored {}              -> Info
      InboundGovernor.TrInboundGovernorCounters {} -> Info
      InboundGovernor.TrRemoteState {}             -> Debug

--
-- | instances of @Transformable@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance Transformable Text IO (ND.InitializationTracer RemoteAddress LocalAddress) where
  trTransformer = trStructuredText
instance HasTextFormatter (ND.InitializationTracer RemoteAddress LocalAddress) where
  formatText a _ = pack (show a)

instance Transformable Text IO (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  trTransformer = trStructuredText
instance HasTextFormatter (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  formatText a _ = pack (show a)


instance Transformable Text IO (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  trTransformer = trStructuredText
instance HasTextFormatter (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  formatText a _ = pack (show a)


instance Transformable Text IO NtN.AcceptConnectionsPolicyTrace where
  trTransformer = trStructuredText
instance HasTextFormatter NtN.AcceptConnectionsPolicyTrace where
  formatText a _ = pack (show a)


instance (StandardHash header, Show peer)
      => Transformable Text IO [TraceLabelPeer peer (FetchDecision [Point header])] where
  trTransformer = trStructuredText
instance (StandardHash header, Show peer)
      => HasTextFormatter [TraceLabelPeer peer (FetchDecision [Point header])] where
  formatText a _ = pack (show a)


instance (Show peer, Show a, HasPrivacyAnnotation a, HasSeverityAnnotation a, ToObject a)
      => Transformable Text IO (TraceLabelPeer peer a) where
  trTransformer = trStructuredText
instance (Show peer, Show a)
      => HasTextFormatter (TraceLabelPeer peer a) where
  formatText a _ = pack (show a)


instance Transformable Text IO (TraceTxSubmissionInbound txid tx) where
  trTransformer = trStructuredText
instance HasTextFormatter (TraceTxSubmissionInbound txid tx) where
  formatText a _ = pack (show a)


instance (Show tx, Show txid)
      => Transformable Text IO (TraceTxSubmissionOutbound txid tx) where
  trTransformer = trStructuredText
instance (Show tx, Show txid)
      => HasTextFormatter (TraceTxSubmissionOutbound txid tx) where
  formatText a _ = pack (show a)


instance Show addr
    => Transformable Text IO (TraceKeepAliveClient addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (TraceKeepAliveClient addr) where
    formatText a _ = pack (show a)


instance Transformable Text IO TraceLedgerPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TraceLedgerPeers where
  formatText _ = pack . show . toList


instance Show addr => Transformable Text IO (WithAddr addr ErrorPolicyTrace) where
  trTransformer = trStructuredText
instance Show addr => HasTextFormatter (WithAddr addr ErrorPolicyTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithDomainName DnsTrace) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithDomainName DnsTrace) where
  formatText a _ = pack (show a)


instance Transformable Text IO (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  trTransformer = trStructuredText
instance HasTextFormatter (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  formatText a _ = pack (show a)


instance (Show peer, ToObject peer)
      => Transformable Text IO (WithMuxBearer peer MuxTrace) where
  trTransformer = trStructuredText
instance (Show peer)
      => HasTextFormatter (WithMuxBearer peer MuxTrace) where
  formatText (WithMuxBearer peer ev) = \_o ->
        "Bearer on " <> pack (show peer)
     <> " event: " <> pack (show ev)


instance Show exception => Transformable Text IO (TraceLocalRootPeers RemoteAddress exception) where
  trTransformer = trStructuredText
instance Show exception => HasTextFormatter (TraceLocalRootPeers RemoteAddress exception) where
    formatText a _ = pack (show a)

instance Transformable Text IO TracePublicRootPeers where
  trTransformer = trStructuredText
instance HasTextFormatter TracePublicRootPeers where
  formatText a _ = pack (show a)

instance Transformable Text IO (TracePeerSelection Socket.SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (TracePeerSelection Socket.SockAddr) where
  formatText a _ = pack (show a)

instance Show conn
      => Transformable Text IO (DebugPeerSelection Socket.SockAddr conn) where
  trTransformer = trStructuredText
instance HasTextFormatter (DebugPeerSelection Socket.SockAddr conn) where
  -- One can only change what is logged with respect to verbosity using json
  -- format.
  formatText _ obj = pack (show obj)

instance Transformable Text IO (PeerSelectionActionsTrace Socket.SockAddr) where
  trTransformer = trStructuredText
instance HasTextFormatter (PeerSelectionActionsTrace Socket.SockAddr) where
  formatText a _ = pack (show a)

instance Transformable Text IO PeerSelectionCounters where
  trTransformer = trStructuredText
instance HasTextFormatter PeerSelectionCounters where
  formatText a _ = pack (show a)

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions
         )
      => Transformable Text IO (ConnectionManagerTrace
                                 addr
                                 (ConnectionHandlerTrace versionNumber agreedOptions)) where
  trTransformer = trStructuredText
instance (Show addr, Show versionNumber, Show agreedOptions)
      => HasTextFormatter (ConnectionManagerTrace
                            addr
                            (ConnectionHandlerTrace versionNumber agreedOptions)) where
  formatText a _ = pack (show a)

instance (Show addr, ToObject addr, ToJSON addr)
      => Transformable Text IO (ServerTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (ServerTrace addr) where
  formatText a _ = pack (show a)

instance (ToJSON addr, Show addr)
      => Transformable Text IO (InboundGovernorTrace addr) where
  trTransformer = trStructuredText
instance Show addr
      => HasTextFormatter (InboundGovernorTrace addr) where
  formatText a _ = pack (show a)

--
-- | instances of @ToObject@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( ConvertTxId blk
         , RunNode blk
         , HasTxs blk
         )
      => ToObject (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  toObject MinimalVerbosity (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  toObject verb (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForVerbosity verb . txId

  toObject _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ( ToObject (AnyMessageAndAgency ps)
         , forall (st :: ps). Show (ClientHasAgency st)
         , forall (st :: ps). Show (ServerHasAgency st)
         )
      => ToObject (AnyMessageAndAgency (Hello ps stIdle)) where
  toObject verb (AnyMessageAndAgency stok msg) =
    case (stok, msg) of
      (_, Hello.MsgHello) ->
        mkObject [ "kind" .= String "MsgHello"
                 , "agency" .= String (pack $ show stok)
                 ]
      ( ClientAgency (Hello.TokClientTalk tok)
        , Hello.MsgTalk msg' ) ->
        mkObject [ "kind" .= String "MsgTalk"
                 , "message" .=
                     toObject verb
                       (AnyMessageAndAgency (ClientAgency tok) msg')
                 ]
      ( ServerAgency (Hello.TokServerTalk tok)
        , Hello.MsgTalk msg' ) ->
        mkObject [ "kind" .= String "MsgTalk"
                 , "message" .=
                     toObject verb
                       (AnyMessageAndAgency (ServerAgency tok) msg')
                 ]

instance (forall result. Show (query result))
      => ToObject (AnyMessageAndAgency (LocalStateQuery blk pt query)) where
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalStateQuery.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (LocalTxSubmission tx err)) where
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  toObject _verb (AnyMessageAndAgency stok LocalTxSub.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ToObject (AnyMessageAndAgency (ChainSync blk pt tip)) where
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   toObject _verb (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance (Show txid, Show tx)
      => ToObject (AnyMessageAndAgency (TxSubmission txid tx)) where
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok (MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  toObject _verb (AnyMessageAndAgency stok MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance ToJSON peerAddr => ToJSON (ConnectionId peerAddr) where
  toJSON ConnectionId { localAddress, remoteAddress } =
    Aeson.object [ "localAddress"  .= toJSON localAddress
                 , "remoteAddress" .= toJSON remoteAddress
                 ]

instance ToJSON peerAddr => ToObject (ConnectionId peerAddr) where
  toObject _verb connId =
    mkObject
      [ "kind" .= String "ConnectionId"
      , "connectionId" .= toJSON connId
      ]

instance Aeson.ToJSON ConnectionManagerCounters where
  toJSON ConnectionManagerCounters { prunableConns
                                   , duplexConns
                                   , uniConns
                                   , incomingConns
                                   , outgoingConns
                                   } =
    Aeson.object [ "kind"           .= String "ConnectionManagerCounters"
                 , "prunable"       .= toJSON prunableConns
                 , "duplex"         .= toJSON duplexConns
                 , "unidirectional" .= toJSON uniConns
                 , "incoming"       .= incomingConns
                 , "outgoing"       .= outgoingConns
                 ]

instance ToObject (FetchDecision [Point header]) where
  toObject _verb (Left decline) =
    mkObject [ "kind" .= String "FetchDecision declined"
             , "declined" .= String (pack (show decline))
             ]
  toObject _verb (Right results) =
    mkObject [ "kind" .= String "FetchDecision results"
             , "length" .= String (pack $ show $ length results)
             ]

-- TODO: use 'ToJSON' constraints
instance (Show ntnAddr, Show ntcAddr) => ToObject (ND.InitializationTracer ntnAddr ntcAddr) where
  toObject _verb (ND.RunServer sockAddr) = mkObject
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  toObject _verb (ND.RunLocalServer localAddress) = mkObject
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  toObject _verb (ND.UsingSystemdSocket localAddress) = mkObject
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack . show $ localAddress)
    ]

  toObject _verb (ND.CreateSystemdSocketForSnocketPath localAddress) = mkObject
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (ND.CreatedLocalSocket localAddress) = mkObject
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    ]
  toObject _verb (ND.ConfiguringLocalSocket localAddress socket) = mkObject
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningLocalSocket localAddress socket) = mkObject
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.LocalSocketUp localAddress fd) = mkObject
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack . show $ localAddress)
    , "socket" .= String (pack (show fd))
    ]
  toObject _verb (ND.CreatingServerSocket socket) = mkObject
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ListeningServerSocket socket) = mkObject
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ServerSocketUp socket) = mkObject
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.ConfiguringServerSocket socket) = mkObject
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  toObject _verb (ND.UnsupportedLocalSystemdSocket path) = mkObject
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  toObject _verb ND.UnsupportedReadySocketCase = mkObject
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  toObject _verb (ND.DiffusionErrored exception) = mkObject
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]

instance ToObject (NtC.HandshakeTr LocalAddress NodeToClientVersion) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject (NtN.HandshakeTr RemoteAddress NodeToNodeVersion) where
  toObject _verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]


instance ToObject LocalAddress where
    toObject _verb addr =
      mkObject [ "kind" .= String "LocalAddress"
               , "path" .= toJSON addr
               ]

instance ToJSON LocalAddress where
    toJSON (LocalAddress path) = String (pack path)

instance Aeson.ToJSONKey LocalAddress where

instance ToObject NtN.AcceptConnectionsPolicyTrace where
  toObject _verb (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
             , "delay" .= show delay
             , "numberOfConnection" .= show numOfConnections
             ]
  toObject _verb (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
    mkObject [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
             , "softLimit" .= show softLimit
             ]


instance ConvertRawHash blk
      => ToObject (Point blk) where
  toObject _verb GenesisPoint =
    mkObject
      [ "kind" .= String "GenesisPoint" ]
  toObject verb (BlockPoint slot h) =
    mkObject
      [ "kind" .= String "BlockPoint"
      , "slot" .= toJSON (unSlotNo slot)
      , "headerHash" .= renderHeaderHashForVerbosity (Proxy @blk) verb h
      ]


instance ToObject SlotNo where
  toObject _verb slot =
    mkObject [ "kind" .= String "SlotNo"
             , "slot" .= toJSON (unSlotNo slot) ]


instance ToObject (TraceFetchClientState header) where
  toObject _verb BlockFetch.AddedFetchRequest {} =
    mkObject [ "kind" .= String "AddedFetchRequest" ]
  toObject _verb BlockFetch.AcknowledgedFetchRequest {} =
    mkObject [ "kind" .= String "AcknowledgedFetchRequest" ]
  toObject _verb BlockFetch.CompletedBlockFetch {} =
    mkObject [ "kind" .= String "CompletedBlockFetch" ]
  toObject _verb BlockFetch.CompletedFetchBatch {} =
    mkObject [ "kind" .= String "CompletedFetchBatch" ]
  toObject _verb BlockFetch.StartedFetchBatch {} =
    mkObject [ "kind" .= String "StartedFetchBatch" ]
  toObject _verb BlockFetch.RejectedFetchBatch {} =
    mkObject [ "kind" .= String "RejectedFetchBatch" ]
  toObject _verb (BlockFetch.ClientTerminating outstanding) =
    mkObject [ "kind" .= String "ClientTerminating"
             , "outstanding" .= outstanding
             ]


instance Show peer
      => ToObject [TraceLabelPeer peer (FetchDecision [Point header])] where
  toObject MinimalVerbosity _ = emptyObject
  toObject _ [] = emptyObject
  toObject _ xs = mkObject
    [ "kind"  .= String "PeersFetch"
    , "peers" .= toJSON
      (foldl' (\acc x -> toObject MaximalVerbosity x : acc) [] xs) ]


instance (Show peer, ToObject a) => ToObject (TraceLabelPeer peer a) where
  toObject verb (TraceLabelPeer peerid a) =
    mkObject [ "peer" .= show peerid ] <> toObject verb a


instance ToObject (AnyMessageAndAgency ps)
      => ToObject (TraceSendRecv ps) where
  toObject verb (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= toObject verb m ]
  toObject verb (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= toObject verb m ]


instance ToObject (TraceTxSubmissionInbound txid tx) where
  toObject _verb (TraceTxSubmissionCollected count) =
    mkObject
      [ "kind" .= String "TxSubmissionCollected"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxSubmissionProcessed processed) =
    mkObject
      [ "kind" .= String "TxSubmissionProcessed"
      , "accepted" .= toJSON (ptxcAccepted processed)
      , "rejected" .= toJSON (ptxcRejected processed)
      ]
  toObject _verb TraceTxInboundTerminated =
    mkObject
      [ "kind" .= String "TxInboundTerminated"
      ]
  toObject _verb (TraceTxInboundCanRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TxInboundCanRequestMoreTxs"
      , "count" .= toJSON count
      ]
  toObject _verb (TraceTxInboundCannotRequestMoreTxs count) =
    mkObject
      [ "kind" .= String "TxInboundCannotRequestMoreTxs"
      , "count" .= toJSON count
      ]


instance Aeson.ToJSONKey Socket.SockAddr where

-- TODO: ouroboros-network should provide a newtype wrapper for 'SockAddr'.
instance ToObject Socket.SockAddr where
    toObject _verb (Socket.SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mkObject [ "address" .= toJSON ip
                 , "port" .= show port
                 ]
    toObject _verb (Socket.SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mkObject [ "address" .= toJSON ip
                 , "port" .= show port
                 ]
    toObject _verb (Socket.SockAddrUnix path) =
        mkObject [ "path" .= show path ]

instance Aeson.ToJSON Socket.SockAddr where
    toJSON (Socket.SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (Socket.SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        Aeson.object [ "address" .= toJSON ip
                     , "port" .= show port
                     ]
    toJSON (Socket.SockAddrUnix path) =
        Aeson.object [ "path" .= show path ]

-- TODO: use the json encoding of transactions
instance (Show txid, Show tx)
      => ToObject (TraceTxSubmissionOutbound txid tx) where
  toObject MaximalVerbosity (TraceTxSubmissionOutboundRecvMsgRequestTxs txids) =
    mkObject
      [ "kind" .= String "TxSubmissionOutboundRecvMsgRequestTxs"
      , "txIds" .= String (pack $ show txids)
      ]
  toObject _verb (TraceTxSubmissionOutboundRecvMsgRequestTxs _txids) =
    mkObject
      [ "kind" .= String "TxSubmissionOutboundRecvMsgRequestTxs"
      ]
  toObject MaximalVerbosity (TraceTxSubmissionOutboundSendMsgReplyTxs txs) =
    mkObject
      [ "kind" .= String "TxSubmissionOutboundSendMsgReplyTxs"
      , "txs" .= String (pack $ show txs)
      ]
  toObject _verb (TraceTxSubmissionOutboundSendMsgReplyTxs _txs) =
    mkObject
      [ "kind" .= String "TxSubmissionOutboundSendMsgReplyTxs"
      ]
  toObject _verb (TraceControlMessage controlMessage) =
    mkObject
      [ "kind" .= String "ControlMessage"
      , "controlMessage" .= String (pack $ show controlMessage)
      ]


instance Show remotePeer => ToObject (TraceKeepAliveClient remotePeer) where
  toObject _verb (AddSample peer rtt pgsv) =
    mkObject
      [ "kind" .= String "KeepAliveClient AddSample"
      , "address" .= show peer
      , "rtt" .= rtt
      , "sampleTime" .= show (dTime $ sampleTime pgsv)
      , "outboundG" .= (realToFrac $ gGSV (outboundGSV pgsv) :: Double)
      , "inboundG" .= (realToFrac $ gGSV (inboundGSV pgsv) :: Double)
      ]
    where
      gGSV :: GSV -> DiffTime
      gGSV (GSV g _ _) = g

      dTime :: Time -> Double
      dTime (Time d) = realToFrac d


instance ToObject TraceLedgerPeers where
  toObject _verb (PickedPeer addr _ackStake stake) =
    mkObject
      [ "kind" .= String "PickedPeer"
      , "address" .= show addr
      , "relativeStake" .= (realToFrac stake :: Double)
      ]
  toObject _verb (PickedPeers (NumberOfPeers n) addrs) =
    mkObject
      [ "kind" .= String "PickedPeers"
      , "desiredCount" .= n
      , "count" .= length addrs
      , "addresses" .= show addrs
      ]
  toObject _verb (FetchingNewLedgerState cnt) =
    mkObject
      [ "kind" .= String "FetchingNewLedgerState"
      , "numberOfPools" .= cnt
      ]
  toObject _verb DisabledLedgerPeers =
    mkObject
      [ "kind" .= String "DisabledLedgerPeers"
      ]
  toObject _verb (TraceUseLedgerAfter ula) =
    mkObject
      [ "kind" .= String "UseLedgerAfter"
      , "useLedgerAfter" .= UseLedger ula
      ]
  toObject _verb WaitingOnRequest =
    mkObject
      [ "kind" .= String "WaitingOnRequest"
      ]
  toObject _verb (RequestForPeers (NumberOfPeers np)) =
    mkObject
      [ "kind" .= String "RequestForPeers"
      , "numberOfPeers" .= np
      ]
  toObject _verb (ReusingLedgerState cnt age) =
    mkObject
      [ "kind" .= String "ReusingLedgerState"
      , "numberOfPools" .= cnt
      , "ledgerStateAge" .= age
      ]
  toObject _verb FallingBackToBootstrapPeers =
    mkObject
      [ "kind" .= String "FallingBackToBootstrapPeers"
      ]


instance Show addr => ToObject (WithAddr addr ErrorPolicyTrace) where
  toObject _verb (WithAddr addr ev) =
    mkObject [ "kind" .= String "ErrorPolicyTrace"
             , "address" .= show addr
             , "event" .= show ev ]


instance ToObject (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "WithIPList SubscriptionTrace"
             , "localAddresses" .= show localAddresses
             , "dests" .= show dests
             , "event" .= show ev ]


instance ToObject (WithDomainName DnsTrace) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance ToObject (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  toObject _verb (WithDomainName dom ev) =
    mkObject [ "kind" .= String "SubscriptionTrace"
             , "domain" .= show dom
             , "event" .= show ev ]


instance (ToObject peer) => ToObject (WithMuxBearer peer MuxTrace) where
  toObject verb (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= toObject verb b
             , "event" .= show ev ]

instance Aeson.ToJSONKey RelayAddress where

instance Show exception => ToObject (TraceLocalRootPeers RemoteAddress exception) where
  toObject _verb (TraceLocalRootDomains groups) =
    mkObject [ "kind" .= String "LocalRootDomains"
             , "localRootDomains" .= toJSON groups
             ]
  toObject _verb (TraceLocalRootWaiting d dt) =
    mkObject [ "kind" .= String "LocalRootWaiting"
             , "domainAddress" .= toJSON d
             , "diffTime" .= show dt
             ]
  toObject _verb (TraceLocalRootResult d res) =
    mkObject [ "kind" .= String "LocalRootResult"
             , "domainAddress" .= toJSON d
             , "result" .= Aeson.toJSONList res
             ]
  toObject _verb (TraceLocalRootGroups groups) =
    mkObject [ "kind" .= String "LocalRootGroups"
             , "localRootGroups" .= toJSON groups
             ]
  toObject _verb (TraceLocalRootFailure d dexception) =
    mkObject [ "kind" .= String "LocalRootFailure"
             , "domainAddress" .= toJSON d
             , "error" .= show dexception
             ]

instance ToObject TracePublicRootPeers where
  toObject _verb (TracePublicRootRelayAddresses relays) =
    mkObject [ "kind" .= String "PublicRootRelayAddresses"
             , "relayAddresses" .= Aeson.toJSONList relays
             ]
  toObject _verb (TracePublicRootDomains domains) =
    mkObject [ "kind" .= String "PublicRootDomains"
             , "domainAddresses" .= Aeson.toJSONList domains
             ]
  toObject _verb (TracePublicRootResult b res) =
    mkObject [ "kind" .= String "PublicRootResult"
             , "domain" .= show b
             , "result" .= Aeson.toJSONList res
             ]
  toObject _verb (TracePublicRootFailure b d) =
    mkObject [ "kind" .= String "PublicRootFailure"
             , "domain" .= show b
             , "error" .= show d
             ]

instance ToJSON PeerStatus where
  toJSON = String . pack . show

instance (Aeson.ToJSONKey peerAddr, ToJSON peerAddr, Show peerAddr)
  => ToJSON (LocalRootPeers peerAddr) where
  toJSON lrp =
    Aeson.object [ "kind" .= String "LocalRootPeers"
                 , "state" .= toJSON (toMap lrp)
                 , "groups" .= Aeson.toJSONList (toGroupSets lrp)
                 ]

instance ToJSON PeerSelectionTargets where
  toJSON (PeerSelectionTargets
            nRootPeers
            nKnownPeers
            nEstablishedPeers
            nActivePeers
         ) =
    Aeson.object [ "kind" .= String "PeerSelectionTargets"
                 , "targetRootPeers" .= nRootPeers
                 , "targetKnownPeers" .= nKnownPeers
                 , "targetEstablishedPeers" .= nEstablishedPeers
                 , "targetActivePeers" .= nActivePeers
                 ]

instance ToObject (TracePeerSelection Socket.SockAddr) where
  toObject _verb (TraceLocalRootPeersChanged lrp lrp') =
    mkObject [ "kind" .= String "LocalRootPeersChanged"
             , "previous" .= toJSON lrp
             , "current" .= toJSON lrp'
             ]
  toObject _verb (TraceTargetsChanged pst pst') =
    mkObject [ "kind" .= String "TargetsChanged"
             , "previous" .= toJSON pst
             , "current" .= toJSON pst'
             ]
  toObject _verb (TracePublicRootsRequest tRootPeers nRootPeers) =
    mkObject [ "kind" .= String "PublicRootsRequest"
             , "targetNumberOfRootPeers" .= tRootPeers
             , "numberOfRootPeers" .= nRootPeers
             ]
  toObject _verb (TracePublicRootsResults res group dt) =
    mkObject [ "kind" .= String "PublicRootsResults"
             , "result" .= Aeson.toJSONList (toList res)
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TracePublicRootsFailure err group dt) =
    mkObject [ "kind" .= String "PublicRootsFailure"
             , "error" .= show err
             , "group" .= group
             , "diffTime" .= dt
             ]
  toObject _verb (TraceGossipRequests targetKnown actualKnown aps sps) =
    mkObject [ "kind" .= String "GossipRequests"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "availablePeers" .= Aeson.toJSONList (toList aps)
             , "selectedPeers" .= Aeson.toJSONList (toList sps)
             ]
  toObject _verb (TraceGossipResults res) =
    mkObject [ "kind" .= String "GossipResults"
             , "result" .= Aeson.toJSONList (map ( bimap show id <$> ) res)
             ]
  toObject _verb (TraceForgetColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "ForgeColdPeers"
             , "targetKnown" .= targetKnown
             , "actualKnown" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdPeers targetKnown actualKnown sp) =
    mkObject [ "kind" .= String "PromoteColdPeers"
             , "targetEstablished" .= targetKnown
             , "actualEstablished" .= actualKnown
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdLocalPeers tLocalEst aLocalEst sp) =
    mkObject [ "kind" .= String "PromoteColdLocalPeers"
             , "targetLocalEstablished" .= tLocalEst
             , "actualLocalEstablished" .= aLocalEst
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteColdFailed tEst aEst p d err) =
    mkObject [ "kind" .= String "PromoteColdFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "delay" .= toJSON d
             , "error" .= show err
             ]
  toObject _verb (TracePromoteColdDone tEst aEst p) =
    mkObject [ "kind" .= String "PromoteColdDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  toObject _verb (TracePromoteWarmPeers tActive aActive sp) =
    mkObject [ "kind" .= String "PromoteWarmPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteWarmLocalPeers taa sp) =
    mkObject [ "kind" .= String "PromoteWarmLocalPeers"
             , "targetActualActive" .= Aeson.toJSONList taa
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TracePromoteWarmFailed tActive aActive p err) =
    mkObject [ "kind" .= String "PromoteWarmFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "error" .= show err
             ]
  toObject _verb (TracePromoteWarmDone tActive aActive p) =
    mkObject [ "kind" .= String "PromoteWarmDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteWarmPeers tEst aEst sp) =
    mkObject [ "kind" .= String "DemoteWarmPeers"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteWarmFailed tEst aEst p err) =
    mkObject [ "kind" .= String "DemoteWarmFailed"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             , "error" .= show err
             ]
  toObject _verb (TraceDemoteWarmDone tEst aEst p) =
    mkObject [ "kind" .= String "DemoteWarmDone"
             , "targetEstablished" .= tEst
             , "actualEstablished" .= aEst
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteHotPeers tActive aActive sp) =
    mkObject [ "kind" .= String "DemoteHotPeers"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteLocalHotPeers taa sp) =
    mkObject [ "kind" .= String "DemoteLocalHotPeers"
             , "targetActualActive" .= Aeson.toJSONList taa
             , "selectedPeers" .= Aeson.toJSONList (toList sp)
             ]
  toObject _verb (TraceDemoteHotFailed tActive aActive p err) =
    mkObject [ "kind" .= String "DemoteHotFailed"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             , "error" .= show err
             ]
  toObject _verb (TraceDemoteHotDone tActive aActive p) =
    mkObject [ "kind" .= String "DemoteHotDone"
             , "targetActive" .= tActive
             , "actualActive" .= aActive
             , "peer" .= toJSON p
             ]
  toObject _verb (TraceDemoteAsynchronous msp) =
    mkObject [ "kind" .= String "DemoteAsynchronous"
             , "state" .= toJSON msp
             ]
  toObject _verb TraceGovernorWakeup =
    mkObject [ "kind" .= String "GovernorWakeup"
             ]
  toObject _verb (TraceChurnWait dt) =
    mkObject [ "kind" .= String "ChurnWait"
             , "diffTime" .= toJSON dt
             ]
  toObject _verb (TraceChurnMode c) =
    mkObject [ "kind" .= String "ChurnMode"
             , "event" .= show c ]

instance Aeson.ToJSON AbstractState where
    toJSON UnknownConnectionSt =
      Aeson.object [ "kind" .= String "UnknownConnectionSt" ]
    toJSON ReservedOutboundSt =
      Aeson.object [ "kind" .= String "ReservedOutboundSt" ]
    toJSON (UnnegotiatedSt provenance) =
      Aeson.object [ "kind" .= String "UnnegotiatedSt"
                   , "provenance" .= String (pack . show $ provenance)
                   ]
    toJSON (InboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON (InboundSt dataFlow) =
      Aeson.object [ "kind" .= String "InboundSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON OutboundUniSt =
      Aeson.object [ "kind" .= String "OutboundUniSt" ]
    toJSON (OutboundDupSt timeoutExpired) =
      Aeson.object [ "kind" .= String "OutboundDupSt"
                   , "timeoutState" .= String (pack . show $ timeoutExpired)
                   ]
    toJSON (OutboundIdleSt dataFlow) =
      Aeson.object [ "kind" .= String "OutboundIdleSt"
                   , "dataFlow" .= String (pack . show $ dataFlow)
                   ]
    toJSON DuplexSt =
      Aeson.object [ "kind" .= String "DuplexSt" ]
    toJSON WaitRemoteIdleSt =
      Aeson.object [ "kind" .= String "WaitRemoteIdleSt" ]
    toJSON TerminatingSt =
      Aeson.object [ "kind" .= String "TerminatingSt" ]
    toJSON TerminatedSt =
      Aeson.object [ "kind" .= String "TerminatedSt" ]


peerSelectionTargetsToObject :: PeerSelectionTargets -> Value
peerSelectionTargetsToObject
  PeerSelectionTargets { targetNumberOfRootPeers,
                         targetNumberOfKnownPeers,
                         targetNumberOfEstablishedPeers,
                         targetNumberOfActivePeers } =
    Object $
      mkObject [ "roots" .= targetNumberOfRootPeers
               , "knownPeers" .= targetNumberOfKnownPeers
               , "established" .= targetNumberOfEstablishedPeers
               , "active" .= targetNumberOfActivePeers
               ]

instance Show peerConn => ToObject (DebugPeerSelection Socket.SockAddr peerConn) where
  toObject verb (TraceGovernorState blockedAt wakeupAfter
                   PeerSelectionState { targets, knownPeers, establishedPeers, activePeers })
      | verb <= NormalVerbosity =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "targets" .= peerSelectionTargetsToObject targets
             , "numberOfPeers" .=
                 Object (mkObject [ "known" .= KnownPeers.size knownPeers
                                  , "established" .= EstablishedPeers.size establishedPeers
                                  , "active" .= Set.size activePeers
                                  ])
             ]
  toObject _ (TraceGovernorState blockedAt wakeupAfter ev) =
    mkObject [ "kind" .= String "DebugPeerSelection"
             , "blockedAt" .= String (pack $ show blockedAt)
             , "wakeupAfter" .= String (pack $ show wakeupAfter)
             , "peerSelectionState" .= String (pack $ show ev)
             ]

-- TODO: Write PeerStatusChangeType ToJSON at ouroboros-network
-- For that an export is needed at ouroboros-network
instance ToObject (PeerSelectionActionsTrace Socket.SockAddr) where
  toObject _verb (PeerStatusChanged ps) =
    mkObject [ "kind" .= String "PeerStatusChanged"
             , "peerStatusChangeType" .= show ps
             ]
  toObject _verb (PeerStatusChangeFailure ps f) =
    mkObject [ "kind" .= String "PeerStatusChangeFailure"
             , "peerStatusChangeType" .= show ps
             , "error" .= show f
             ]
  toObject _verb (PeerMonitoringError connId s) =
    mkObject [ "kind" .= String "PeerMonitoridngError"
             , "connectionId" .= toJSON connId
             , "error" .= show s
             ]
  toObject _verb (PeerMonitoringResult connId wf) =
    mkObject [ "kind" .= String "PeerMonitoringResult"
             , "connectionId" .= toJSON connId
             , "withProtocolTemp" .= show wf
             ]

instance ToObject PeerSelectionCounters where
  toObject _verb ev =
    mkObject [ "kind" .= String "PeerSelectionCounters"
             , "coldPeers" .= coldPeers ev
             , "warmPeers" .= warmPeers ev
             , "hotPeers" .= hotPeers ev
             ]

instance (Show (ClientHasAgency st), Show (ServerHasAgency st))
  => ToJSON (PeerHasAgency pr st) where
  toJSON (ClientAgency cha) =
    Aeson.object [ "kind" .= String "ClientAgency"
                 , "agency" .= show cha
                 ]
  toJSON (ServerAgency sha) =
    Aeson.object [ "kind" .= String "ServerAgency"
                 , "agency" .= show sha
                 ]

instance ToJSON ProtocolLimitFailure where
  toJSON (ExceededSizeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= toJSON tok
                 ]
  toJSON (ExceededTimeLimit tok) =
    Aeson.object [ "kind" .= String "ProtocolLimitFailure"
                 , "agency" .= toJSON tok
                 ]

instance Show vNumber => ToJSON (RefuseReason vNumber) where
  toJSON (VersionMismatch vNumber tags) =
    Aeson.object [ "kind" .= String "VersionMismatch"
                 , "versionNumber" .= show vNumber
                 , "tags" .= Aeson.toJSONList tags
                 ]
  toJSON (HandshakeDecodeError vNumber t) =
    Aeson.object [ "kind" .= String "HandshakeDecodeError"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]
  toJSON (Refused vNumber t) =
    Aeson.object [ "kind" .= String "Refused"
                 , "versionNumber" .= show vNumber
                 , "text" .= String (pack $ show t)
                 ]

instance Show vNumber => ToJSON (HandshakeProtocolError vNumber) where
  toJSON (HandshakeError rvNumber) =
    Aeson.object [ "kind" .= String "HandshakeError"
                 , "reason" .= toJSON rvNumber
                 ]
  toJSON (NotRecognisedVersion vNumber) =
    Aeson.object [ "kind" .= String "NotRecognisedVersion"
                 , "versionNumber" .= show vNumber
                 ]
  toJSON (InvalidServerSelection vNumber t) =
    Aeson.object [ "kind" .= String "InvalidServerSelection"
                 , "versionNumber" .= show vNumber
                 , "reason" .= String (pack $ show t)
                 ]

instance Show vNumber => ToJSON (HandshakeException vNumber) where
  toJSON (HandshakeProtocolLimit plf) =
    Aeson.object [ "kind" .= String "HandshakeProtocolLimit"
                 , "handshakeProtocolLimit" .= toJSON plf
                 ]
  toJSON (HandshakeProtocolError err) =
    Aeson.object [ "kind" .= String "HandshakeProtocolError"
                 , "error" .= show err
                 ]

instance ToJSON NodeToNodeVersion where
  toJSON x = String (pack $ show x)

instance ToJSON NodeToClientVersion where
  toJSON x = String (pack $ show x)

instance ToJSON NodeToNodeVersionData where
  toJSON (NodeToNodeVersionData (NetworkMagic m) dm) =
    Aeson.object [ "kind" .= String "NodeToNodeVersionData"
                 , "networkMagic" .= toJSON m
                 , "diffusionMode" .= show dm
                 ]

instance ToJSON NodeToClientVersionData where
  toJSON (NodeToClientVersionData (NetworkMagic m)) =
    Aeson.object [ "kind" .= String "NodeToClientVersionData"
                 , "networkMagic" .= toJSON m
                 ]

instance (Show versionNumber, ToJSON versionNumber, ToJSON agreedOptions)
  => ToObject (ConnectionHandlerTrace versionNumber agreedOptions) where
  toObject _verb (TrHandshakeSuccess versionNumber agreedOptions) =
    mkObject
      [ "kind" .= String "HandshakeSuccess"
      , "versionNumber" .= toJSON versionNumber
      , "agreedOptions" .= toJSON agreedOptions
      ]
  toObject _verb (TrHandshakeClientError err) =
    mkObject
      [ "kind" .= String "HandshakeClientError"
      , "error" .= toJSON err
      ]
  toObject _verb (TrHandshakeServerError err) =
    mkObject
      [ "kind" .= String "HandshakeServerError"
      , "error" .= toJSON err
      ]
  toObject _verb (TrError e err cerr) =
    mkObject
      [ "kind" .= String "Error"
      , "context" .= show e
      , "error" .= show err
      , "command" .= show cerr
      ]

instance (Show addr, Show versionNumber, Show agreedOptions, ToObject addr,
          ToJSON addr, ToJSON versionNumber, ToJSON agreedOptions)
      => ToObject (ConnectionManagerTrace addr (ConnectionHandlerTrace versionNumber agreedOptions)) where
  toObject verb ev =
    case ev of
      TrIncludeConnection prov peerAddr ->
        mkObject $ reverse
          [ "kind" .= String "IncludeConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrUnregisterConnection prov peerAddr ->
        mkObject $ reverse
          [ "kind" .= String "UnregisterConnection"
          , "remoteAddress" .= toObject verb peerAddr
          , "provenance" .= String (pack . show $ prov)
          ]
      TrConnect (Just localAddress) remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectTo"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          ]
      TrConnect Nothing remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectTo"
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectError (Just localAddress) remoteAddress err ->
        mkObject
          [ "kind" .= String "ConnectError"
          , "connectionId" .= toJSON ConnectionId { localAddress, remoteAddress }
          , "error" .= String (pack . show $ err)
          ]
      TrConnectError Nothing remoteAddress err ->
        mkObject
          [ "kind" .= String "ConnectError"
          , "remoteAddress" .= toObject verb remoteAddress
          , "error" .= String (pack . show $ err)
          ]
      TrTerminatingConnection prov connId ->
        mkObject
          [ "kind" .= String "TerminatingConnection"
          , "provenance" .= String (pack . show $ prov)
          , "connectionId" .= toJSON connId
          ]
      TrTerminatedConnection prov remoteAddress ->
        mkObject
          [ "kind" .= String "TerminatedConnection"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= toObject verb remoteAddress
          ]
      TrConnectionHandler connId a ->
        mkObject
          [ "kind" .= String "ConnectionHandler"
          , "connectionId" .= toJSON connId
          , "connectionHandler" .= toObject verb a
          ]
      TrShutdown ->
        mkObject
          [ "kind" .= String "Shutdown"
          ]
      TrConnectionExists prov remoteAddress inState ->
        mkObject
          [ "kind" .= String "ConnectionExists"
          , "provenance" .= String (pack . show $ prov)
          , "remoteAddress" .= toObject verb remoteAddress
          , "state" .= toJSON inState
          ]
      TrForbiddenConnection connId ->
        mkObject
          [ "kind" .= String "ForbiddenConnection"
          , "connectionId" .= toJSON connId
          ]
      TrImpossibleConnection connId ->
        mkObject
          [ "kind" .= String "ImpossibleConnection"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionFailure connId ->
        mkObject
          [ "kind" .= String "ConnectionFailure"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionNotFound prov remoteAddress ->
        mkObject
          [ "kind" .= String "ConnectionNotFound"
          , "remoteAddress" .= toObject verb remoteAddress
          , "provenance" .= String (pack . show $ prov)
          ]
      TrForbiddenOperation remoteAddress connState ->
        mkObject
          [ "kind" .= String "ForbiddenOperation"
          , "remoteAddress" .= toObject verb remoteAddress
          , "connectionState" .= toJSON connState
          ]
      TrPruneConnections peers ->
        mkObject
          [ "kind" .= String "PruneConnections"
          , "peers" .= toJSON (toObject verb `map` peers)
          ]
      TrConnectionCleanup connId ->
        mkObject
          [ "kind" .= String "ConnectionCleanup"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionTimeWait connId ->
        mkObject
          [ "kind" .= String "ConnectionTimeWait"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionTimeWaitDone connId ->
        mkObject
          [ "kind" .= String "ConnectionTimeWaitDone"
          , "connectionId" .= toJSON connId
          ]
      TrConnectionManagerCounters cmCounters ->
        mkObject
          [ "kind"  .= String "ConnectionManagerCounters"
          , "state" .= toJSON cmCounters
          ]
      TrState cmState ->
        mkObject
          [ "kind"  .= String "ConnectionManagerState"
          , "state" .= Aeson.listValue (\(addr, connState) ->
                                         Aeson.object
                                           [ "remoteAddress"   .= toJSON addr
                                           , "connectionState" .= toJSON connState
                                           ])
                                       (Map.toList cmState)
          ]

instance (Show addr, ToObject addr, ToJSON addr)
      => ToObject (ServerTrace addr) where
  toObject verb (TrAcceptConnection peerAddr)     =
    mkObject [ "kind" .= String "AcceptConnection"
             , "address" .= toObject verb peerAddr
             ]
  toObject _verb (TrAcceptError exception)         =
    mkObject [ "kind" .= String "AcceptErroor"
             , "error" .= show exception
             ]
  toObject verb (TrAcceptPolicyTrace policyTrace) =
    mkObject [ "kind" .= String "AcceptPolicyTrace"
             , "policy" .= toObject verb policyTrace
             ]
  toObject verb (TrServerStarted peerAddrs)       =
    mkObject [ "kind" .= String "AcceptPolicyTrace"
             , "addresses" .= toJSON (toObject verb `map` peerAddrs)
             ]
  toObject _verb TrServerStopped                   =
    mkObject [ "kind" .= String "ServerStopped"
             ]
  toObject _verb (TrServerError exception)         =
    mkObject [ "kind" .= String "ServerError"
             , "error" .= show exception
             ]

instance ToJSON MiniProtocolNum where
  toJSON (MiniProtocolNum w) =
    Aeson.object [ "kind" .= String "MiniProtocolNum"
                 , "num" .= w
                 ]

instance ToJSON addr => ToJSON (OperationResult addr) where
  toJSON (UnsupportedState as) =
    Aeson.object [ "kind" .= String "UnsupportedState"
                 , "unsupportedState" .= toJSON as
                 ]
  toJSON (OperationSuccess addr) =
    Aeson.object [ "kind" .= String "OperationSuccess"
                 , "operationSuccess" .= toJSON addr
                 ]

instance ToJSON RemoteSt where
  toJSON = String . pack . show

instance ToJSON addr => Aeson.ToJSONKey (ConnectionId addr) where

instance ToJSON addr
      => ToObject (InboundGovernorTrace addr) where
  toObject _verb (TrNewConnection p connId)            =
    mkObject [ "kind" .= String "NewConnection"
             , "provenance" .= show p
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrResponderRestarted connId m)       =
    mkObject [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrResponderStartFailure connId m s)  =
    mkObject [ "kind" .= String "ResponderStartFailure"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "error" .= show s
             ]
  toObject _verb (TrResponderErrored connId m s)       =
    mkObject [ "kind" .= String "ResponderErrored"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             , "error" .= show s
             ]
  toObject _verb (TrResponderStarted connId m)         =
    mkObject [ "kind" .= String "ResponderStarted"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrResponderTerminated connId m)      =
    mkObject [ "kind" .= String "ResponderTerminated"
             , "connectionId" .= toJSON connId
             , "miniProtocolNum" .= toJSON m
             ]
  toObject _verb (TrPromotedToWarmRemote connId opRes) =
    mkObject [ "kind" .= String "PromotedToWarmRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (TrPromotedToHotRemote connId)        =
    mkObject [ "kind" .= String "PromotedToHotRemote"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrDemotedToColdRemote connId od)     =
    mkObject [ "kind" .= String "DemotedToColdRemote"
             , "connectionId" .= toJSON connId
             , "result" .= show od
             ]
  toObject _verb (TrWaitIdleRemote connId opRes) =
    mkObject [ "kind" .= String "WaitIdleRemote"
             , "connectionId" .= toJSON connId
             , "result" .= toJSON opRes
             ]
  toObject _verb (TrMuxCleanExit connId)               =
    mkObject [ "kind" .= String "MuxCleanExit"
             , "connectionId" .= toJSON connId
             ]
  toObject _verb (TrMuxErrored connId s)               =
    mkObject [ "kind" .= String "MuxErrored"
             , "connectionId" .= toJSON connId
             , "error" .= show s
             ]
  toObject _verb (TrInboundGovernorCounters counters) =
    mkObject [ "kind" .= String "InboundGovernorCounters"
             , "warmPeers" .= warmPeersRemote counters
             , "hotPeers" .= hotPeersRemote counters
             ]
  toObject _verb (TrRemoteState st) =
    mkObject [ "kind" .= String "RemoteState"
             , "remoteSt" .= toJSON st
             ]
