
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}


-- {-# OPTIONS_GHC -Wno-unused-imports  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import           Data.Aeson (ToJSON)
import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Prelude hiding (trace)
import           Cardano.TraceDispatcher.ChainDB.Combinators
import           Cardano.TraceDispatcher.ChainDB.Docu
import           Cardano.TraceDispatcher.Common.ConvertTxId
import           Cardano.TraceDispatcher.Common.Formatting ()
import           Cardano.TraceDispatcher.Consensus.Combinators
import           Cardano.TraceDispatcher.Consensus.Docu
import           Cardano.TraceDispatcher.Consensus.StateInfo
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Network.Formatting ()


import           Cardano.Node.Configuration.Logging (EKGDirect)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block (CannotForge, ForgeStateUpdateError,
                     Point)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerUpdate,
                     LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId, HasTxId, HasTxs)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import           Ouroboros.Consensus.Mempool.API (TraceEventMempool (..))
import           Ouroboros.Consensus.MiniProtocol.BlockFetch.Server
                     (TraceBlockFetchServerEvent (..))
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Client
                     (TraceChainSyncClientEvent)
import           Ouroboros.Consensus.MiniProtocol.ChainSync.Server
                     (TraceChainSyncServerEvent)
import           Ouroboros.Consensus.MiniProtocol.LocalTxSubmission.Server
                     (TraceLocalTxSubmissionServerEvent (..))
import qualified Ouroboros.Consensus.Network.NodeToClient as NtC
import qualified Ouroboros.Consensus.Network.NodeToNode as NtN
import qualified Ouroboros.Consensus.Node.Run as Consensus
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import           Ouroboros.Consensus.Shelley.Ledger.Block
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB
import           Ouroboros.Consensus.Storage.Serialisation (SerialisedHeader)

import           Ouroboros.Network.Block (Serialised, Tip)
import qualified Ouroboros.Network.BlockFetch.ClientState as BlockFetch
import           Ouroboros.Network.BlockFetch.Decision
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv)
import           Ouroboros.Network.KeepAlive (TraceKeepAliveClient (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import           Ouroboros.Network.NodeToNode (ErrorPolicyTrace (..),
                     WithAddr (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type (BlockFetch)
import           Ouroboros.Network.Protocol.ChainSync.Type (ChainSync)
import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (LocalStateQuery)
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.TxSubmission.Type (TxSubmission)
import           Ouroboros.Network.Protocol.TxSubmission2.Type (TxSubmission2)
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (WithIPList (..))
import           Ouroboros.Network.Subscription.Worker (SubscriptionTrace (..))
import           Ouroboros.Network.TxSubmission.Inbound
                     (TraceTxSubmissionInbound)
import           Ouroboros.Network.TxSubmission.Outbound
                     (TraceTxSubmissionOutbound)

type Peer = NtN.ConnectionId Socket.SockAddr

mkStandardTracer ::
     LogFormatting evt
  => Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> IO (Trace IO evt)
mkStandardTracer name namesFor severityFor trStdout trForward mbTrEkg = do
  tr1 <- humanFormatter True "Cardano" trStdout
  tr2 <- forwardFormatter "Cardano" trForward
  let trNs1 = appendName name $ appendName "Node" tr1
  let trNs2 = appendName name $ appendName "Node" tr2
  case mbTrEkg of
    Nothing ->
      pure $ withNamesAppended namesFor
              $ withSeverity severityFor (trNs1 <> trNs2)
    Just trEkg -> do
      tr3 <- metricsFormatter "Cardano" trEkg
      let trNs3 = appendName name $ appendName "Node" tr3
      pure $ withNamesAppended namesFor
              $ withSeverity severityFor (trNs1 <> trNs2 <> trNs3)

mkStandardTracerSimple ::
     LogFormatting evt
  => Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> Trace IO FormattedMessage
  -> IO (Trace IO evt)
mkStandardTracerSimple name namesFor severityFor trStdout = do
  tr <- humanFormatter True "Cardano" trStdout
  let trNs = appendName name $ appendName "Node" tr
  pure $ withNamesAppended namesFor
          $ withSeverity severityFor trNs

docTracers :: forall blk t.
  ( Show t
  , Show (Header blk)
  , forall result. Show (Query blk result)
  , LogFormatting (LedgerUpdate blk)
  , LogFormatting (LedgerWarning blk)
  , LogFormatting (ApplyTxErr blk)
  , LogFormatting (CannotForge blk)
  , LogFormatting (Header blk)
  , LogFormatting (ForgeStateUpdateError blk)
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LogFormatting (GenTx blk)
  , ToJSON (GenTxId blk)
  , HasTxId (GenTx blk)
  , LedgerSupportsProtocol blk
  , Consensus.RunNode blk
  , HasTxs blk
  , ConvertTxId' blk
  )
  => Proxy blk -> IO ()
docTracers _ = do
    trBase <- standardTracer Nothing
    cdbmTr <- mkStandardTracerSimple
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                trBase
    cscTr  <- mkStandardTracerSimple
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                trBase
    csshTr <- mkStandardTracerSimple
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                trBase
    cssbTr <- mkStandardTracerSimple
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                trBase
    bfdTr  <- mkStandardTracerSimple
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                trBase
    bfcTr  <- mkStandardTracerSimple
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                trBase
    bfsTr  <- mkStandardTracerSimple
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                trBase
    fsiTr  <- mkStandardTracerSimple
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                trBase
    txiTr  <- mkStandardTracerSimple
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                trBase
    txoTr  <- mkStandardTracerSimple
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                trBase
    ltxsTr <- mkStandardTracerSimple
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                trBase
    mpTr   <- mkStandardTracerSimple
                "Mempool"
                namesForMempool
                severityMempool
                trBase
    fTr    <- mkStandardTracerSimple
                "Forge"
                namesForForge
                severityForge
                trBase
    btTr   <- mkStandardTracerSimple
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                trBase
    kacTr  <- mkStandardTracerSimple
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                trBase
    tcsTr  <-  mkStandardTracerSimple
                "TChainSync"
                namesForTChainSync
                severityTChainSync
                trBase
    ttsTr  <-  mkStandardTracerSimple
                "TTxSubmission"
                namesForTTxSubmission
                severityTTxSubmission
                trBase
    tsqTr  <-  mkStandardTracerSimple
                "TStateQuery"
                namesForTStateQuery
                severityTStateQuery
                trBase
    tcsnTr <-  mkStandardTracerSimple
                "TChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                trBase
    tcssTr <-  mkStandardTracerSimple
                "TChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                trBase
    tbfTr  <-  mkStandardTracerSimple
                "TBlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                trBase
    tbfsTr <-  mkStandardTracerSimple
                "TBlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                trBase
    tsnTr  <-  mkStandardTracerSimple
                "TxSubmission"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                trBase
    ts2nTr  <-  mkStandardTracerSimple
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                trBase
    ipsTr   <-  mkStandardTracerSimple
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                trBase
    dnssTr  <-  mkStandardTracerSimple
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                trBase
    dnsrTr  <-  mkStandardTracerSimple
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                trBase
    errpTr  <-  mkStandardTracerSimple
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                trBase
    lerrpTr <-  mkStandardTracerSimple
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                trBase
    apTr    <-  mkStandardTracerSimple
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                trBase
    muxTr   <-  mkStandardTracerSimple
                "Mux"
                namesForMux
                severityMux
                trBase
    muxLTr   <-  mkStandardTracerSimple
                "MuxLocal"
                namesForMux
                severityMux
                trBase
    hsTr   <-  mkStandardTracerSimple
                "Handshake"
                namesForHandshake
                severityHandshake
                trBase
    lhsTr  <-  mkStandardTracerSimple
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                trBase
    diTr   <-  mkStandardTracerSimple
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                trBase

    configureTracers emptyTraceConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers emptyTraceConfig docChainSyncClientEvent [cscTr]
    configureTracers emptyTraceConfig docChainSyncServerEvent [csshTr]
    configureTracers emptyTraceConfig docChainSyncServerEvent [cssbTr]
    configureTracers emptyTraceConfig docBlockFetchDecision   [bfdTr]
    configureTracers emptyTraceConfig docBlockFetchClient     [bfcTr]
    configureTracers emptyTraceConfig docBlockFetchServer     [bfsTr]
    configureTracers emptyTraceConfig docTxInbound            [txiTr]
    configureTracers emptyTraceConfig docLocalTxSubmissionServer [ltxsTr]
    configureTracers emptyTraceConfig docMempool              [mpTr]
    configureTracers emptyTraceConfig docForge                [fTr]
    configureTracers emptyTraceConfig docBlockchainTime       [btTr]
    configureTracers emptyTraceConfig docKeepAliveClient      [kacTr]
    configureTracers emptyTraceConfig docTChainSync           [tcsTr]
    configureTracers emptyTraceConfig docTTxSubmission        [ttsTr]
    configureTracers emptyTraceConfig docTStateQuery          [tsqTr]
    configureTracers emptyTraceConfig docTChainSync           [tcsnTr]
    configureTracers emptyTraceConfig docTChainSync           [tcssTr]
    configureTracers emptyTraceConfig docTBlockFetch          [tbfTr]
    configureTracers emptyTraceConfig docTBlockFetch          [tbfsTr]
    configureTracers emptyTraceConfig docTTxSubmissionNode    [tsnTr]
    configureTracers emptyTraceConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers emptyTraceConfig docIPSubscription       [ipsTr]
    configureTracers emptyTraceConfig docDNSSubscription      [dnssTr]
    configureTracers emptyTraceConfig docDNSResolver          [dnsrTr]
    configureTracers emptyTraceConfig docErrorPolicy          [errpTr]
    configureTracers emptyTraceConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers emptyTraceConfig docAcceptPolicy         [apTr]
    configureTracers emptyTraceConfig docMux                  [muxTr]
    configureTracers emptyTraceConfig docMux                  [muxLTr]
    configureTracers emptyTraceConfig docHandshake            [hsTr]
    configureTracers emptyTraceConfig docLocalHandshake       [lhsTr]
    configureTracers emptyTraceConfig docLocalHandshake       [lhsTr]
    configureTracers emptyTraceConfig docDiffusionInit        [diTr]

    cdbmTrDoc    <- documentMarkdown
                (docChainDBTraceEvent :: Documented
                  (ChainDB.TraceEvent blk))
                [cdbmTr]
    cscTrDoc    <- documentMarkdown
                (docChainSyncClientEvent :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceChainSyncClientEvent blk)))
                [cscTr]
    csshTrDoc    <- documentMarkdown
                (docChainSyncServerEvent :: Documented
                  (TraceChainSyncServerEvent blk))
                [csshTr]
    cssbTrDoc    <- documentMarkdown
                (docChainSyncServerEvent :: Documented
                  (TraceChainSyncServerEvent blk))
                [cssbTr]
    bfdTrDoc    <- documentMarkdown
                (docBlockFetchDecision :: Documented
                  [BlockFetch.TraceLabelPeer Peer (FetchDecision [Point (Header blk)])])
                [bfdTr]
    bfcTrDoc    <- documentMarkdown
                (docBlockFetchClient :: Documented
                  (BlockFetch.TraceLabelPeer Peer (BlockFetch.TraceFetchClientState (Header blk))))
                [bfcTr]
    _bfsTrDoc    <- documentMarkdown
                (docBlockFetchServer :: Documented
                  (TraceBlockFetchServerEvent blk))
                [bfsTr]
    fsiTrDoc    <- documentMarkdown
                (docForgeStateInfo :: Documented
                  (Consensus.TraceLabelCreds HotKey.KESInfo))
                [fsiTr]
    txiTrDoc    <- documentMarkdown
                (docTxInbound :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceTxSubmissionInbound (GenTxId blk) (GenTx blk))))
                [txiTr]
    txoTrDoc    <- documentMarkdown
                (docTxOutbound :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceTxSubmissionOutbound (GenTxId blk) (GenTx blk))))
                [txoTr]
    ltxsTrDoc    <- documentMarkdown
                (docLocalTxSubmissionServer :: Documented
                  (TraceLocalTxSubmissionServerEvent blk))
                [ltxsTr]
    mpTrDoc    <- documentMarkdown
                (docMempool :: Documented
                  (TraceEventMempool blk))
                [mpTr]
    fTrDoc    <- documentMarkdown
                (docForge :: Documented
                  (Consensus.TraceLabelCreds (Consensus.TraceForgeEvent blk)))
                [fTr]
    btTrDoc   <- documentMarkdown
                (docBlockchainTime :: Documented
                  (TraceBlockchainTimeEvent t))
                [btTr]
    kacTrDoc  <- documentMarkdown
                (docKeepAliveClient :: Documented
                  (TraceKeepAliveClient Peer))
                [kacTr]
    tcsTrDoc  <- documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (Serialised blk) (Point blk) (Tip blk)))))
                [tcsTr]
    ttsTrDoc  <-  documentMarkdown
                (docTTxSubmission :: Documented
                   (BlockFetch.TraceLabelPeer
                      Peer
                      (TraceSendRecv
                         (LTS.LocalTxSubmission
                            (GenTx blk) (ApplyTxErr blk)))))
                [ttsTr]
    tsqTrDoc  <-  documentMarkdown
                (docTStateQuery :: Documented
                   (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (LocalStateQuery blk (Point blk) (Query blk)))))
                [tsqTr]
    tcsnTrDoc  <-  documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (Header blk) (Point blk) (Tip blk)))))
                [tcsnTr]
    tcssTrDoc  <-  documentMarkdown
                (docTChainSync :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (ChainSync (SerialisedHeader blk) (Point blk) (Tip blk)))))
                [tcssTr]
    tbfTrDoc  <-  documentMarkdown
                (docTBlockFetch :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (BlockFetch blk (Point blk)))))
                [tbfTr]
    tbfsTrDoc  <-  documentMarkdown
                (docTBlockFetch :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (BlockFetch (Serialised blk) (Point blk)))))
                [tbfsTr]
    tsnTrDoc   <-  documentMarkdown
                (docTTxSubmissionNode :: Documented
                  (BlockFetch.TraceLabelPeer Peer
                    (TraceSendRecv
                      (TxSubmission (GenTxId blk) (GenTx blk)))))
                [tsnTr]
    ts2nTrDoc  <-  documentMarkdown
                    (docTTxSubmission2Node :: Documented
                      (BlockFetch.TraceLabelPeer Peer
                        (TraceSendRecv
                          (TxSubmission2 (GenTxId blk) (GenTx blk)))))
                    [ts2nTr]
    _ipsTrDoc   <-  documentMarkdown
                    (docIPSubscription :: Documented
                      (WithIPList (SubscriptionTrace Socket.SockAddr)))
                    [ipsTr]
    dnssTrDoc   <-  documentMarkdown
                    (docDNSSubscription :: Documented
                      (WithDomainName (SubscriptionTrace Socket.SockAddr)))
                    [dnssTr]
    dnsrTrDoc   <-  documentMarkdown
                    (docDNSResolver :: Documented (WithDomainName DnsTrace))
                    [dnsrTr]
    errpTrDoc   <-  documentMarkdown
                    (docErrorPolicy :: Documented
                      (WithAddr Socket.SockAddr ErrorPolicyTrace))
                    [errpTr]
    lerrpTrDoc  <-  documentMarkdown
                    (docLocalErrorPolicy :: Documented
                      (WithAddr LocalAddress ErrorPolicyTrace))
                    [lerrpTr]
    apTrDoc     <-  documentMarkdown
                    (docAcceptPolicy :: Documented
                       NtN.AcceptConnectionsPolicyTrace)
                    [apTr]
    muxTrDoc     <-  documentMarkdown
                    (docMux :: Documented
                      (WithMuxBearer Peer MuxTrace))
                    [muxTr]
    muxLTrDoc    <-  documentMarkdown
                    (docMux :: Documented
                      (WithMuxBearer Peer MuxTrace))
                    [muxLTr]
    hsTrDoc      <-  documentMarkdown
                    (docHandshake :: Documented NtN.HandshakeTr)
                    [hsTr]
    lhsTrDoc     <-  documentMarkdown
                    (docLocalHandshake :: Documented NtC.HandshakeTr)
                    [lhsTr]
    diTrDoc      <-  documentMarkdown
                    (docDiffusionInit :: Documented ND.DiffusionInitializationTracer)
                    [diTr]

    let bl = cdbmTrDoc
            ++ cscTrDoc
            ++ csshTrDoc
            ++ cssbTrDoc
            ++ bfdTrDoc
            ++ bfcTrDoc
  --          ++ bfsTrDoc
            ++ fsiTrDoc
            ++ txiTrDoc
            ++ txoTrDoc
            ++ ltxsTrDoc
            ++ mpTrDoc
            ++ fTrDoc
            ++ btTrDoc
            ++ kacTrDoc
            ++ tcsTrDoc
            ++ ttsTrDoc
            ++ tsqTrDoc
            ++ tcsnTrDoc
            ++ tcssTrDoc
            ++ tbfTrDoc
            ++ tbfsTrDoc
            ++ tsnTrDoc
            ++ ts2nTrDoc
  --          ++ ipsTrDoc
            ++ dnssTrDoc
            ++ dnsrTrDoc
            ++ errpTrDoc
            ++ lerrpTrDoc
            ++ apTrDoc
            ++ muxTrDoc
            ++ muxLTrDoc
            ++ hsTrDoc
            ++ lhsTrDoc
            ++ diTrDoc

    res <- buildersToText bl
    T.writeFile "/home/yupanqui/IOHK/CardanoLogging.md" res
    pure ()

-- | Tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , TraceConstraints blk
  , Show peer, Eq peer
  , Show localPeer
  , ToObject peer
  , ToObject localPeer
  , LogFormatting peer
  , LogFormatting localPeer
  )
  => BlockConfig blk
  -> TraceOptions
  -> Old.Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> TraceConfig
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr _nodeKern _ekgDirect
  trBase trForward mbTrEKG _trConfig = do
    cdbmTr <- mkStandardTracer
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                trBase trForward mbTrEKG
    cscTr  <- mkStandardTracer
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                trBase trForward mbTrEKG
    csshTr <- mkStandardTracer
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                trBase trForward mbTrEKG
    cssbTr <- mkStandardTracer
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                trBase trForward mbTrEKG
    bfdTr  <- mkStandardTracer
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                trBase trForward mbTrEKG
    bfcTr  <- mkStandardTracer
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                trBase trForward mbTrEKG
    bfsTr  <- mkStandardTracer
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                trBase trForward mbTrEKG
    fsiTr  <- mkStandardTracer
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                trBase trForward mbTrEKG
    txiTr  <- mkStandardTracer
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                trBase trForward mbTrEKG
    txoTr  <- mkStandardTracer
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                trBase trForward mbTrEKG
    ltxsTr <- mkStandardTracer
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                trBase trForward mbTrEKG
    mpTr   <- mkStandardTracer
                "Mempool"
                namesForMempool
                severityMempool
                trBase trForward mbTrEKG
    fTr    <- mkStandardTracer
                "Forge"
                namesForForge
                severityForge
                trBase trForward mbTrEKG
    btTr   <- mkStandardTracer
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                trBase trForward mbTrEKG
    kacTr  <- mkStandardTracer
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                trBase trForward mbTrEKG
    tcsTr  <-  mkStandardTracer
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                trBase trForward mbTrEKG
    ttsTr  <-  mkStandardTracer
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                trBase trForward mbTrEKG
    tsqTr  <-  mkStandardTracer
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                trBase trForward mbTrEKG
    tcsnTr <-  mkStandardTracer
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                trBase trForward mbTrEKG
    tcssTr <-  mkStandardTracer
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                trBase trForward mbTrEKG
    tbfTr  <-  mkStandardTracer
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                trBase trForward mbTrEKG
    tbfsTr <-  mkStandardTracer
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                trBase trForward mbTrEKG
    tsnTr  <-  mkStandardTracer
                "TxSubmissionTracer"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                trBase trForward mbTrEKG
    ts2nTr  <-  mkStandardTracer
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                trBase trForward mbTrEKG
    ipsTr   <-  mkStandardTracer
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                trBase trForward mbTrEKG
    dnssTr  <-  mkStandardTracer
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                trBase trForward mbTrEKG
    dnsrTr  <-  mkStandardTracer
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                trBase trForward mbTrEKG
    errpTr  <-  mkStandardTracer
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                trBase trForward mbTrEKG
    lerrpTr <-  mkStandardTracer
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                trBase trForward mbTrEKG
    apTr    <-  mkStandardTracer
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                trBase trForward mbTrEKG
    muxTr   <-  mkStandardTracer
                "Mux"
                namesForMux
                severityMux
                trBase trForward mbTrEKG
    muxLTr   <-  mkStandardTracer
                "MuxLocal"
                namesForMux
                severityMux
                trBase trForward mbTrEKG
    hsTr   <-  mkStandardTracer
                "Handshake"
                namesForHandshake
                severityHandshake
                trBase trForward mbTrEKG
    lhsTr  <-  mkStandardTracer
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                trBase trForward mbTrEKG
    diTr   <-  mkStandardTracer
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                trBase trForward mbTrEKG

    configureTracers emptyTraceConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers emptyTraceConfig docChainSyncClientEvent [cscTr]
    configureTracers emptyTraceConfig docChainSyncServerEvent [csshTr]
    configureTracers emptyTraceConfig docChainSyncServerEvent [cssbTr]
    configureTracers emptyTraceConfig docBlockFetchDecision   [bfdTr]
    configureTracers emptyTraceConfig docBlockFetchClient     [bfcTr]
    configureTracers emptyTraceConfig docBlockFetchServer     [bfsTr]
    configureTracers emptyTraceConfig docTxInbound            [txiTr]
    configureTracers emptyTraceConfig docLocalTxSubmissionServer [ltxsTr]
    configureTracers emptyTraceConfig docMempool              [mpTr]
    configureTracers emptyTraceConfig docForge                [fTr]
    configureTracers emptyTraceConfig docBlockchainTime       [btTr]
    configureTracers emptyTraceConfig docKeepAliveClient      [kacTr]
    configureTracers emptyTraceConfig docTChainSync           [tcsTr]
    configureTracers emptyTraceConfig docTTxSubmission        [ttsTr]
    configureTracers emptyTraceConfig docTStateQuery          [tsqTr]
    configureTracers emptyTraceConfig docTChainSync           [tcsnTr]
    configureTracers emptyTraceConfig docTChainSync           [tcssTr]
    configureTracers emptyTraceConfig docTBlockFetch          [tbfTr]
    configureTracers emptyTraceConfig docTBlockFetch          [tbfsTr]
    configureTracers emptyTraceConfig docTTxSubmissionNode    [tsnTr]
    configureTracers emptyTraceConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers emptyTraceConfig docIPSubscription       [ipsTr]
    configureTracers emptyTraceConfig docDNSSubscription      [dnssTr]
    configureTracers emptyTraceConfig docDNSResolver          [dnsrTr]
    configureTracers emptyTraceConfig docErrorPolicy          [errpTr]
    configureTracers emptyTraceConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers emptyTraceConfig docAcceptPolicy         [apTr]
    configureTracers emptyTraceConfig docMux                  [muxTr]
    configureTracers emptyTraceConfig docMux                  [muxLTr]
    configureTracers emptyTraceConfig docHandshake            [hsTr]
    configureTracers emptyTraceConfig docLocalHandshake       [lhsTr]
    configureTracers emptyTraceConfig docDiffusionInit        [diTr]


    pure Tracers
      { chainDBTracer = Tracer (traceWith cdbmTr)
      , consensusTracers = Consensus.Tracers
        { Consensus.chainSyncClientTracer = Tracer (traceWith cscTr)
        , Consensus.chainSyncServerHeaderTracer = Tracer (traceWith csshTr)
        , Consensus.chainSyncServerBlockTracer = Tracer (traceWith cssbTr)
        , Consensus.blockFetchDecisionTracer = Tracer (traceWith bfdTr)
        , Consensus.blockFetchClientTracer = Tracer (traceWith bfcTr)
        , Consensus.blockFetchServerTracer = Tracer (traceWith bfsTr)
        , Consensus.forgeStateInfoTracer =
            Tracer (traceWith (traceAsKESInfo (Proxy @blk) fsiTr))
        , Consensus.txInboundTracer = Tracer (traceWith txiTr)
        , Consensus.txOutboundTracer = Tracer (traceWith txoTr)
        , Consensus.localTxSubmissionServerTracer = Tracer (traceWith ltxsTr)
        , Consensus.mempoolTracer = Tracer (traceWith mpTr)
        , Consensus.forgeTracer = Tracer (traceWith fTr)
        , Consensus.blockchainTimeTracer = Tracer (traceWith btTr)
        , Consensus.keepAliveClientTracer = Tracer (traceWith kacTr)
        }
      , nodeToClientTracers = NtC.Tracers
        { NtC.tChainSyncTracer = Tracer (traceWith tcsTr)
        , NtC.tTxSubmissionTracer = Tracer (traceWith ttsTr)
        , NtC.tStateQueryTracer = Tracer (traceWith tsqTr)
        }
      , nodeToNodeTracers = NtN.Tracers
        { NtN.tChainSyncTracer = Tracer (traceWith tcsnTr)
        , NtN.tChainSyncSerialisedTracer = Tracer (traceWith tcssTr)
        , NtN.tBlockFetchTracer = Tracer (traceWith tbfTr)
        , NtN.tBlockFetchSerialisedTracer = Tracer (traceWith tbfsTr)
        , NtN.tTxSubmissionTracer = Tracer (traceWith tsnTr)
        , NtN.tTxSubmission2Tracer = Tracer (traceWith ts2nTr)
        }
      , ipSubscriptionTracer = Tracer (traceWith ipsTr)
      , dnsSubscriptionTracer= Tracer (traceWith dnssTr)
      , dnsResolverTracer = Tracer (traceWith dnsrTr)
      , errorPolicyTracer = Tracer (traceWith errpTr)
      , localErrorPolicyTracer = Tracer (traceWith lerrpTr)
      , acceptPolicyTracer = Tracer (traceWith apTr)
      , muxTracer = Tracer (traceWith muxTr)
      , muxLocalTracer = Tracer (traceWith muxLTr)
      , handshakeTracer = Tracer (traceWith hsTr)
      , localHandshakeTracer = Tracer (traceWith lhsTr)
      , diffusionInitializationTracer = Tracer (traceWith diTr)
    }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ _ _ _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect
