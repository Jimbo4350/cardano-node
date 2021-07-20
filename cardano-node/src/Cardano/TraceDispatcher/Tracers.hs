{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE PackageImports        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import qualified Data.Text.IO as T
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket

import           Cardano.Logging
import           Cardano.Prelude hiding (trace)
import           Cardano.TraceDispatcher.BasicInfo.Combinators
import           Cardano.TraceDispatcher.BasicInfo.Types (BasicInfo)
import           Cardano.TraceDispatcher.ChainDB.Combinators
import           Cardano.TraceDispatcher.ChainDB.Docu
import           Cardano.TraceDispatcher.Consensus.Combinators
import           Cardano.TraceDispatcher.Consensus.Docu
import           Cardano.TraceDispatcher.Consensus.ForgingThreadStats
                     (docForgeStats, forgeThreadStats, ForgeThreadStats)
import           Cardano.TraceDispatcher.Consensus.StateInfo
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Network.Combinators
import           Cardano.TraceDispatcher.Network.Docu
import           Cardano.TraceDispatcher.Peer
import           Cardano.TraceDispatcher.Network.Formatting ()
import           Cardano.TraceDispatcher.Resources (namesForResources,
                     severityResources, startResourceTracer)
import qualified "trace-dispatcher" Control.Tracer as NT
-- import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck


import           Cardano.Node.Configuration.Logging (EKGDirect)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.OrphanInstances.Common (ToObject)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..))

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.BlockchainTime.WallClock.Util
                     (TraceBlockchainTimeEvent (..))
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr, GenTx,
                     GenTxId)
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


import           Ouroboros.Network.Block (Point (..), Serialised, Tip)
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

import           Debug.Trace

type Peer = NtN.ConnectionId Socket.SockAddr

data MessageOrLimit m = Message m | Limit LimitingMessage

instance (LogFormatting m) => LogFormatting (MessageOrLimit m) where
  forMachine dtal (Message m) = forMachine dtal m
  forMachine dtal (Limit m)   = forMachine dtal m
  forHuman (Message m) = forHuman m
  forHuman (Limit m)   = forHuman m
  asMetrics (Message m) = asMetrics m
  asMetrics (Limit m)   = asMetrics m

-- | Construct a tracer according to the requirements for cardano node.
--
-- The tracer gets a 'name', which is appended to its namespace.
--
-- The tracer gets a 'namesFor', 'severityFor' and 'privacyFor' function
-- as arguments, to set the logging context accordingly.
--
-- The tracer gets the backends: 'trStdout', 'trForward' and 'mbTrEkg'
-- as arguments.
--
-- The returned tracer need to be configured for the specification of
-- filtering, detailLevel, frequencyLimiting and backends with formatting before use.
mkCardanoTracer :: forall evt.
     LogFormatting evt
  => Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> IO (Trace IO evt)
mkCardanoTracer name namesFor severityFor privacyFor trStdout trForward mbTrEkg =
    mkCardanoTracer' name namesFor severityFor privacyFor
      trStdout trForward mbTrEkg noHook
  where
    noHook :: Trace IO evt -> IO (Trace IO evt)
    noHook tr = pure tr

-- | Adds the possibility to add special tracers via the hook function
mkCardanoTracer' :: forall evt evt1.
     LogFormatting evt1
  => Text
  -> (evt -> [Text])
  -> (evt -> SeverityS)
  -> (evt -> Privacy)
  -> Trace IO FormattedMessage
  -> Trace IO FormattedMessage
  -> Maybe (Trace IO FormattedMessage)
  -> (Trace IO evt1 -> IO (Trace IO evt))
  -> IO (Trace IO evt)
mkCardanoTracer' name namesFor severityFor privacyFor
  trStdout trForward mbTrEkg hook = do
    tr    <- withBackendsFromConfig backendsAndFormat
    tr'   <- withLimitersFromConfig (contramap Message tr) (contramap Limit tr)
    tr''  <- hook tr'
    addContextAndFilter tr''
  where
    addContextAndFilter :: Trace IO evt -> IO (Trace IO evt)
    addContextAndFilter tr = do
      tr'  <- withDetailsFromConfig tr
      tr'' <- filterSeverityFromConfig tr'
      pure $ withNamesAppended namesFor
            $ appendName name
              $ appendName "Node"
                $ withSeverity severityFor
                  $ withPrivacy privacyFor
                    tr''

    backendsAndFormat ::
         Maybe [BackendConfig]
      -> Trace m x
      -> IO (Trace IO (MessageOrLimit evt1))
    backendsAndFormat mbBackends _ =
      let backends = case mbBackends of
                        Just b -> b
                        Nothing -> [EKGBackend, Forwarder, Stdout HumanFormatColoured]
      in do
        mbEkgTrace     <- case mbTrEkg of
                            Nothing -> pure Nothing
                            Just ekgTrace ->
                              if elem EKGBackend backends
                                then liftM Just
                                      (metricsFormatter "Cardano" ekgTrace)
                                else pure Nothing
        mbForwardTrace <- if elem Forwarder backends
                            then liftM (Just . filterTraceByPrivacy (Just Public))
                                  (forwardFormatter "Cardano" trForward)
                            else pure Nothing
        mbStdoutTrace  <-  if elem (Stdout HumanFormatColoured) backends
                            then liftM Just
                                (humanFormatter True "Cardano" trStdout)
                            else if elem (Stdout HumanFormatUncoloured) backends
                              then liftM Just
                                  (humanFormatter False "Cardano" trStdout)
                              else if elem (Stdout MachineFormat) backends
                                then liftM Just
                                  (machineFormatter "Cardano" trStdout)
                                else pure Nothing
        case mbEkgTrace <> mbForwardTrace <> mbStdoutTrace of
          Nothing -> pure $ Trace NT.nullTracer
          Just tr -> pure (preFormatted backends tr)


-- | Construct tracers for all system components.
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
  -> [BasicInfo]
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr nodeKernel _ekgDirect
  trBase trForward mbTrEKG trConfig basicInfos = do
    trace ("TraceConfig " <> show trConfig) $ pure ()
    cdbmTr <- mkCardanoTracer
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                trBase trForward mbTrEKG
    cscTr  <- mkCardanoTracer
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
                trBase trForward mbTrEKG
    csshTr <- mkCardanoTracer
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
                trBase trForward mbTrEKG
    cssbTr <- mkCardanoTracer
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
                trBase trForward mbTrEKG
    bfdTr  <- mkCardanoTracer
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
                trBase trForward mbTrEKG
    bfcTr  <- mkCardanoTracer
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
                trBase trForward mbTrEKG
    bfsTr  <- mkCardanoTracer
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
                trBase trForward mbTrEKG
    fsiTr  <- mkCardanoTracer
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
                trBase trForward mbTrEKG
    txiTr  <- mkCardanoTracer
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
                trBase trForward mbTrEKG
    txoTr  <- mkCardanoTracer
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
                trBase trForward mbTrEKG
    ltxsTr <- mkCardanoTracer
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
                trBase trForward mbTrEKG
    mpTr   <- mkCardanoTracer
                "Mempool"
                namesForMempool
                severityMempool
                allPublic
                trBase trForward mbTrEKG
    fTr    <- mkCardanoTracer'
                "Forge"
                namesForForge
                severityForge
                allPublic
                trBase trForward mbTrEKG
                (forgeTracerTransform nodeKernel)
    fSttTr <- mkCardanoTracer'
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                trBase trForward mbTrEKG
                forgeThreadStats
    btTr   <- mkCardanoTracer
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
                trBase trForward mbTrEKG
    kacTr  <- mkCardanoTracer
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
                trBase trForward mbTrEKG
    tcsTr  <-  mkCardanoTracer
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
                trBase trForward mbTrEKG
    ttsTr  <-  mkCardanoTracer
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
                trBase trForward mbTrEKG
    tsqTr  <-  mkCardanoTracer
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
                trBase trForward mbTrEKG
    tcsnTr <-  mkCardanoTracer
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
                trBase trForward mbTrEKG
    tcssTr <-  mkCardanoTracer
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
                trBase trForward mbTrEKG
    tbfTr  <-  mkCardanoTracer
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
                trBase trForward mbTrEKG
    tbfsTr <-  mkCardanoTracer
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
                trBase trForward mbTrEKG
    tsnTr  <-  mkCardanoTracer
                "TxSubmissionTracer"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
                trBase trForward mbTrEKG
    ts2nTr  <-  mkCardanoTracer
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
                trBase trForward mbTrEKG
    ipsTr   <-  mkCardanoTracer
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
                trBase trForward mbTrEKG
    dnssTr  <-  mkCardanoTracer
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
                trBase trForward mbTrEKG
    dnsrTr  <-  mkCardanoTracer
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
                trBase trForward mbTrEKG
    errpTr  <-  mkCardanoTracer
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
                trBase trForward mbTrEKG
    lerrpTr <-  mkCardanoTracer
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
                trBase trForward mbTrEKG
    apTr    <-  mkCardanoTracer
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
                trBase trForward mbTrEKG
    muxTr   <-  mkCardanoTracer
                "Mux"
                namesForMux
                severityMux
                allPublic
                trBase trForward mbTrEKG
    muxLTr   <-  mkCardanoTracer
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
                trBase trForward mbTrEKG
    hsTr   <-  mkCardanoTracer
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
                trBase trForward mbTrEKG
    lhsTr  <-  mkCardanoTracer
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
                trBase trForward mbTrEKG
    diTr   <-  mkCardanoTracer
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
                trBase trForward mbTrEKG
    rsTr   <- mkCardanoTracer
                "Resources"
                (\ _ -> [])
                (\ _ -> Info)
                allPublic
                trBase trForward mbTrEKG
    biTr   <- mkCardanoTracer
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
                trBase trForward mbTrEKG
    pTr   <- mkCardanoTracer
                "Peers"
                namesForPeers
                severityPeers
                allPublic
                trBase trForward mbTrEKG

    configureTracers trConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers trConfig docChainSyncClientEvent [cscTr]
    configureTracers trConfig docChainSyncServerEvent [csshTr]
    configureTracers trConfig docChainSyncServerEvent [cssbTr]
    configureTracers trConfig docBlockFetchDecision   [bfdTr]
    configureTracers trConfig docBlockFetchClient     [bfcTr]
    configureTracers trConfig docBlockFetchServer     [bfsTr]
    configureTracers trConfig docForgeStateInfo       [fsiTr]
    configureTracers trConfig docTxInbound            [txiTr]
    configureTracers trConfig docTxOutbound           [txoTr]
    configureTracers trConfig docLocalTxSubmissionServer [ltxsTr]
    configureTracers trConfig docMempool              [mpTr]
    configureTracers trConfig docForge                [fTr, fSttTr]
    configureTracers trConfig docBlockchainTime       [btTr]
    configureTracers trConfig docKeepAliveClient      [kacTr]
    configureTracers trConfig docTChainSync           [tcsTr]
    configureTracers trConfig docTTxSubmission        [ttsTr]
    configureTracers trConfig docTStateQuery          [tsqTr]
    configureTracers trConfig docTChainSync           [tcsnTr]
    configureTracers trConfig docTChainSync           [tcssTr]
    configureTracers trConfig docTBlockFetch          [tbfTr]
    configureTracers trConfig docTBlockFetch          [tbfsTr]
    configureTracers trConfig docTTxSubmissionNode    [tsnTr]
    configureTracers trConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers trConfig docIPSubscription       [ipsTr]
    configureTracers trConfig docDNSSubscription      [dnssTr]
    configureTracers trConfig docDNSResolver          [dnsrTr]
    configureTracers trConfig docErrorPolicy          [errpTr]
    configureTracers trConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers trConfig docAcceptPolicy         [apTr]
    configureTracers trConfig docMux                  [muxTr]
    configureTracers trConfig docMux                  [muxLTr]
    configureTracers trConfig docHandshake            [hsTr]
    configureTracers trConfig docLocalHandshake       [lhsTr]
    configureTracers trConfig docDiffusionInit        [diTr]
    configureTracers trConfig docResourceStats        [rsTr]
    configureTracers trConfig docBasicInfo            [biTr]
    configureTracers trConfig docPeers                [pTr]

-- -- TODO JNF Code for debugging frequency limiting
--     void . forkIO $
--       sendContinously
--         0.1
--         cdbmTr
--         (ChainDB.TraceOpenEvent
--           (ChainDB.OpenedDB (Point Origin) (Point Origin)))
-- -- End of  debugging code

    mapM_ (traceWith biTr) basicInfos
    startResourceTracer rsTr
    startPeerTracer pTr nodeKernel

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
        , Consensus.forgeTracer =
            Tracer (traceWith (contramap Left fTr))
            <> Tracer (traceWith (contramap Left fSttTr))
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
      , basicInfoTracer = Tracer (traceWith biTr)
    }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect _ _ _ _ _ =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect

-- -- TODO JNF Code for debugging frequency limiting
-- sendContinously ::
--      Double
--   -> Trace IO m
--   -> m
--   -> IO ()
-- sendContinously delay tracer message = do
--   threadDelay (round (delay * 1000000.0))
--   traceWith tracer message
--   sendContinously delay tracer message
-- -- End of  debugging code

docTracers :: forall blk t.
  ( Show t
  , forall result. Show (Query blk result)
  , TraceConstraints blk
  , LogFormatting (ChainDB.InvalidBlockReason blk)
  , LedgerSupportsProtocol blk
  , Consensus.RunNode blk
  )
  => FilePath
  -> FilePath
  -> Proxy blk
  -> IO ()
docTracers configFileName outputFileName _ = do
    trConfig   <- readConfiguration configFileName
    trBase     <- standardTracer Nothing
    trForward  <- forwardTracer trConfig
    mbTrEKG :: Maybe (Trace IO FormattedMessage) <-
                  liftM Just (ekgTracer (Right undefined))
    cdbmTr <- mkCardanoTracer
                "ChainDB"
                namesForChainDBTraceEvents
                severityChainDB
                allPublic
                trBase trForward mbTrEKG
    cscTr  <- mkCardanoTracer
                "ChainSyncClient"
                namesForChainSyncClientEvent
                severityChainSyncClientEvent
                allPublic
                trBase trForward mbTrEKG
    csshTr <- mkCardanoTracer
                "ChainSyncServerHeader"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
                trBase trForward mbTrEKG
    cssbTr <- mkCardanoTracer
                "ChainSyncServerBlock"
                namesForChainSyncServerEvent
                severityChainSyncServerEvent
                allPublic
                trBase trForward mbTrEKG
    bfdTr  <- mkCardanoTracer
                "BlockFetchDecision"
                namesForBlockFetchDecision
                severityBlockFetchDecision
                allConfidential
                trBase trForward mbTrEKG
    bfcTr  <- mkCardanoTracer
                "BlockFetchClient"
                namesForBlockFetchClient
                severityBlockFetchClient
                allPublic
                trBase trForward mbTrEKG
    bfsTr  <- mkCardanoTracer
                "BlockFetchServer"
                namesForBlockFetchServer
                severityBlockFetchServer
                allPublic
                trBase trForward mbTrEKG
    fsiTr  <- mkCardanoTracer
                "ForgeStateInfo"
                namesForStateInfo
                severityStateInfo
                allPublic
                trBase trForward mbTrEKG
    txiTr  <- mkCardanoTracer
                "TxInbound"
                namesForTxInbound
                severityTxInbound
                allPublic
                trBase trForward mbTrEKG
    txoTr  <- mkCardanoTracer
                "TxOutbound"
                namesForTxOutbound
                severityTxOutbound
                allPublic
                trBase trForward mbTrEKG
    ltxsTr <- mkCardanoTracer
                "LocalTxSubmissionServer"
                namesForLocalTxSubmissionServer
                severityLocalTxSubmissionServer
                allPublic
                trBase trForward mbTrEKG
    -- mpTr   <- mkCardanoTracer
    --             "Mempool"
    --             namesForMempool
    --             severityMempool
    --             allPublic
    --             trBase trForward mbTrEKG
    fTr    <- mkCardanoTracer
                "Forge"
                namesForForge
                severityForge
                allPublic
                trBase trForward mbTrEKG
    fSttTr <- mkCardanoTracer'
                "ForgeStats"
                namesForForge
                severityForge
                allPublic
                trBase trForward mbTrEKG
                forgeThreadStats
    btTr   <- mkCardanoTracer
                "BlockchainTime"
                namesForBlockchainTime
                severityBlockchainTime
                allPublic
                trBase trForward mbTrEKG
    kacTr  <- mkCardanoTracer
                "KeepAliveClient"
                namesForKeepAliveClient
                severityKeepAliveClient
                allPublic
                trBase trForward mbTrEKG
    tcsTr  <-  mkCardanoTracer
                "ChainSyncClient"
                namesForTChainSync
                severityTChainSync
                allPublic
                trBase trForward mbTrEKG
    ttsTr  <-  mkCardanoTracer
                "TxSubmissionClient"
                namesForTTxSubmission
                severityTTxSubmission
                allPublic
                trBase trForward mbTrEKG
    tsqTr  <-  mkCardanoTracer
                "StateQueryClient"
                namesForTStateQuery
                severityTStateQuery
                allPublic
                trBase trForward mbTrEKG
    tcsnTr <-  mkCardanoTracer
                "ChainSyncNode"
                namesForTChainSyncNode
                severityTChainSyncNode
                allPublic
                trBase trForward mbTrEKG
    tcssTr <-  mkCardanoTracer
                "ChainSyncSerialised"
                namesForTChainSyncSerialised
                severityTChainSyncSerialised
                allPublic
                trBase trForward mbTrEKG
    tbfTr  <-  mkCardanoTracer
                "BlockFetch"
                namesForTBlockFetch
                severityTBlockFetch
                allPublic
                trBase trForward mbTrEKG
    tbfsTr <-  mkCardanoTracer
                "BlockFetchSerialised"
                namesForTBlockFetchSerialised
                severityTBlockFetchSerialised
                allPublic
                trBase trForward mbTrEKG
    tsnTr  <-  mkCardanoTracer
                "TxSubmissionTracer"
                namesForTxSubmissionNode
                severityTxSubmissionNode
                allPublic
                trBase trForward mbTrEKG
    ts2nTr  <-  mkCardanoTracer
                "TxSubmission2"
                namesForTxSubmission2Node
                severityTxSubmission2Node
                allPublic
                trBase trForward mbTrEKG
    ipsTr   <-  mkCardanoTracer
                "IpSubscription"
                namesForIPSubscription
                severityIPSubscription
                allPublic
                trBase trForward mbTrEKG
    dnssTr  <-  mkCardanoTracer
                "DnsSubscription"
                namesForDNSSubscription
                severityDNSSubscription
                allPublic
                trBase trForward mbTrEKG
    dnsrTr  <-  mkCardanoTracer
                "DNSResolver"
                namesForDNSResolver
                severityDNSResolver
                allPublic
                trBase trForward mbTrEKG
    errpTr  <-  mkCardanoTracer
                "ErrorPolicy"
                namesForErrorPolicy
                severityErrorPolicy
                allPublic
                trBase trForward mbTrEKG
    lerrpTr <-  mkCardanoTracer
                "LocalErrorPolicy"
                namesForLocalErrorPolicy
                severityLocalErrorPolicy
                allPublic
                trBase trForward mbTrEKG
    apTr    <-  mkCardanoTracer
                "AcceptPolicy"
                namesForAcceptPolicy
                severityAcceptPolicy
                allPublic
                trBase trForward mbTrEKG
    muxTr   <-  mkCardanoTracer
                "Mux"
                namesForMux
                severityMux
                allPublic
                trBase trForward mbTrEKG
    muxLTr   <-  mkCardanoTracer
                "MuxLocal"
                namesForMux
                severityMux
                allPublic
                trBase trForward mbTrEKG
    hsTr   <-  mkCardanoTracer
                "Handshake"
                namesForHandshake
                severityHandshake
                allPublic
                trBase trForward mbTrEKG
    lhsTr  <-  mkCardanoTracer
                "LocalHandshake"
                namesForLocalHandshake
                severityLocalHandshake
                allPublic
                trBase trForward mbTrEKG
    diTr   <-  mkCardanoTracer
                "DiffusionInit"
                namesForDiffusionInit
                severityDiffusionInit
                allPublic
                trBase trForward mbTrEKG
    rsTr   <- mkCardanoTracer
                "Resources"
                namesForResources
                severityResources
                allPublic
                trBase trForward mbTrEKG
    biTr   <- mkCardanoTracer
                "BasicInfo"
                namesForBasicInfo
                severityBasicInfo
                allPublic
                trBase trForward mbTrEKG

    configureTracers trConfig docChainDBTraceEvent    [cdbmTr]
    configureTracers trConfig docChainSyncClientEvent [cscTr]
    configureTracers trConfig docChainSyncServerEvent [csshTr]
    configureTracers trConfig docChainSyncServerEvent [cssbTr]
    configureTracers trConfig docBlockFetchDecision   [bfdTr]
    configureTracers trConfig docBlockFetchClient     [bfcTr]
    configureTracers trConfig docBlockFetchServer     [bfsTr]
    configureTracers trConfig docForgeStateInfo       [fsiTr]
    configureTracers trConfig docTxInbound            [txiTr]
    configureTracers trConfig docTxOutbound           [txoTr]
    configureTracers trConfig docLocalTxSubmissionServer [ltxsTr]
--    configureTracers trConfig docMempool              [mpTr]
    configureTracers trConfig docForge                [fTr, fSttTr]
    configureTracers trConfig docBlockchainTime       [btTr]
    configureTracers trConfig docKeepAliveClient      [kacTr]
    configureTracers trConfig docTChainSync           [tcsTr]
    configureTracers trConfig docTTxSubmission        [ttsTr]
    configureTracers trConfig docTStateQuery          [tsqTr]
    configureTracers trConfig docTChainSync           [tcsnTr]
    configureTracers trConfig docTChainSync           [tcssTr]
    configureTracers trConfig docTBlockFetch          [tbfTr]
    configureTracers trConfig docTBlockFetch          [tbfsTr]
    configureTracers trConfig docTTxSubmissionNode    [tsnTr]
    configureTracers trConfig docTTxSubmission2Node   [ts2nTr]
    configureTracers trConfig docIPSubscription       [ipsTr]
    configureTracers trConfig docDNSSubscription      [dnssTr]
    configureTracers trConfig docDNSResolver          [dnsrTr]
    configureTracers trConfig docErrorPolicy          [errpTr]
    configureTracers trConfig docLocalErrorPolicy     [lerrpTr]
    configureTracers trConfig docAcceptPolicy         [apTr]
    configureTracers trConfig docMux                  [muxTr]
    configureTracers trConfig docMux                  [muxLTr]
    configureTracers trConfig docHandshake            [hsTr]
    configureTracers trConfig docLocalHandshake       [lhsTr]
    configureTracers trConfig docDiffusionInit        [diTr]
    configureTracers trConfig docResourceStats        [rsTr]
    configureTracers trConfig docBasicInfo            [biTr]

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
    bfsTrDoc    <- documentMarkdown
                (docBlockFetchServer :: Documented
                  (TraceBlockFetchServerEvent blk))
                [bfsTr]
    -- fsiTrDoc    <- documentMarkdown
    --             (docForgeStateInfo :: Documented
    --               (Consensus.TraceLabelCreds HotKey.KESInfo))
    --             [fsiTr]
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
    -- mpTrDoc    <- documentMarkdown
    --             (docMempool :: Documented
    --               (TraceEventMempool blk))
    --             [mpTr]
    fTrDoc    <- documentMarkdown
                (docForge :: Documented
                  (ForgeTracerType blk))
                [fTr]
    -- TODO JNF Docu for forgeThreadStats
    -- fSttTr'      <- forgeThreadStats fSttTr
    -- fSttTrDoc    <- documentMarkdown
    --             (docForgeStats :: Documented
    --               ForgeThreadStats)
    --             [unfold fSttTr]
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
    ipsTrDoc   <-  documentMarkdown
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
    rsTrDoc      <-  documentMarkdown
                    (docResourceStats :: Documented ResourceStats)
                    [rsTr]
    biTrDoc      <-  documentMarkdown
                    (docBasicInfo :: Documented BasicInfo)
                    [biTr]

    let bl = cdbmTrDoc
            ++ cscTrDoc
            ++ csshTrDoc
            ++ cssbTrDoc
            ++ bfdTrDoc
            ++ bfcTrDoc
            ++ bfsTrDoc
--            ++ fsiTrDoc
            ++ txiTrDoc
            ++ txoTrDoc
            ++ ltxsTrDoc
--            ++ mpTrDoc
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
            ++ ipsTrDoc
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
            ++ rsTrDoc
            ++ biTrDoc

    res <- buildersToText bl trConfig
    T.writeFile outputFileName res
    pure ()
