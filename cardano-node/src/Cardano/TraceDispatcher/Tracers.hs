{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE PackageImports       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-unused-imports  #-}


module Cardano.TraceDispatcher.Tracers
  ( mkDispatchTracers
  , docTracers
  ) where

import           Cardano.Prelude
import qualified Data.Text.IO as T
import           Debug.Trace

import           Cardano.Logging
import           Cardano.TraceDispatcher.ChainDBTracer
import           Cardano.TraceDispatcher.OrphanInstances.Consensus ()

import           Cardano.Node.Configuration.Logging (EKGDirect)

import qualified Cardano.BM.Data.Trace as Old
import           Cardano.Tracing.Config (TraceOptions (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.ConvertTxId
import           Cardano.Tracing.Kernel (NodeKernelData)
import           Cardano.Tracing.Metrics (HasKESInfo, HasKESMetricsData)
import           Cardano.Tracing.Tracers
import           "contra-tracer" Control.Tracer (Tracer (..), nullTracer)

import           Ouroboros.Consensus.Block (ConvertRawHash, HasHeader, Header)
import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)
import           Ouroboros.Consensus.Byron.Ledger.Config (BlockConfig)
import           Ouroboros.Consensus.Ledger.Inspect (InspectLedger,
                     LedgerUpdate, LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsProtocol
                     (LedgerSupportsProtocol)
import qualified Ouroboros.Consensus.Network.NodeToClient as NodeToClient
import qualified Ouroboros.Consensus.Network.NodeToNode as NodeToNode
import qualified Ouroboros.Consensus.Node.Run as Consensus (RunNode)
import qualified Ouroboros.Consensus.Node.Tracers as Consensus
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

chainDBMachineTracer ::
  ( LogFormatting (LedgerUpdate blk)
  , LogFormatting (LedgerWarning blk)
  , LogFormatting (Header blk)
  , ConvertRawHash blk
  , LedgerSupportsProtocol blk
  , InspectLedger blk
  )
  => IO (Trace IO (ChainDB.TraceEvent blk))
chainDBMachineTracer = do
    trBase <- standardTracer Nothing
    tr <- humanFormatter True "cardano" trBase
    let cdbmTrNs = appendName "chainDB" $ appendName "node" tr
    configureTracers emptyTraceConfig docChainDBTraceEvent [cdbmTrNs]
    pure cdbmTrNs

docTracers :: IO ()
docTracers = do
  cdbmTr <- chainDBMachineTracer
  bl1  <- documentMarkdown
            (docChainDBTraceEvent :: Documented (ChainDB.TraceEvent ByronBlock))
            [cdbmTr]
  T.writeFile "/home/yupanqui/IOHK/CardanoLogging.md" (buildersToText bl1)


-- | Tracers for all system components.
--
mkDispatchTracers
  :: forall peer localPeer blk.
  ( Consensus.RunNode blk
  , LogFormatting (LedgerWarning blk)
  , HasKESMetricsData blk
  , HasKESInfo blk
  , TraceConstraints blk
  , Show peer, Eq peer
  , Show localPeer
  )
  => BlockConfig blk
  -> TraceOptions
  -> Old.Trace IO Text
  -> NodeKernelData blk
  -> Maybe EKGDirect
  -> IO (Tracers peer localPeer blk)
mkDispatchTracers _blockConfig (TraceDispatcher _trSel) _tr _nodeKern _ekgDirect = do
  cdbmTr <- trace "!!!mkDispatchTracers" $ chainDBMachineTracer
  pure Tracers
    { chainDBTracer = Tracer (traceWith cdbmTr)

    , consensusTracers = Consensus.Tracers
      { Consensus.chainSyncClientTracer = nullTracer
      , Consensus.chainSyncServerHeaderTracer = nullTracer
      , Consensus.chainSyncServerBlockTracer = nullTracer
      , Consensus.blockFetchDecisionTracer = nullTracer
      , Consensus.blockFetchClientTracer = nullTracer
      , Consensus.blockFetchServerTracer = nullTracer
      , Consensus.forgeStateInfoTracer = nullTracer
      , Consensus.txInboundTracer = nullTracer
      , Consensus.txOutboundTracer = nullTracer
      , Consensus.localTxSubmissionServerTracer = nullTracer
      , Consensus.mempoolTracer = nullTracer
      , Consensus.forgeTracer = nullTracer
      , Consensus.blockchainTimeTracer = nullTracer
      , Consensus.keepAliveClientTracer = nullTracer
      }
    , nodeToClientTracers = NodeToClient.Tracers
      { NodeToClient.tChainSyncTracer = nullTracer
      , NodeToClient.tTxSubmissionTracer = nullTracer
      , NodeToClient.tStateQueryTracer = nullTracer
      }
    , nodeToNodeTracers = NodeToNode.Tracers
      { NodeToNode.tChainSyncTracer = nullTracer
      , NodeToNode.tChainSyncSerialisedTracer = nullTracer
      , NodeToNode.tBlockFetchTracer = nullTracer
      , NodeToNode.tBlockFetchSerialisedTracer = nullTracer
      , NodeToNode.tTxSubmissionTracer = nullTracer
      , NodeToNode.tTxSubmission2Tracer = nullTracer
      }
    , ipSubscriptionTracer = nullTracer
    , dnsSubscriptionTracer= nullTracer
    , dnsResolverTracer = nullTracer
    , errorPolicyTracer = nullTracer
    , localErrorPolicyTracer = nullTracer
    , acceptPolicyTracer = nullTracer
    , muxTracer = nullTracer
    , muxLocalTracer = nullTracer
    , handshakeTracer = nullTracer
    , localHandshakeTracer = nullTracer
    , diffusionInitializationTracer = nullTracer
  }

mkDispatchTracers blockConfig tOpts tr nodeKern ekgDirect =
  mkTracers blockConfig tOpts tr nodeKern ekgDirect
