{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}

module Cardano.TraceDispatcher.Consensus.StartLeadershipCheck
  (
    TraceStartLeadershipCheckPlus (..)
  , ForgeTracerType
  , forgeTracerTransform
  , LedgerQueriesX (..)
  ) where


import           Cardano.Logging
import           Cardano.Prelude
import qualified "trace-dispatcher" Control.Tracer as T
import           Data.IORef (readIORef)
import qualified Data.Map.Strict as Map

import qualified Ouroboros.Network.AnchoredFragment as AF
import           Ouroboros.Network.Block (BlockNo (..), blockNo, unBlockNo)
import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteConnectionId)

import           Ouroboros.Consensus.Block (SlotNo (..))
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron
import qualified Ouroboros.Consensus.Cardano as Cardano
import qualified Ouroboros.Consensus.Cardano.Block as Cardano
import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.Embed.Unary
import           Ouroboros.Consensus.Ledger.Abstract (IsLedger)
import           Ouroboros.Consensus.Ledger.Extended (ExtLedgerState,
                     ledgerState)
import           Ouroboros.Consensus.Node (NodeKernel (..))
import           Ouroboros.Consensus.Node.Tracers
import qualified Ouroboros.Consensus.Storage.ChainDB as ChainDB

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import           Cardano.Slotting.Slot (fromWithOrigin)

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import           Shelley.Spec.Ledger.BaseTypes (StrictMaybe (..), fromSMaybe)
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import           Cardano.Tracing.Kernel (NodeKernelData (..))


type ForgeTracerType blk = Either (TraceLabelCreds (TraceForgeEvent blk))
                                  (TraceLabelCreds TraceStartLeadershipCheckPlus)

data TraceStartLeadershipCheckPlus =
  TraceStartLeadershipCheckPlus {
        tsSlotNo       :: SlotNo
      , tsUtxoSize     :: Int
      , tsDelegMapSize :: Int
      , tsChainDensity :: Double
    }

-- newtype NodeKernelData blk =
--   NodeKernelData
--   { _unNodeKernelData :: IORef (StrictMaybe (NodeKernel IO RemoteConnectionId LocalConnectionId blk))
--   }

forgeTracerTransform ::
  (  IsLedger (LedgerState blk)
  ,  LedgerQueriesX blk
  ,  AF.HasHeader (Header blk))
  => NodeKernelData blk
  -> Trace IO (ForgeTracerType blk)
  -> IO (Trace IO (ForgeTracerType blk))
forgeTracerTransform nodeKern (Trace tr) = pure $ Trace $ T.arrow $ T.emit $
    \case
      (lc, Nothing, (Left (TraceLabelCreds creds
                        (TraceStartLeadershipCheck slotNo)))) -> do
        query <- mapNodeKernelDataIO
                    (\nk ->
                       (,,)
                         <$> nkQueryLedger (ledgerUtxoSizeX . ledgerState) nk
                         <*> nkQueryLedger (ledgerDelegMapSizeX . ledgerState) nk
                         <*> nkQueryChain fragmentChainDensity nk)
                    nodeKern
        fromSMaybe (pure ())
           (query <&>
             \(utxoSize, delegMapSize, chainDensity) ->
                let msg = TraceStartLeadershipCheckPlus
                            slotNo
                            utxoSize
                            delegMapSize
                            (fromRational chainDensity)
                in T.traceWith tr (lc, Nothing, Right (TraceLabelCreds creds msg)))
      (lc, Nothing, a) ->
          T.traceWith tr (lc, Nothing, a)
      (lc, Just control, a) ->
          T.traceWith tr (lc, Just control, a)

nkQueryLedger ::
     IsLedger (LedgerState blk)
  => (ExtLedgerState blk -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryLedger f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentLedger getChainDB)

fragmentChainDensity ::
  AF.HasHeader (Header blk)
  => AF.AnchoredFragment (Header blk) -> Rational
fragmentChainDensity frag = calcDensity blockD slotD
  where
    calcDensity :: Word64 -> Word64 -> Rational
    calcDensity bl sl
      | sl > 0 = toRational bl / toRational sl
      | otherwise = 0
    slotN  = unSlotNo $ fromWithOrigin 0 (AF.headSlot frag)
    -- Slot of the tip - slot @k@ blocks back. Use 0 as the slot for genesis
    -- includes EBBs
    slotD   = slotN
            - unSlotNo (fromWithOrigin 0 (AF.lastSlot frag))
    -- Block numbers start at 1. We ignore the genesis EBB, which has block number 0.
    blockD = blockN - firstBlock
    blockN = unBlockNo $ fromWithOrigin (BlockNo 1) (AF.headBlockNo frag)
    firstBlock = case unBlockNo . blockNo <$> AF.last frag of
      -- Empty fragment, no blocks. We have that @blocks = 1 - 1 = 0@
      Left _  -> 1
      -- The oldest block is the genesis EBB with block number 0,
      -- don't let it contribute to the number of blocks
      Right 0 -> 1
      Right b -> b

nkQueryChain ::
     (AF.AnchoredFragment (Header blk) -> a)
  -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
  -> IO a
nkQueryChain f NodeKernel{getChainDB} =
  f <$> atomically (ChainDB.getCurrentChain getChainDB)

class LedgerQueriesX blk where
  ledgerUtxoSizeX     :: LedgerState blk -> Int
  ledgerDelegMapSizeX :: LedgerState blk -> Int

instance LedgerQueriesX Byron.ByronBlock where
  ledgerUtxoSizeX = Map.size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState
  ledgerDelegMapSizeX _ = 0

instance LedgerQueriesX (Shelley.ShelleyBlock era) where
  ledgerUtxoSizeX =
      (\(Shelley.UTxO xs)-> Map.size xs)
    . Shelley._utxo
    . Shelley._utxoState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState
  ledgerDelegMapSizeX =
      Map.size
    . Shelley._delegations
    . Shelley._dstate
    . Shelley._delegationState
    . Shelley.esLState
    . Shelley.nesEs
    . Shelley.shelleyLedgerState

instance (LedgerQueriesX x, NoHardForks x)
      => LedgerQueriesX (HardForkBlock '[x]) where
  ledgerUtxoSizeX = ledgerUtxoSizeX . project
  ledgerDelegMapSizeX = ledgerDelegMapSizeX . project

instance LedgerQueriesX (Cardano.CardanoBlock c) where
  ledgerUtxoSizeX = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerUtxoSizeX ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerUtxoSizeX ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerUtxoSizeX ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerUtxoSizeX ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerUtxoSizeX ledgerAlonzo
  ledgerDelegMapSizeX = \case
    Cardano.LedgerStateByron   ledgerByron   -> ledgerDelegMapSizeX ledgerByron
    Cardano.LedgerStateShelley ledgerShelley -> ledgerDelegMapSizeX ledgerShelley
    Cardano.LedgerStateAllegra ledgerAllegra -> ledgerDelegMapSizeX ledgerAllegra
    Cardano.LedgerStateMary    ledgerMary    -> ledgerDelegMapSizeX ledgerMary
    Cardano.LedgerStateAlonzo  ledgerAlonzo  -> ledgerDelegMapSizeX ledgerAlonzo


mapNodeKernelDataIO ::
  (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (StrictMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f
