{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}

module Cardano.Tracing.Constraints
  ( TraceConstraints
  ) where

import           Prelude (Show)

import           Data.Aeson

import           Cardano.BM.Tracing (ToObject)
import           Cardano.Logging (LogFormatting)
import           Cardano.TraceDispatcher.Consensus.Formatting (GetKESInfoX (..),
                     HasKESInfoX (..))
import           Cardano.TraceDispatcher.Consensus.StartLeadershipCheck
                     (LedgerQueriesX)
import           Cardano.TraceDispatcher.Era.ConvertTxId (ConvertTxId')
import           Cardano.Tracing.ConvertTxId (ConvertTxId)
import           Cardano.Tracing.Metrics (HasKESInfo (..),
                     HasKESMetricsData (..))
import           Cardano.Tracing.Queries (LedgerQueries)

import           Cardano.Ledger.Alonzo (AlonzoEra)
import           Cardano.Ledger.Alonzo.PParams (PParamsUpdate)
import           Cardano.Ledger.Alonzo.Rules.Bbody (AlonzoBbodyPredFail)
import           Cardano.Ledger.Alonzo.Rules.Utxo (UtxoPredicateFailure)
import           Cardano.Ledger.Alonzo.Rules.Utxow (AlonzoPredFail)
import           Cardano.Ledger.Alonzo.TxBody (TxOut)
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge,
                     ForgeStateUpdateError, Header)
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Ouroboros.Consensus.Block (BlockProtocol, CannotForge, ForgeStateUpdateError,
                   Header)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.Inspect (LedgerEvent, LedgerUpdate,
                     LedgerWarning)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     HasTxId, HasTxs (..))
import           Ouroboros.Consensus.Protocol.Abstract (ValidationErr)
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (GenTx, TxId)

-- | Tracing-related constraints for monitoring purposes.
type TraceConstraints blk =
    ( ConvertTxId' blk
    , ConvertTxId blk
    , HasTxs blk
    , HasTxId (GenTx blk)
    , LedgerQueries blk
    , LedgerQueriesX blk
    , ToJSON   (TxId (GenTx blk))
    , ToJSON   (TxOut (AlonzoEra StandardCrypto))
    , ToJSON   (PParamsUpdate (AlonzoEra StandardCrypto))
    , HasKESMetricsData blk
    , HasKESInfo blk
    , HasKESInfoX blk
    , GetKESInfoX blk


    , ToObject (ApplyTxErr blk)
    , ToObject (GenTx blk)
    , ToObject (Header blk)
    , ToObject (LedgerError blk)
    , ToObject (LedgerEvent blk)
    , ToObject (OtherHeaderEnvelopeError blk)
    , ToObject (ValidationErr (BlockProtocol blk))
    , ToObject (CannotForge blk)
    , ToObject (ForgeStateUpdateError blk)

    -- TODO: handle the implications in the new logging
    , ToObject (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , ToObject (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , ToObject (AlonzoPredFail (AlonzoEra StandardCrypto))

    , LogFormatting (LedgerUpdate blk)
    , LogFormatting (LedgerWarning blk)
    , LogFormatting (ApplyTxErr blk)
    , LogFormatting (GenTx blk)
    , LogFormatting (Header blk)
    , LogFormatting (LedgerError blk)
    , LogFormatting (LedgerEvent blk)
    , LogFormatting (OtherHeaderEnvelopeError blk)
    , LogFormatting (ValidationErr (BlockProtocol blk))
    , LogFormatting (CannotForge blk)
    , LogFormatting (ForgeStateUpdateError blk)
    , LogFormatting (UtxoPredicateFailure (AlonzoEra StandardCrypto))
    , LogFormatting (AlonzoBbodyPredFail (AlonzoEra StandardCrypto))
    , LogFormatting (AlonzoPredFail (AlonzoEra StandardCrypto))

    , Show blk
    , Show (Header blk)
    )
