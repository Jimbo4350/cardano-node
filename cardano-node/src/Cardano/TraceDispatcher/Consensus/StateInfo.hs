{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# OPTIONS_GHC -Wno-deprecations  #-}

module Cardano.TraceDispatcher.Consensus.StateInfo
  (
    severityStateInfo
  , namesForStateInfo
  , traceAsKESInfo
  ) where

import           Data.SOP.Strict

import           Cardano.Logging
import           Cardano.Prelude hiding (All, Show, show)
import           Cardano.TraceDispatcher.Consensus.Formatting

import           Ouroboros.Consensus.Block.Forging
import           Ouroboros.Consensus.Node.Tracers (TraceLabelCreds (..))
import qualified Ouroboros.Consensus.Shelley.Protocol.HotKey as HotKey
import           Shelley.Spec.Ledger.OCert (KESPeriod (..))



traceAsKESInfo
  :: forall m blk . (GetKESInfoX blk, MonadIO m)
  => Proxy blk
  -> Trace m (TraceLabelCreds (HotKey.KESInfo))
  -> Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsKESInfo pr tr = traceAsMaybeKESInfo pr (filterTraceMaybe tr)

traceAsMaybeKESInfo
  :: forall m blk . (GetKESInfoX blk, MonadIO m)
  => Proxy blk
  -> Trace m (Maybe (TraceLabelCreds (HotKey.KESInfo)))
  -> Trace m (TraceLabelCreds (ForgeStateInfo blk))
traceAsMaybeKESInfo pr (Trace tr) = Trace $
  contramap
        (\(lc, mbC, (TraceLabelCreds c e)) ->
            case getKESInfoFromStateInfoX pr e of
              Just kesi -> (lc, mbC, Just (TraceLabelCreds c kesi))
              Nothing   -> (lc, mbC, Nothing))
        tr

severityStateInfo :: TraceLabelCreds HotKey.KESInfo -> SeverityS
severityStateInfo (TraceLabelCreds _creds a) = severityStateInfo'  a

severityStateInfo' :: HotKey.KESInfo -> SeverityS
severityStateInfo' forgeStateInfo =
    let maxKesEvos = endKesPeriod - startKesPeriod
        oCertExpiryKesPeriod = startKesPeriod + maxKesEvos
        kesPeriodsUntilExpiry = max 0 (oCertExpiryKesPeriod - currKesPeriod)
    in if kesPeriodsUntilExpiry > 7
      then Info
      else if kesPeriodsUntilExpiry <= 1
        then Alert
        else Warning
    where
    HotKey.KESInfo
      { HotKey.kesStartPeriod = KESPeriod startKesPeriod
      , HotKey.kesEvolution = currKesPeriod
      , HotKey.kesEndPeriod = KESPeriod endKesPeriod
      } = forgeStateInfo

namesForStateInfo :: TraceLabelCreds HotKey.KESInfo -> [Text]
namesForStateInfo (TraceLabelCreds _creds a) = namesForStateInfo' a

namesForStateInfo' :: HotKey.KESInfo -> [Text]
namesForStateInfo' _fsi = []
