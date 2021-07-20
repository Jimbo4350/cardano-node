{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -Wno-all-missed-specialisations #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Benchmarking.GeneratorTx.SubmissionClient
  ( SubmissionThreadStats(..)
  , TxSource(..)
  , txSubmissionClient
  ) where

import           Prelude (error, fail)
import           Cardano.Prelude hiding (ByteString, atomically, retry, state, threadDelay)

import           Control.Arrow ((&&&))

import qualified Data.List as L
import qualified Data.List.Extra as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

import           Control.Tracer (Tracer, traceWith)

import           Cardano.Tracing.OrphanInstances.Byron ()
import           Cardano.Tracing.OrphanInstances.Common ()
import           Cardano.Tracing.OrphanInstances.Consensus ()
import           Cardano.Tracing.OrphanInstances.Network ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Ouroboros.Consensus.Cardano as Consensus (CardanoBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, GenTxId, txInBlockSize)
import qualified Ouroboros.Consensus.Ledger.SupportsMempool as Mempool
import           Ouroboros.Consensus.Shelley.Ledger.Mempool (mkShelleyTx)
import qualified Ouroboros.Consensus.Shelley.Ledger.Mempool as Mempool (TxId(ShelleyTxId))
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Ouroboros.Consensus.Cardano.Block (GenTx (GenTxAllegra, GenTxAlonzo, GenTxShelley, GenTxMary))
import qualified Ouroboros.Consensus.Cardano.Block as Block (TxId(GenTxIdShelley, GenTxIdAllegra, GenTxIdAlonzo, GenTxIdMary))

import           Ouroboros.Network.Protocol.TxSubmission.Client (ClientStIdle (..),
                                                                 ClientStTxIds (..),
                                                                 ClientStTxs (..),
                                                                 TxSubmissionClient (..))
import           Ouroboros.Network.Protocol.TxSubmission.Type (BlockingReplyList (..),
                                                               TokBlockingStyle (..), TxSizeInBytes)

import           Cardano.Api
import           Cardano.Api.Shelley (Tx(ShelleyTx), fromShelleyTxId)

import           Cardano.Benchmarking.Tracer
import           Cardano.Benchmarking.Types
type CardanoBlock    = Consensus.CardanoBlock  StandardCrypto

data SubmissionThreadStats
  = SubmissionThreadStats
      { stsAcked       :: {-# UNPACK #-} !Ack
      , stsSent        :: {-# UNPACK #-} !Sent
      , stsUnavailable :: {-# UNPACK #-} !Unav
      }

data TxSource era
  = Exhausted
  | Active (ProduceNextTxs era)

type ProduceNextTxs era = (forall m blocking . MonadIO m => TokBlockingStyle blocking -> Req -> m (TxSource era, [Tx era]))

produceNextTxs :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> LocalState era -> m (LocalState era, [Tx era])
produceNextTxs blocking req (txProducer, unack, stats) = do
  (newTxProducer, txList) <- produceNextTxs' blocking req txProducer
  return ((newTxProducer, unack, stats), txList)

produceNextTxs' :: forall m blocking era . MonadIO m => TokBlockingStyle blocking -> Req -> TxSource era -> m (TxSource era, [Tx era])
produceNextTxs' _ _ Exhausted = return (Exhausted, [])
produceNextTxs' blocking req (Active callback) = callback blocking req

type LocalState era = (TxSource era, UnAcked (Tx era), SubmissionThreadStats)
type EndOfProtocolCallback m = SubmissionThreadStats -> m ()

txSubmissionClient
  :: forall m era tx.
     ( MonadIO m, MonadFail m
     , IsShelleyBasedEra era
     , tx      ~ Tx era
     )
  => Tracer m NodeToNodeSubmissionTrace
  -> Tracer m (TraceBenchTxSubmit TxId)
  -> TxSource era
  -> EndOfProtocolCallback m
  -> TxSubmissionClient (GenTxId CardanoBlock) (GenTx CardanoBlock) m ()
txSubmissionClient tr bmtr initialTxSource endOfProtocolCallback =
  TxSubmissionClient $
    pure $ client (initialTxSource, UnAcked [], SubmissionThreadStats 0 0 0)
 where
  discardAcknowledged :: TokBlockingStyle a -> Ack -> LocalState era -> m (LocalState era)
  discardAcknowledged blocking (Ack ack) (txSource, UnAcked unAcked, stats) = do
    when (tokIsBlocking blocking && ack /= length unAcked) $ do
      let err = "decideAnnouncement: TokBlocking, but length unAcked != ack"
      traceWith bmtr (TraceBenchTxSubError err)
      fail (T.unpack err)
    let (stillUnacked, acked) = L.splitAtEnd ack unAcked
    let newStats = stats { stsAcked = stsAcked stats + Ack ack }
    traceWith bmtr $ TraceBenchTxSubServAck  (getTxId . getTxBody <$> acked)
    return (txSource, UnAcked stillUnacked, newStats)

  queueNewTxs :: [tx] -> LocalState era -> LocalState era
  queueNewTxs newTxs (txSource, UnAcked unAcked, stats)
    = (txSource, UnAcked (newTxs <> unAcked), stats)

  -- Sadly, we can't just return what we want, we instead have to
  -- communicate via IORefs, because..
  -- The () return type is forced by Ouroboros.Network.NodeToNode.connectTo
  client ::LocalState era -> ClientStIdle (GenTxId CardanoBlock) (GenTx CardanoBlock) m ()

  client localState = ClientStIdle
    { recvMsgRequestTxIds = requestTxIds localState
    , recvMsgRequestTxs = requestTxs localState
    }

  requestTxIds :: forall blocking.
       LocalState era
    -> TokBlockingStyle blocking
    -> Word16
    -> Word16
    -> m (ClientStTxIds blocking (GenTxId CardanoBlock) (GenTx CardanoBlock) m ())
  requestTxIds state blocking ackNum reqNum = do
    let ack = Ack $ fromIntegral ackNum
        req = Req $ fromIntegral reqNum
    traceWith tr $ reqIdsTrace ack req blocking
    stateA <- discardAcknowledged blocking ack state

    (stateB, newTxs) <- produceNextTxs blocking req stateA
    let stateC@(_, UnAcked outs , stats) = queueNewTxs newTxs stateB

    traceWith tr $ idListTrace (ToAnnce newTxs) blocking
    traceWith bmtr $ TraceBenchTxSubServAnn  (getTxId . getTxBody <$> newTxs)
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> outs)

    case blocking of
      TokBlocking -> case NE.nonEmpty newTxs of
        Nothing -> do
          traceWith tr EndOfProtocol
          endOfProtocolCallback stats
          pure $ SendMsgDone ()
        (Just txs) -> pure $ SendMsgReplyTxIds
                              (BlockingReply $ txToIdSize <$> txs)
                              (client stateC)
      TokNonBlocking ->  pure $ SendMsgReplyTxIds
                             (NonBlockingReply $ txToIdSize <$> newTxs)
                             (client stateC)
                    
  requestTxs ::
       LocalState era
    -> [GenTxId CardanoBlock]
    -> m (ClientStTxs (GenTxId CardanoBlock) (GenTx CardanoBlock) m ())
  requestTxs (txSource, unAcked, stats) txIds = do
    let  reqTxIds :: [TxId]
         reqTxIds = fmap fromGenTxId txIds
    traceWith tr $ ReqTxs (length reqTxIds)
    let UnAcked ua = unAcked
        uaIds = getTxId . getTxBody <$> ua
        (toSend, _retained) = L.partition ((`L.elem` reqTxIds) . getTxId . getTxBody) ua
        missIds = reqTxIds L.\\ uaIds

    traceWith tr $ TxList (length toSend)
    traceWith bmtr $ TraceBenchTxSubServReq reqTxIds
    traceWith bmtr $ TraceBenchTxSubServOuts (getTxId . getTxBody <$> ua)
    unless (L.null missIds) $
      traceWith bmtr $ TraceBenchTxSubServUnav missIds
    pure $ SendMsgReplyTxs (toGenTx <$> toSend)
      (client (txSource, unAcked,
        stats { stsSent =
                stsSent stats + Sent (length toSend)
              , stsUnavailable =
                stsUnavailable stats + Unav (length missIds)}))

  txToIdSize :: tx -> (GenTxId CardanoBlock, TxSizeInBytes)
  txToIdSize = (Mempool.txId &&& txInBlockSize) . toGenTx

  toGenTx :: tx -> GenTx CardanoBlock
  toGenTx tx = case (shelleyBasedEra @ era , tx) of
    (ShelleyBasedEraShelley, ShelleyTx _ tx') -> GenTxShelley (mkShelleyTx tx')
    (ShelleyBasedEraAllegra, ShelleyTx _ tx') -> GenTxAllegra (mkShelleyTx tx')
    (ShelleyBasedEraMary, ShelleyTx _ tx') -> GenTxMary (mkShelleyTx tx')
    (ShelleyBasedEraAlonzo, ShelleyTx _ tx') -> GenTxAlonzo (mkShelleyTx tx')

  fromGenTxId :: GenTxId CardanoBlock -> TxId
  fromGenTxId (Block.GenTxIdShelley (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId (Block.GenTxIdAllegra (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId (Block.GenTxIdMary    (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId (Block.GenTxIdAlonzo  (Mempool.ShelleyTxId i)) = fromShelleyTxId i
  fromGenTxId _ = error "submission.hs: fromGenTxId"

  tokIsBlocking :: TokBlockingStyle a -> Bool
  tokIsBlocking = \case
    TokBlocking    -> True
    TokNonBlocking -> False

  reqIdsTrace :: Ack -> Req -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  reqIdsTrace ack req = \case
     TokBlocking    -> ReqIdsBlocking ack req
     TokNonBlocking -> ReqIdsPrompt   ack req

  idListTrace :: ToAnnce tx -> TokBlockingStyle a -> NodeToNodeSubmissionTrace
  idListTrace (ToAnnce toAnn) = \case
     TokBlocking    -> IdsListBlocking $ length toAnn
     TokNonBlocking -> IdsListPrompt   $ length toAnn

