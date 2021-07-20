{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Trace.Forward.Protocol.Codec (
  codecTraceForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)
import           Text.Printf (printf)
import           Ouroboros.Network.Codec (Codec, PeerHasAgency (..),
                                          PeerRole (..), SomeMessage (..),
                                          mkCodecCborLazyBS)

import           Trace.Forward.Protocol.Type

codecTraceForward
  :: forall lo m.
     (MonadST m)
  => (Request -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s Request)
  -> ([(Text, Text)] -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s [(Text, Text)])
  -> ([lo] -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s [lo])
  -> Codec (TraceForward lo)
           DeserialiseFailure m LBS.ByteString
codecTraceForward encodeRequest   decodeRequest
                  encodeNIReply   decodeNIReply
                  encodeReplyList decodeReplyList =
  mkCodecCborLazyBS encode decode
 where
  -- Encode messages.
  encode :: forall (pr  :: PeerRole)
                   (st  :: TraceForward lo)
                   (st' :: TraceForward lo).
            PeerHasAgency pr st
         -> Message (TraceForward lo) st st'
         -> CBOR.Encoding

  encode (ClientAgency TokIdle) MsgNodeInfoRequest =
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 0

  encode (ClientAgency TokIdle) (MsgRequest blocking request) =
    CBOR.encodeListLen 3
      <> CBOR.encodeWord 1
      <> CBOR.encodeBool (case blocking of
                            TokBlocking    -> True
                            TokNonBlocking -> False)
      <> encodeRequest request

  encode (ClientAgency TokIdle) MsgDone =
    CBOR.encodeListLen 1
      <> CBOR.encodeWord 2

  encode (ServerAgency TokNodeInfoBusy) (MsgNodeInfoReply reply) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 3
      <> encodeNIReply reply

  encode (ServerAgency (TokBusy _)) (MsgReply reply) =
    CBOR.encodeListLen 2
      <> CBOR.encodeWord 4
      <> encodeReplyList replyList
   where
    replyList =
      case reply of
        BlockingReply    los -> NE.toList los
        NonBlockingReply los -> los

  -- Decode messages
  decode :: forall (pr :: PeerRole)
                   (st :: TraceForward lo) s.
            PeerHasAgency pr st
         -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (0, 1, ClientAgency TokIdle) -> do
        return $ SomeMessage MsgNodeInfoRequest

      (1, 3, ClientAgency TokIdle) -> do
        blocking <- CBOR.decodeBool
        request <- decodeRequest
        return $! if blocking then
                    SomeMessage (MsgRequest TokBlocking request)
                  else
                    SomeMessage (MsgRequest TokNonBlocking request)

      (2, 1, ClientAgency TokIdle) ->
        return $ SomeMessage MsgDone

      (3, 2, ServerAgency TokNodeInfoBusy) ->
        SomeMessage . MsgNodeInfoReply <$> decodeNIReply

      (4, 2, ServerAgency (TokBusy blocking)) -> do
        replyList <- decodeReplyList
        case (blocking, replyList) of
          (TokBlocking, x:xs) ->
            return $ SomeMessage (MsgReply (BlockingReply (x NE.:| xs)))

          (TokNonBlocking, los) ->
            return $ SomeMessage (MsgReply (NonBlockingReply los))

          (TokBlocking, []) ->
            fail "codecTraceForward: MsgReply: empty list not permitted"

      -- Failures per protocol state
      (_, _, ClientAgency TokIdle) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency TokNodeInfoBusy) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency (TokBusy TokBlocking)) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, ServerAgency (TokBusy TokNonBlocking)) ->
        fail (printf "codecTraceForward (%s) unexpected key (%d, %d)" (show stok) key len)
