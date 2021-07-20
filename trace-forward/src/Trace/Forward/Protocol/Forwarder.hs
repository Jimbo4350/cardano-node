{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Trace.Forward.Protocol.Forwarder
  ( TraceForwarder (..)
  , traceForwarderPeer
  ) where

import           Network.TypedProtocol.Core (Peer (..), PeerHasAgency (..),
                                             PeerRole (..))

import           Trace.Forward.Protocol.Type

-- | Please note that the forwarder is a client from the __networking__
-- point of view: it establishes network connection with the acceptor.
-- But after the connection is established, the forwarder becomes a server
-- from the __interaction__ point of view: the acceptor sends a request for
-- new 'TraceObject's, the forwarder replies to the acceptor.
--
data TraceForwarder lo m a = TraceForwarder
  { -- | The acceptor sent us a request for node's basic info.
    recvMsgNodeInfoRequest
      :: m (NodeInfo, TraceForwarder lo m a)

    -- | The acceptor sent us a request for new 'TraceObject's.
  , recvMsgRequest
      :: forall blocking. TokBlockingStyle blocking
      -> Request
      -> m (BlockingReplyList blocking lo, TraceForwarder lo m a)

    -- | The acceptor terminated. Here we have a pure return value, but we
    -- could have done another action in 'm' if we wanted to.
  , recvMsgDone :: m a
  }

-- | Interpret a particular action sequence into the server side of the
-- 'TraceForward' protocol.
--
traceForwarderPeer
  :: Monad m
  => TraceForwarder lo m a
  -> Peer (TraceForward lo) 'AsServer 'StIdle m a
traceForwarderPeer TraceForwarder{..} =
  -- In the 'StIdle' state the forwarder is awaiting a request message
  -- from the acceptor.
  Await (ClientAgency TokIdle) $ \case
    -- The acceptor sent us a request for node's basic info, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgNodeInfoRequest -> Effect $ do
      (reply, next) <- recvMsgNodeInfoRequest
      return $ Yield (ServerAgency TokNodeInfoBusy)
                     (MsgNodeInfoReply reply)
                     (traceForwarderPeer next)

    -- The acceptor sent us a request for new 'TraceObject's, so now we're
    -- in the 'StBusy' state which means it's the forwarder's turn to send
    -- a reply.
    MsgRequest blocking request -> Effect $ do
      (reply, next) <- recvMsgRequest blocking request
      return $ Yield (ServerAgency (TokBusy blocking))
                     (MsgReply reply)
                     (traceForwarderPeer next)

    -- The acceptor sent the done transition, so we're in the 'StDone' state
    -- so all we can do is stop using 'done', with a return value.
    MsgDone -> Effect $ Done TokDone <$> recvMsgDone
