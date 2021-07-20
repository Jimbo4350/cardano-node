{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the Trace forwarding/accepting protocol.
--
-- Since we are using a typed protocol framework this is in some sense /the/
-- definition of the protocol: what is allowed and what is not allowed.

module Trace.Forward.Protocol.Type
  ( TraceForward (..)
  , TokBlockingStyle (..)
  , Message (..)
  , ClientHasAgency (..)
  , ServerHasAgency (..)
  , NobodyHasAgency (..)
  , Request (..)
  , BlockingReplyList (..)
  , NodeInfo
  , NodeInfoStore
  ) where

import           Codec.Serialise (Serialise (..))
import           Data.IORef (IORef)
import           Data.List.NonEmpty (NonEmpty)
import           Data.Proxy (Proxy(..))
import           Data.Text (Text)
import           Data.Word (Word16)
import           GHC.Generics (Generic)
import           Network.TypedProtocol.Core (Protocol (..))
import           Ouroboros.Network.Util.ShowProxy (ShowProxy(..))

-- | A kind to identify our protocol, and the types of the states in the state
-- transition diagram of the protocol.
--
-- IMPORTANT NOTE: the following terminology is used:
--
-- 1. From the protocol's point of view, two peers talk to each other:
--    the forwarder and the acceptor.
-- 2. The forwarder is an application that collects 'TraceObject's and sends
--    them to the acceptor by request.
-- 3. The acceptor is an application that receives 'TraceObject's from the
--    forwarder.
-- 4. You can this of the forwarder as a client, and the acceptor as a server:
--    4.1. The client is "initially active side", because it establishes the
--    connection with the server.
--    4.2. The server is "initially passive side", because it accepts the
--    connection from the client.
-- 5. But after the connection is established, the roles are REVERSED:
--    the acceptor becomes an active side because it asks 'TraceObject's, the
--    forwarder becomes a passive side because it waits for the request from
--    the acceptor, collects the 'TraceObject's and sends them to the acceptor.

-- | The request for N 'TraceObject's.
-- The acceptor will send this request to the forwarder.
newtype Request = GetTraceObjects Word16
  deriving (Eq, Generic, Show)

instance ShowProxy Request
instance Serialise Request

data TraceForward lo where

  -- | Both acceptor and forwarder are in idle state. The acceptor can send a
  -- request for node's basic info or for a list of 'TraceObject's, the forwarder
  -- is waiting for some request.
  --
  -- Node's basic info is a fundamental information about the node, such as
  -- its protocol, version, start time, etc. It is assuming that the node
  -- is able to provide such information immediately.
  StIdle :: TraceForward lo

  -- | The acceptor has sent a request for node's basic info. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with node's basic info.
  StNodeInfoBusy :: TraceForward lo

  -- | The acceptor has sent a next request for 'TraceObject's. The acceptor is
  -- now waiting for a reply, and the forwarder is busy getting ready to send a
  -- reply with new list of 'TraceObject's.
  --
  -- There are two sub-states for this, for blocking and non-blocking cases.
  StBusy :: StBlockingStyle -> TraceForward lo

  -- | Both the acceptor and forwarder are in the terminal state. They're done.
  StDone :: TraceForward lo

instance (ShowProxy lo)
      => ShowProxy (TraceForward lo) where
  showProxy _ = concat
    [ "TraceForward ("
    , showProxy (Proxy :: Proxy lo)
    , ")"
    ]

data StBlockingStyle where
  -- | In this sub-state the reply need not be prompt. There is no timeout.
  StBlocking    :: StBlockingStyle
  -- | In this sub-state the peer must reply. There is a timeout.
  StNonBlocking :: StBlockingStyle

-- | The value level equivalent of 'StBlockingStyle'.
--
-- This is also used in 'MsgRequest' where it is interpreted (and can be encoded)
-- as a 'Bool' with 'True' for blocking, and 'False' for non-blocking.
data TokBlockingStyle (k :: StBlockingStyle) where
  TokBlocking    :: TokBlockingStyle 'StBlocking
  TokNonBlocking :: TokBlockingStyle 'StNonBlocking

deriving instance Eq   (TokBlockingStyle b)
deriving instance Show (TokBlockingStyle b)

-- | We have requests for lists of things. In the blocking case the
-- corresponding reply must be non-empty, whereas in the non-blocking case
-- an empty reply is fine.
--
data BlockingReplyList (blocking :: StBlockingStyle) lo where
  BlockingReply    :: NonEmpty lo  -> BlockingReplyList 'StBlocking    lo
  NonBlockingReply ::         [lo] -> BlockingReplyList 'StNonBlocking lo

deriving instance Eq   lo => Eq   (BlockingReplyList blocking lo)
deriving instance Show lo => Show (BlockingReplyList blocking lo)

-- | The node (on the forwarder side) provides its basic information, such as
--   protocol, version, start time, unique name, etc.
type NodeInfo = [(Text, Text)]

-- | The store for 'NodeInfo', will be used on the acceptor's side to store received node's info.
type NodeInfoStore = IORef NodeInfo

instance Protocol (TraceForward lo) where

  -- | The messages in the Trace forwarding/accepting protocol.
  --
  data Message (TraceForward lo) from to where

    -- | Request the node's basic info from the forwarder.
    --   State: Idle -> NodeInfoBusy.
    MsgNodeInfoRequest
      :: Message (TraceForward lo) 'StIdle 'StNodeInfoBusy

    -- | Reply with the node's basic info, as a list of pairs.
    --   State: NodeInfoBusy -> Idle.
    MsgNodeInfoReply
      :: NodeInfo
      -> Message (TraceForward lo) 'StNodeInfoBusy 'StIdle

    -- | Request the list of 'TraceObject's from the forwarder.
    --   State: Idle -> Busy.
    --
    -- With 'TokBlocking' this is a a blocking operation: the reply will
    -- always have at least one 'TraceObject', and it does not expect a prompt
    -- reply: there is no timeout. This covers the case when there
    -- is nothing else to do but wait.
    --
    -- With 'TokNonBlocking' this is a non-blocking operation: the reply
    -- may be an empty list and this does expect a prompt reply.
    MsgRequest
      :: TokBlockingStyle blocking
      -> Request
      -> Message (TraceForward lo) 'StIdle ('StBusy blocking)

    -- | Reply with a list of 'TraceObject's for the acceptor.
    -- State: Busy -> Idle.
    MsgReply
      :: BlockingReplyList blocking lo
      -> Message (TraceForward lo) ('StBusy blocking) 'StIdle

    -- | Terminating message. State: Idle -> Done.
    MsgDone
      :: Message (TraceForward lo) 'StIdle 'StDone

  -- | This is an explanation of our states, in terms of which party has agency
  -- in each state.
  --
  -- 1. When both peers are in Idle state, the acceptor can send a message
  --    to the forwarder (request for new 'TraceObject's),
  -- 2. When both peers are in Busy state, the forwarder is expected to send
  --    a reply to the acceptor (list of new 'TraceObject's).
  --
  -- So we assume that, from __interaction__ point of view:
  -- 1. ClientHasAgency (from 'Network.TypedProtocol.Core') corresponds to acceptor's agency.
  -- 3. ServerHasAgency (from 'Network.TypedProtocol.Core') corresponds to forwarder's agency.
  --
  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokNodeInfoBusy :: ServerHasAgency 'StNodeInfoBusy
    TokBusy         :: TokBlockingStyle blocking -> ServerHasAgency ('StBusy blocking)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  -- | Impossible cases.
  exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

instance (Show lo)
      => Show (Message (TraceForward lo) from to) where
  show MsgNodeInfoRequest{} = "MsgNodeInfoRequest"
  show MsgNodeInfoReply{}   = "MsgNodeInfoReply"
  show MsgRequest{}         = "MsgRequest"
  show MsgReply{}           = "MsgReply"
  show MsgDone{}            = "MsgDone"

instance Show (ClientHasAgency (st :: TraceForward lo)) where
  show TokIdle = "TokIdle"

instance Show (ServerHasAgency (st :: TraceForward lo)) where
  show TokNodeInfoBusy = "TokNodeInfoBusy"
  show TokBusy{}       = "TokBusy"
