{-# LANGUAGE PackageImports #-}

module Trace.Forward.Configuration
  ( AcceptorConfiguration (..)
  , ForwarderConfiguration (..)
  , HowToConnect (..)
  , Host
  , Port
  ) where

-- Temporary solution, to avoid conflicts with trace-dispatcher.
import "contra-tracer" Control.Tracer (Tracer)
import           Data.IORef (IORef)
import           Data.Text (Text)
import           Data.Word (Word16)
import           Ouroboros.Network.Driver (TraceSendRecv)

import           Trace.Forward.Protocol.Type (NodeInfo, Request, TraceForward)

type Host = Text
type Port = Word16

-- | Specifies how to connect to the peer.
data HowToConnect
  = LocalPipe    !FilePath    -- ^ Local pipe (UNIX or Windows).
  | RemoteSocket !Host !Port  -- ^ Remote socket (host and port).

-- | Acceptor configuration.
data AcceptorConfiguration lo = AcceptorConfiguration
  { -- | The tracer that will be used by the acceptor in its network layer.
    acceptorTracer    :: !(Tracer IO (TraceSendRecv (TraceForward lo)))
    -- | The endpoint that will be used to listen to the forwarder.
  , forwarderEndpoint :: !HowToConnect
    -- | The request specifies how many 'TraceObject's will be requested.
  , whatToRequest     :: !Request
    -- | Additional action that will be performed every time the acceptor will
    -- receive the reply from the forwarder.
  , actionOnReply     :: !([lo] -> IO ())
    -- | 'IORef' that can be used as a brake: if an external thread will set it to
    -- 'True', the acceptor will send 'MsgDone' message to the forwarder and their
    -- session will be closed.
  , shouldWeStop      :: !(IORef Bool)
    -- | An action that will be performed before sending 'MsgDone' message.
  , actionOnDone      :: !(IO ())
  }

-- | Forwarder configuration.
data ForwarderConfiguration lo = ForwarderConfiguration
  { -- | The tracer that will be used by the forwarder in its network layer.
    forwarderTracer    :: !(Tracer IO (TraceSendRecv (TraceForward lo)))
    -- | The endpoint that will be used to connect to the acceptor.
  , acceptorEndpoint   :: !HowToConnect
    -- | An action that returns node's basic information.
  , nodeBasicInfo      :: !(IO NodeInfo)
    -- | Additional action that will be performed every time the forwarder will
    -- receive the request from the acceptor.
  , actionOnRequest    :: !(Request -> IO ())
  }
