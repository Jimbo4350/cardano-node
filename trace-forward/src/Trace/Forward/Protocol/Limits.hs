{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Trace.Forward.Protocol.Limits
  ( byteLimitsTraceForward
  , timeLimitsTraceForward
  ) where

import           Data.Time.Clock (DiffTime)

import           Network.TypedProtocol.Core (PeerHasAgency (..), PeerRole (..))
import           Ouroboros.Network.Driver.Limits (ProtocolSizeLimits (..),
                                                  ProtocolTimeLimits (..))
import           Ouroboros.Network.Protocol.Limits (largeByteLimit, shortWait,
                                                    smallByteLimit, waitForever)

import           Trace.Forward.Protocol.Type

-- | Byte Limits.
byteLimitsTraceForward
  :: forall bytes lo.
     (bytes -> Word)
  -> ProtocolSizeLimits (TraceForward lo) bytes
byteLimitsTraceForward = ProtocolSizeLimits stateToLimit
 where
  stateToLimit
    :: forall lo
              (pr :: PeerRole)
              (st :: TraceForward lo).
       PeerHasAgency pr st
    -> Word
  stateToLimit (ServerAgency TokNodeInfoBusy)          = largeByteLimit
  stateToLimit (ServerAgency (TokBusy TokBlocking))    = largeByteLimit
  stateToLimit (ServerAgency (TokBusy TokNonBlocking)) = largeByteLimit
  stateToLimit (ClientAgency TokIdle)                  = smallByteLimit

-- | Time Limits.
timeLimitsTraceForward
  :: forall lo. ProtocolTimeLimits (TraceForward lo)
timeLimitsTraceForward = ProtocolTimeLimits stateToLimit
 where
  stateToLimit
    :: forall lo
              (pr :: PeerRole)
              (st :: TraceForward lo).
       PeerHasAgency pr st
    -> Maybe DiffTime
  stateToLimit (ServerAgency TokNodeInfoBusy)          = shortWait
  stateToLimit (ServerAgency (TokBusy TokBlocking))    = waitForever
  stateToLimit (ServerAgency (TokBusy TokNonBlocking)) = shortWait
  stateToLimit (ClientAgency TokIdle)                  = waitForever
