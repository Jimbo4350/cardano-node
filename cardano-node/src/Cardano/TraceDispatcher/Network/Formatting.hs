{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.TraceDispatcher.Network.Formatting
  (
  ) where

import           Data.Aeson (Value (String), toJSON, (.=))
import qualified Data.IP as IP
import           Data.Text (pack)
import           Network.Mux (MuxTrace (..), WithMuxBearer (..))
import qualified Network.Socket as Socket
import           Text.Show

import           Cardano.TraceDispatcher.Era.ConvertTxId
import           Cardano.TraceDispatcher.Formatting ()
import           Cardano.TraceDispatcher.Render

import           Cardano.Logging
import           Cardano.Prelude hiding (Show, show)

import           Ouroboros.Consensus.Block (ConvertRawHash, GetHeader,
                     getHeader)
import           Ouroboros.Consensus.Ledger.Query (Query)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTx, HasTxId,
                     HasTxs, LedgerSupportsMempool, extractTxs,
                     txForgetValidated, txId)
import           Ouroboros.Consensus.Node.Run (SerialiseNodeToNodeConstraints,
                     estimateBlockSize)

import           Ouroboros.Network.Block (HasHeader, Point, Serialised,
                     blockHash)
import           Ouroboros.Network.Codec (AnyMessageAndAgency (..))
import           Ouroboros.Network.Codec (PeerHasAgency (..))
import qualified Ouroboros.Network.Diffusion as ND
import           Ouroboros.Network.Driver.Simple (TraceSendRecv (..))
import qualified Ouroboros.Network.NodeToClient as NtC
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Protocol.ChainSync.Type as ChainSync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LSQ
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Type as LTS
import           Ouroboros.Network.Protocol.Trans.Hello.Type
                     (ClientHasAgency (..), Message (..), ServerHasAgency (..))
import qualified Ouroboros.Network.Protocol.TxSubmission.Type as STX
import qualified Ouroboros.Network.Protocol.TxSubmission2.Type as TXS
import           Ouroboros.Network.Snocket (LocalAddress (..))
import           Ouroboros.Network.Subscription.Dns (DnsTrace (..),
                     WithDomainName (..))
import           Ouroboros.Network.Subscription.Ip (SubscriptionTrace,
                     WithIPList (..))


instance LogFormatting (AnyMessageAndAgency ps)
      => LogFormatting (TraceSendRecv ps) where
  forMachine dtal (TraceSendMsg m) = mkObject
    [ "kind" .= String "Send" , "msg" .= forMachine dtal m ]
  forMachine dtal (TraceRecvMsg m) = mkObject
    [ "kind" .= String "Recv" , "msg" .= forMachine dtal m ]

  forHuman (TraceSendMsg m) = "Send: " <> forHuman m
  forHuman (TraceRecvMsg m) = "Receive: " <> forHuman m

  asMetrics (TraceSendMsg m) = asMetrics m
  asMetrics (TraceRecvMsg m) = asMetrics m

instance LogFormatting (AnyMessageAndAgency (ChainSync blk pt tip)) where
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRequestNext{}) =
     mkObject [ "kind" .= String "MsgRequestNext"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgAwaitReply{}) =
     mkObject [ "kind" .= String "MsgAwaitReply"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollForward{}) =
     mkObject [ "kind" .= String "MsgRollForward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgRollBackward{}) =
     mkObject [ "kind" .= String "MsgRollBackward"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgFindIntersect{}) =
     mkObject [ "kind" .= String "MsgFindIntersect"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectFound{}) =
     mkObject [ "kind" .= String "MsgIntersectFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgIntersectNotFound{}) =
     mkObject [ "kind" .= String "MsgIntersectNotFound"
              , "agency" .= String (pack $ show stok)
              ]
   forMachine _dtal (AnyMessageAndAgency stok ChainSync.MsgDone{}) =
     mkObject [ "kind" .= String "MsgDone"
              , "agency" .= String (pack $ show stok)
              ]

instance LogFormatting (AnyMessageAndAgency (LTS.LocalTxSubmission tx err)) where
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgSubmitTx{}) =
    mkObject [ "kind" .= String "MsgSubmitTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgAcceptTx{}) =
    mkObject [ "kind" .= String "MsgAcceptTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgRejectTx{}) =
    mkObject [ "kind" .= String "MsgRejectTx"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LTS.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]


instance (forall result. Show (Query blk result))
      => LogFormatting (AnyMessageAndAgency (LSQ.LocalStateQuery blk pt (Query blk))) where
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquire{}) =
    mkObject [ "kind" .= String "MsgAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgAcquired{}) =
    mkObject [ "kind" .= String "MsgAcquired"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgFailure{}) =
    mkObject [ "kind" .= String "MsgFailure"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgQuery{}) =
    mkObject [ "kind" .= String "MsgQuery"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgResult{}) =
    mkObject [ "kind" .= String "MsgResult"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgRelease{}) =
    mkObject [ "kind" .= String "MsgRelease"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgReAcquire{}) =
    mkObject [ "kind" .= String "MsgReAcquire"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _dtal (AnyMessageAndAgency stok LSQ.MsgDone{}) =
    mkObject [ "kind" .= String "MsgDone"
             , "agency" .= String (pack $ show stok)
             ]

--
-- | instances of @forMachine@
--
-- NOTE: this list is sorted by the unqualified name of the outermost type.

instance ( ConvertTxId' blk
         , ConvertRawHash blk
         , HasHeader blk
         , GetHeader blk
         , HasTxId (GenTx blk)
         , SerialiseNodeToNodeConstraints blk
         , HasTxs blk
         , LedgerSupportsMempool blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch blk (Point blk))) where
  forMachine DBrief (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine dtal (AnyMessageAndAgency stok (MsgBlock blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
             , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
             , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             , "txIds" .= toJSON (presentTx <$> map txForgetValidated (extractTxs blk))
             ]
      where
        presentTx :: GenTx blk -> Value
        presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance ( ConvertTxId' blk
         , HasTxId (GenTx blk)
         , ConvertRawHash blk
         , HasTxs blk
         )
      => LogFormatting (AnyMessageAndAgency (BlockFetch (Serialised blk) (Point blk))) where
  forMachine DBrief (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
            -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
            -- , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
             ]

  forMachine _dtal (AnyMessageAndAgency stok (MsgBlock _blk)) =
    mkObject [ "kind" .= String "MsgBlock"
             , "agency" .= String (pack $ show stok)
          -- TODO
          -- , "blockHash" .= renderHeaderHash (Proxy @blk) (blockHash blk)
          --  , "blockSize" .= toJSON (estimateBlockSize (getHeader blk))
          --   , "txIds" .= toJSON (presentTx <$> extractTxs blk)
             ]
      -- where
      --   presentTx :: GenTx blk -> Value
      --   presentTx =  String . renderTxIdForDetails dtal . txId

  forMachine _v (AnyMessageAndAgency stok MsgRequestRange{}) =
    mkObject [ "kind" .= String "MsgRequestRange"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgStartBatch{}) =
    mkObject [ "kind" .= String "MsgStartBatch"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgNoBlocks{}) =
    mkObject [ "kind" .= String "MsgNoBlocks"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgBatchDone{}) =
    mkObject [ "kind" .= String "MsgBatchDone"
             , "agency" .= String (pack $ show stok)
             ]
  forMachine _v (AnyMessageAndAgency stok MsgClientDone{}) =
    mkObject [ "kind" .= String "MsgClientDone"
             , "agency" .= String (pack $ show stok)
             ]

instance (Show txid, Show tx)
      => LogFormatting (AnyMessageAndAgency (STX.TxSubmission txid tx)) where
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxs txids)) =
    mkObject
      [ "kind" .= String "MsgRequestTxs"
      , "agency" .= String (pack $ show stok)
      , "txIds" .= String (pack $ show txids)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxs txs)) =
    mkObject
      [ "kind" .= String "MsgReplyTxs"
      , "agency" .= String (pack $ show stok)
      , "txs" .= String (pack $ show txs)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgRequestTxIds _ _ _)) =
    mkObject
      [ "kind" .= String "MsgRequestTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok (STX.MsgReplyTxIds _)) =
    mkObject
      [ "kind" .= String "MsgReplyTxIds"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine _dtal (AnyMessageAndAgency stok STX.MsgDone) =
    mkObject
      [ "kind" .= String "MsgDone"
      , "agency" .= String (pack $ show stok)
      ]

instance (Show txid, Show tx)
      => LogFormatting (AnyMessageAndAgency (TXS.TxSubmission2 txid tx)) where
  forMachine _dtal (AnyMessageAndAgency
                   -- we need this pattern match for GHC to recognise this
                   -- function as total.
                   stok@(ClientAgency TokHello)
                   MsgHello) =
    mkObject
      [ "kind" .= String "MsgHello"
      , "agency" .= String (pack $ show stok)
      ]
  forMachine dtal (AnyMessageAndAgency
                  (ClientAgency (TokClientTalk stok))
                  (MsgTalk msg)) =
    forMachine dtal (AnyMessageAndAgency (ClientAgency stok) msg)
  forMachine dtal (AnyMessageAndAgency
                  (ServerAgency (TokServerTalk stok))
                  (MsgTalk msg)) =
    forMachine dtal (AnyMessageAndAgency (ServerAgency stok) msg)

instance LogFormatting (WithIPList (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithIPList localAddresses dests ev) =
    mkObject [ "kind" .= String "IP SubscriptionTrace"
             , "localAddresses" .= String (pack $ show localAddresses)
             , "dests" .= String (pack $ show dests)
             , "event" .= String (pack $ show ev)]
  forHuman (WithIPList localAddresses dests ev) =
                     pack (show ev)
                  <> ". Local addresses are "
                  <> (pack $ show localAddresses)
                  <> ". Destinations are "
                  <> (pack $ show dests)
                  <> "."

instance LogFormatting (WithDomainName (SubscriptionTrace Socket.SockAddr)) where
  forMachine _dtal (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DNS SubscriptionTrace"
             , "domain" .= String (pack $ show dom)
             , "event" .= String (pack $ show ev)]
  forHuman (WithDomainName dom ev) =
                     pack (show ev)
                  <> ". Domain is "
                  <> pack (show dom)
                  <> "."

instance LogFormatting (WithDomainName DnsTrace) where
  forMachine _dtal (WithDomainName dom ev) =
    mkObject [ "kind" .= String "DnsTrace"
             , "domain" .= String (pack $ show dom)
             , "event" .= String (pack $ show ev)]
  forHuman (WithDomainName dom ev) =
                     pack (show ev)
                  <> ". Domain is "
                  <> pack (show dom)
                  <> "."

instance LogFormatting NtN.RemoteAddress where
    forMachine _dtal (Socket.SockAddrInet port addr) =
        let ip = IP.fromHostAddress addr in
        mkObject [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (Socket.SockAddrInet6 port _ addr _) =
        let ip = IP.fromHostAddress6 addr in
        mkObject [ "addr" .= show ip
                 , "port" .= show port
                 ]
    forMachine _dtal (Socket.SockAddrUnix path) =
        mkObject [ "path" .= show path ]


instance LogFormatting NtN.RemoteConnectionId where
    forMachine dtal (NtN.ConnectionId l r) =
        mkObject [ "local" .= forMachine dtal l
                 , "remote" .= forMachine dtal r
                 ]

instance LogFormatting LocalAddress where
    forMachine _dtal (LocalAddress path) =
        mkObject ["path" .= path]

instance LogFormatting NtC.LocalConnectionId where
    forMachine dtal (NtC.ConnectionId l r) =
        mkObject [ "local" .= forMachine dtal l
                 , "remote" .= forMachine dtal r
                 ]

instance Show addr => LogFormatting (NtN.WithAddr addr NtN.ErrorPolicyTrace) where
    forMachine _dtal (NtN.WithAddr addr ev) =
      mkObject [ "kind" .= String "ErrorPolicyTrace"
               , "address" .= show addr
               , "event" .= show ev ]
    forHuman (NtN.WithAddr addr ev) = "With address " <> showT addr <> ". " <> showT ev

instance LogFormatting NtN.AcceptConnectionsPolicyTrace where
    forMachine _dtal (NtN.ServerTraceAcceptConnectionRateLimiting delay numOfConnections) =
      mkObject [ "kind" .= String "ServerTraceAcceptConnectionRateLimiting"
               , "delay" .= show delay
               , "numberOfConnection" .= show numOfConnections
               ]
    forMachine _dtal (NtN.ServerTraceAcceptConnectionHardLimit softLimit) =
      mkObject [ "kind" .= String "ServerTraceAcceptConnectionHardLimit"
               , "softLimit" .= show softLimit
               ]
    forHuman m = showT m

instance (LogFormatting peer, Show peer) =>
    LogFormatting (WithMuxBearer peer MuxTrace) where
  forMachine dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "MuxTrace"
             , "bearer" .= forMachine dtal b
             , "event" .= showT ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

instance LogFormatting NtC.HandshakeTr where
  forMachine _dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "LocalHandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev

instance LogFormatting NtN.HandshakeTr where
  forMachine _dtal (WithMuxBearer b ev) =
    mkObject [ "kind" .= String "HandshakeTrace"
             , "bearer" .= show b
             , "event" .= show ev ]
  forHuman (WithMuxBearer b ev) = "With mux bearer " <> showT b
                                      <> ". " <> showT ev


instance LogFormatting ND.DiffusionInitializationTracer where
  forMachine _dtal (ND.RunServer sockAddr) = mkObject
    [ "kind" .= String "RunServer"
    , "socketAddress" .= String (pack (show sockAddr))
    ]

  forMachine _dtal (ND.RunLocalServer localAddress) = mkObject
    [ "kind" .= String "RunLocalServer"
    , "localAddress" .= String (pack (show localAddress))
    ]
  forMachine _dtal (ND.UsingSystemdSocket path) = mkObject
    [ "kind" .= String "UsingSystemdSocket"
    , "path" .= String (pack path)
    ]

  forMachine _dtal (ND.CreateSystemdSocketForSnocketPath path) = mkObject
    [ "kind" .= String "CreateSystemdSocketForSnocketPath"
    , "path" .= String (pack path)
    ]
  forMachine _dtal (ND.CreatedLocalSocket path) = mkObject
    [ "kind" .= String "CreatedLocalSocket"
    , "path" .= String (pack path)
    ]
  forMachine _dtal (ND.ConfiguringLocalSocket path socket) = mkObject
    [ "kind" .= String "ConfiguringLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningLocalSocket path socket) = mkObject
    [ "kind" .= String "ListeningLocalSocket"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.LocalSocketUp path fd) = mkObject
    [ "kind" .= String "LocalSocketUp"
    , "path" .= String (pack path)
    , "socket" .= String (pack (show fd))
    ]
  forMachine _dtal (ND.CreatingServerSocket socket) = mkObject
    [ "kind" .= String "CreatingServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ListeningServerSocket socket) = mkObject
    [ "kind" .= String "ListeningServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ServerSocketUp socket) = mkObject
    [ "kind" .= String "ServerSocketUp"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.ConfiguringServerSocket socket) = mkObject
    [ "kind" .= String "ConfiguringServerSocket"
    , "socket" .= String (pack (show socket))
    ]
  forMachine _dtal (ND.UnsupportedLocalSystemdSocket path) = mkObject
    [ "kind" .= String "UnsupportedLocalSystemdSocket"
    , "path" .= String (pack (show path))
    ]
  forMachine _dtal ND.UnsupportedReadySocketCase = mkObject
    [ "kind" .= String "UnsupportedReadySocketCase"
    ]
  forMachine _dtal (ND.DiffusionErrored exception) = mkObject
    [ "kind" .= String "DiffusionErrored"
    , "path" .= String (pack (show exception))
    ]
