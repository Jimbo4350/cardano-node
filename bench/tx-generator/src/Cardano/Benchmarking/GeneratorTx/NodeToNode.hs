{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans -Wno-unticked-promoted-constructors -Wno-all-missed-specialisations #-}

module Cardano.Benchmarking.GeneratorTx.NodeToNode
  ( ConnectClient
  , benchmarkConnectTxSubmit
  ) where

import           Cardano.Prelude (forever, liftIO)
import           Prelude

import           Codec.Serialise (DeserialiseFailure)
import           Control.Monad.Class.MonadTimer (MonadTimer, threadDelay)
import           Control.Monad.Class.MonadSTM.Strict (newTVarIO)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import           Data.Proxy (Proxy (..))
import           Network.Socket (AddrInfo (..))
import           System.Random (newStdGen)

import           Control.Tracer (Tracer, nullTracer)
import           Ouroboros.Consensus.Byron.Ledger.Mempool (GenTx)
import           Ouroboros.Consensus.Block.Abstract
import qualified Ouroboros.Consensus.Cardano as Consensus (CardanoBlock)
import           Ouroboros.Consensus.Ledger.SupportsMempool (GenTxId)
import           Ouroboros.Consensus.Network.NodeToNode (Codecs (..), defaultCodecs)
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Node.Run (RunNode)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Ouroboros.Network.Channel (Channel (..))
import           Ouroboros.Network.DeltaQ (defaultGSV)
import           Ouroboros.Network.Driver (runPeerWithLimits)
import           Ouroboros.Network.KeepAlive
import           Ouroboros.Network.Magic
import           Ouroboros.Network.Mux (MuxPeer (..), RunMiniProtocol (..), continueForever)
import           Ouroboros.Network.NodeToClient (chainSyncPeerNull, IOManager)
import           Ouroboros.Network.NodeToNode (NetworkConnectTracers (..))
import qualified Ouroboros.Network.NodeToNode as NtN
import           Ouroboros.Network.Protocol.BlockFetch.Client (BlockFetchClient (..),
                                                               blockFetchClientPeer)
import           Ouroboros.Network.Protocol.Handshake.Version (simpleSingletonVersions)
import           Ouroboros.Network.Protocol.KeepAlive.Codec
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.TxSubmission.Client (TxSubmissionClient,
                                                                 txSubmissionClientPeer)
import           Ouroboros.Network.Protocol.Trans.Hello.Util (wrapClientPeer)

import           Ouroboros.Network.Snocket (socketSnocket)

import           Cardano.Benchmarking.Tracer (SendRecvConnect, SendRecvTxSubmission2)

type CardanoBlock    = Consensus.CardanoBlock  StandardCrypto
type ConnectClient = AddrInfo -> TxSubmissionClient (GenTxId CardanoBlock) (GenTx CardanoBlock) IO () -> IO ()

benchmarkConnectTxSubmit
  :: forall blk. (blk ~ CardanoBlock, RunNode blk )
  => IOManager
  -> Tracer IO SendRecvConnect
  -> Tracer IO SendRecvTxSubmission2
  -> CodecConfig CardanoBlock
  -> NetworkMagic
  -> AddrInfo
  -- ^ remote address information
  -> TxSubmissionClient (GenTxId blk) (GenTx blk) IO ()
  -- ^ the particular txSubmission peer
  -> IO ()

benchmarkConnectTxSubmit ioManager handshakeTracer submissionTracer codecConfig networkMagic remoteAddr myTxSubClient =
  NtN.connectTo
    (socketSnocket ioManager)
    NetworkConnectTracers {
        nctMuxTracer       = nullTracer,
        nctHandshakeTracer = handshakeTracer
      }
    peerMultiplex
    (addrAddress <$> Nothing)
    (addrAddress remoteAddr)
 where
  n2nVer :: NodeToNodeVersion
  n2nVer = NodeToNodeV_7
  blkN2nVer :: BlockNodeToNodeVersion blk
  blkN2nVer = supportedVers Map.! n2nVer
  supportedVers :: Map.Map NodeToNodeVersion (BlockNodeToNodeVersion blk)
  supportedVers = supportedNodeToNodeVersions (Proxy @blk)
  myCodecs :: Codecs blk DeserialiseFailure IO
                ByteString ByteString ByteString ByteString ByteString ByteString ByteString
  myCodecs  = defaultCodecs codecConfig blkN2nVer n2nVer
  peerMultiplex =
    simpleSingletonVersions
      n2nVer
      (NtN.NodeToNodeVersionData
       { NtN.networkMagic = networkMagic
       , NtN.diffusionMode = NtN.InitiatorOnlyDiffusionMode
       }) $
      NtN.nodeToNodeProtocols NtN.defaultMiniProtocolParameters ( \them _ ->
        NtN.NodeToNodeProtocols
          { NtN.chainSyncProtocol = InitiatorProtocolOnly $
                                      MuxPeer
                                        nullTracer
                                        (cChainSyncCodec myCodecs)
                                        chainSyncPeerNull
          , NtN.blockFetchProtocol = InitiatorProtocolOnly $
                                       MuxPeer
                                         nullTracer
                                         (cBlockFetchCodec myCodecs)
                                         (blockFetchClientPeer blockFetchClientNull)
          , NtN.keepAliveProtocol = InitiatorProtocolOnly $
                                      MuxPeerRaw
                                        (kaClient n2nVer them)
          , NtN.txSubmissionProtocol = InitiatorProtocolOnly $
                                         MuxPeer
                                           submissionTracer
                                           (cTxSubmission2Codec myCodecs)
                                           (wrapClientPeer $ txSubmissionClientPeer myTxSubClient)
          } )
        n2nVer
  -- Stolen from: Ouroboros/Consensus/Network/NodeToNode.hs
  kaClient
    :: Ord remotePeer
    => NodeToNodeVersion
    -> remotePeer
    -> Channel IO ByteString
    -> IO ((), Maybe ByteString)
  kaClient version them channel = do
    case version of
      -- Version 1 doesn't support keep alive protocol but Blockfetch
      -- still requires a PeerGSV per peer.
      NodeToNodeV_1 -> forever (threadDelay 1000) >> return ((), Nothing)
      NodeToNodeV_2 -> forever (threadDelay 1000) >> return ((), Nothing)
      _             -> do
        keepAliveRng <- newStdGen
        peerGSVMap <- liftIO . newTVarIO $ Map.singleton them defaultGSV
        runPeerWithLimits
          nullTracer
          (cKeepAliveCodec myCodecs)
          (byteLimitsKeepAlive (const 0)) -- TODO: Real Bytelimits, see #1727
          timeLimitsKeepAlive
          channel
          $ keepAliveClientPeer
          $ keepAliveClient
              nullTracer
              keepAliveRng
              (continueForever (Proxy :: Proxy IO)) them peerGSVMap
              (KeepAliveInterval 10)

-- the null block fetch client
blockFetchClientNull
  :: forall block point m a.  MonadTimer m
  => BlockFetchClient block point m a
blockFetchClientNull
  = BlockFetchClient $ forever $ threadDelay (24 * 60 * 60) {- one day in seconds -}
