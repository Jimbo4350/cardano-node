{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.TraceDispatcher.Peer
  ( PeerT (..)
  , startPeerTracer
  , namesForPeers
  , severityPeers
  , docPeers
  , ppPeer
  ) where

import           Cardano.Logging
import           Cardano.Prelude hiding (atomically)
import           Prelude (String)

import qualified Control.Monad.Class.MonadSTM.Strict as STM

import           Data.Aeson (ToJSON (..), Value (..), toJSON, (.=))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Text.Printf (printf)

import           Ouroboros.Consensus.Block (Header)
import           Ouroboros.Consensus.Node (remoteAddress)
import           Ouroboros.Consensus.Util.Orphans ()

import qualified Ouroboros.Network.AnchoredFragment as Net
import           Ouroboros.Network.Block (unSlotNo)
import qualified Ouroboros.Network.Block as Net
import qualified Ouroboros.Network.BlockFetch.ClientRegistry as Net
import           Ouroboros.Network.BlockFetch.ClientState
                     (PeerFetchInFlight (..), PeerFetchStatus (..),
                     readFetchClientState)

import           Cardano.Tracing.Kernel

startPeerTracer ::
     Trace IO [PeerT blk]
  -> NodeKernelData blk
  -> IO ()
startPeerTracer tr nodeKern = do
    void $ forkIO $ forever $ do
      peers <- getCurrentPeers nodeKern
      traceWith tr peers
      threadDelay 2000000 -- 2 seconds. TODO JNF:  make configurable

data PeerT blk = PeerT
    RemoteConnectionId
    (Net.AnchoredFragment (Header blk))
    (PeerFetchStatus (Header blk))
    (PeerFetchInFlight (Header blk))


ppPeer :: PeerT blk -> Text
ppPeer (PeerT cid _af status inflight) =
  Text.pack $ printf "%-15s %-8s %s" (ppCid cid) (ppStatus status) (ppInFlight inflight)

ppCid :: RemoteConnectionId -> String
ppCid = takeWhile (/= ':') . show . remoteAddress

ppInFlight :: PeerFetchInFlight header -> String
ppInFlight f = printf
 "%5s  %3d  %5d  %6d"
 (ppMaxSlotNo $ peerFetchMaxSlotNo f)
 (peerFetchReqsInFlight f)
 (Set.size $ peerFetchBlocksInFlight f)
 (peerFetchBytesInFlight f)

ppMaxSlotNo :: Net.MaxSlotNo -> String
ppMaxSlotNo Net.NoMaxSlotNo   = "???"
ppMaxSlotNo (Net.MaxSlotNo x) = show (unSlotNo x)

ppStatus :: PeerFetchStatus header -> String
ppStatus PeerFetchStatusShutdown = "shutdown"
ppStatus PeerFetchStatusAberrant = "aberrant"
ppStatus PeerFetchStatusBusy     = "fetching"
ppStatus PeerFetchStatusReady {} = "ready"

getCurrentPeers
  :: NodeKernelData blk
  -> IO [PeerT blk]
getCurrentPeers nkd = mapNodeKernelDataIO extractPeers nkd
                      <&> fromSMaybe mempty
 where
  tuple3pop :: (a, b, c) -> (a, b)
  tuple3pop (a, b, _) = (a, b)

  getCandidates
    :: STM.StrictTVar IO (Map peer (STM.StrictTVar IO (Net.AnchoredFragment (Header blk))))
    -> STM.STM IO (Map peer (Net.AnchoredFragment (Header blk)))
  getCandidates var = STM.readTVar var >>= traverse STM.readTVar

  extractPeers :: NodeKernel IO RemoteConnectionId LocalConnectionId blk
                -> IO [PeerT blk]
  extractPeers kernel = do
    peerStates <- fmap tuple3pop <$> (   STM.atomically
                                       . (>>= traverse readFetchClientState)
                                       . Net.readFetchClientsStateVars
                                       . getFetchClientRegistry $ kernel
                                     )
    candidates <- STM.atomically . getCandidates . getNodeCandidates $ kernel

    let peers = flip Map.mapMaybeWithKey candidates $ \cid af ->
                  maybe Nothing
                        (\(status, inflight) -> Just $ PeerT cid af status inflight)
                        $ Map.lookup cid peerStates
    pure . Map.elems $ peers

namesForPeers :: [PeerT blk] -> [Text]
namesForPeers _ = []

severityPeers :: [PeerT blk] -> SeverityS
severityPeers _ = Notice

docPeers :: Documented [PeerT blk]
docPeers = Documented [
      DocMsg
        []
        [(["peersFromNodeKernel"],"TODO Doc")]
        "TODO Doc"
    ]

instance LogFormatting [PeerT blk] where
  forMachine DBrief _ = mkObject [ "kind"  .= String "NodeKernelPeers"]
  forMachine _ []     = mkObject [ "kind"  .= String "NodeKernelPeers"]
  forMachine dtal xs  = mkObject
    [ "kind"  .= String "NodeKernelPeers"
    , "peers" .= toJSON (foldl' (\acc x -> forMachine dtal x : acc) [] xs)
    ]
  forHuman peers = Text.concat $ intersperse ", " (map ppPeer peers)
  asMetrics peers = [IntM ["peersFromNodeKernel"] (fromIntegral (length peers))]

instance LogFormatting (PeerT blk) where
  forMachine _dtal (PeerT cid _af status inflight) =
    mkObject [ "peerAddress"   .= String (Text.pack . show . remoteAddress $ cid)
             , "peerStatus"    .= String (Text.pack . ppStatus $ status)
             , "peerSlotNo"    .= String (Text.pack . ppMaxSlotNo . peerFetchMaxSlotNo $ inflight)
             , "peerReqsInF"   .= String (show . peerFetchReqsInFlight $ inflight)
             , "peerBlocksInF" .= String (show . Set.size . peerFetchBlocksInFlight $ inflight)
             , "peerBytesInF"  .= String (show . peerFetchBytesInFlight $ inflight)
             ]
