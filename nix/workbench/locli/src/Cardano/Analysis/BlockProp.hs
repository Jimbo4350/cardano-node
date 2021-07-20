{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-partial-fields -Wno-unused-matches -Wno-deprecations -Wno-unused-local-binds -Wno-incomplete-record-updates #-}
{- HLINT ignore "Use head" -}
{- HLINT ignore "Avoid lambda" -}
module Cardano.Analysis.BlockProp (module Cardano.Analysis.BlockProp) where

import           Prelude (String, (!!), error, head, id, show, tail)
import           Cardano.Prelude hiding (head, show)

import           Control.Arrow ((***), (&&&))
import Control.DeepSeq qualified as DS
import           Control.Concurrent.Async (mapConcurrently)
import           Data.Aeson (ToJSON(..), FromJSON(..))
import qualified Data.Aeson as AE
import           Data.Bifunctor
import           Data.Function (on)
import           Data.List (dropWhileEnd, intercalate)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, mapMaybe, isNothing)
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Text.Short (toText)
import           Data.Tuple (swap)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

import           Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime)

import           Text.Printf (printf)

import           Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import           Data.Accum
import           Data.Distribution
import           Cardano.Analysis.Profile
import           Cardano.Unlog.LogObject hiding (Text)
import           Cardano.Unlog.Render
import           Cardano.Unlog.Resources
import           Cardano.Unlog.SlotStats

import qualified Debug.Trace as D


data BlockPropagation
  = BlockPropagation
    { bpForgerForges        :: !(Distribution Float NominalDiffTime)
    , bpForgerAdoptions     :: !(Distribution Float NominalDiffTime)
    , bpForgerAnnouncements :: !(Distribution Float NominalDiffTime)
    , bpForgerSends         :: !(Distribution Float NominalDiffTime)
    , bpPeerNotices         :: !(Distribution Float NominalDiffTime)
    , bpPeerRequests        :: !(Distribution Float NominalDiffTime)
    , bpPeerFetches         :: !(Distribution Float NominalDiffTime)
    , bpPeerAdoptions       :: !(Distribution Float NominalDiffTime)
    , bpPeerAnnouncements   :: !(Distribution Float NominalDiffTime)
    , bpPeerSends           :: !(Distribution Float NominalDiffTime)
    , bpChainBlockEvents    :: [BlockEvents]
    }
  deriving Show

instance RenderDistributions BlockPropagation where
  rdFields =
    --  Width LeftPad
    [ Field 6 0 "forged"        (f!!0) "Forge"  $ DDeltaT bpForgerForges
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ DDeltaT bpForgerAdoptions
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ DDeltaT bpForgerAnnouncements
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ DDeltaT bpForgerSends
    , Field 4 1 "noticedVal"    (p!!0) "Notic"  $ DDeltaT bpPeerNotices
    , Field 4 1 "requestedVal"  (p!!1) "Reque"  $ DDeltaT bpPeerRequests
    , Field 4 1 "fetchedVal"    (p!!2) "Fetch"  $ DDeltaT bpPeerFetches
    , Field 4 1 "pAdoptedVal"   (p!!3) "Adopt"  $ DDeltaT bpPeerAdoptions
    , Field 4 1 "pAnnouncedVal" (p!!4) "Annou"  $ DDeltaT bpPeerAnnouncements
    , Field 4 1 "pSendStartVal" (p!!5) "Send"   $ DDeltaT bpPeerSends
    ]
   where
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 5 "Peer event Δt:"

instance AE.ToJSON BlockPropagation where
  toJSON BlockPropagation{..} = AE.Array $ Vec.fromList
    [ extendObject "kind" "forgerForges"        $ toJSON bpForgerForges
    , extendObject "kind" "forgerAdoptions"     $ toJSON bpForgerAdoptions
    , extendObject "kind" "forgerAnnouncements" $ toJSON bpForgerAnnouncements
    , extendObject "kind" "forgerSends"         $ toJSON bpForgerSends
    , extendObject "kind" "peerNoticesMean"       $ toJSON bpPeerNotices
    , extendObject "kind" "peerRequestsMean"      $ toJSON bpPeerRequests
    , extendObject "kind" "peerFetchesMean"       $ toJSON bpPeerFetches
    , extendObject "kind" "peerAdoptionsMean"     $ toJSON bpPeerAdoptions
    , extendObject "kind" "peerAnnouncementsMean" $ toJSON bpPeerAnnouncements
    , extendObject "kind" "peerSendsMean"         $ toJSON bpPeerSends
    ]

data BPError
  = BPError
  { eBlock :: !Hash
  , eFile  :: !(Maybe FilePath)
  , eLO    :: !(Maybe LogObject)
  , eDesc  :: !BPErrorKind
  }
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

data BPErrorKind
  = BPEBefore                !Phase !Phase
  | BPEUnexpectedForObserver !Phase
  | BPEUnexpectedForForger   !Phase
  | BPEUnexpectedAsFirst     !Phase
  | BPEDuplicateForge
  | BPEMissingPhase          !Phase
  | BPENegativePhase         !Phase !NominalDiffTime
  | BPEFork                  !Hash
  deriving (FromJSON, Generic, NFData, Show, ToJSON)

bpeIsFork, bpeIsMissingAny, bpeIsNegativeAny  :: BPError -> Bool
bpeIsFork BPError{eDesc=BPEFork{}} = True
bpeIsFork _ = False
bpeIsMissingAny BPError{eDesc=BPEMissingPhase{}} = True
bpeIsMissingAny _ = False
bpeIsNegativeAny BPError{eDesc=BPENegativePhase{}} = True
bpeIsNegativeAny _ = False

bpeIsMissing, bpeIsNegative  :: Phase -> BPError -> Bool
bpeIsMissing  p BPError{eDesc=BPEMissingPhase p'} = p == p'
bpeIsMissing  _ _ = False
bpeIsNegative p BPError{eDesc=BPENegativePhase p' _} = p == p'
bpeIsNegative _ _ = False

data Phase
  = Notice
  | Request
  | Fetch
  | Forge
  | Acquire
  | Adopt
  | Announce
  | Send
  deriving (FromJSON, Eq, Generic, NFData, Ord, Show, ToJSON)

-- | Block's events, as seen by its forger.
data ForgerEvents a
  =  ForgerEvents
  { bfeHost       :: !Host
  , bfeBlock      :: !Hash
  , bfeBlockPrev  :: !Hash
  , bfeBlockNo    :: !BlockNo
  , bfeSlotNo     :: !SlotNo
  , bfeSlotStart  :: !SlotStart
  , bfeForged     :: !(Maybe a)
  , bfeAdopted    :: !(Maybe a)
  , bfeChainDelta :: !Int
  , bfeAnnounced  :: !(Maybe a)
  , bfeSending    :: !(Maybe a)
  , bfeErrs       :: [BPError]
  }
  deriving (Generic, NFData, AE.FromJSON, AE.ToJSON, Show)

type ForgerEventsAbs = ForgerEvents UTCTime
type ForgerEventsRel = ForgerEvents NominalDiffTime

bfePrevBlock :: ForgerEvents a -> Maybe Hash
bfePrevBlock x = case bfeBlockNo x of
  0 -> Nothing
  _ -> Just $ bfeBlockPrev x

-- | Block's events, as seen by an observer.
data ObserverEvents a
  =  ObserverEvents
  { boeHost       :: !Host
  , boeBlock      :: !Hash
  , boeBlockNo    :: !BlockNo
  , boeSlotNo     :: !SlotNo
  , boeSlotStart  :: !SlotStart
  , boeNoticed    :: !(Maybe a)
  , boeRequested  :: !(Maybe a)
  , boeFetched    :: !(Maybe a)
  , boeAdopted    :: !(Maybe a)
  , boeChainDelta :: !Int
  , boeAnnounced  :: !(Maybe a)
  , boeSending    :: !(Maybe a)
  , boeErrs       :: [BPError]
  }
  deriving (Generic, NFData, AE.FromJSON, AE.ToJSON, Show)

type ObserverEventsAbs = ObserverEvents UTCTime
type ObserverEventsRel = ObserverEvents NominalDiffTime

mbePhaseIndex :: Map Phase (MachBlockEvents a -> Maybe a)
mbePhaseIndex = Map.fromList
  [ (Notice,     mbeNoticed)
  , (Request,    mbeRequested)
  , (Fetch,      mbeAcquired)
  , (Forge,      mbeAcquired)
  , (Acquire,    mbeAcquired)
  , (Adopt,      mbeAdopted)
  , (Announce,   mbeAnnounced)
  , (Send,       mbeSending)
  ]

mbeGetProjection :: Phase -> (MachBlockEvents a -> Maybe a)
mbeGetProjection k =
  Map.lookup k mbePhaseIndex
  & fromMaybe (error $ "Unknown phase: " <> show k)

-- | Sum of observer and forger events alike.
data MachBlockEvents a
  = MFE (ForgerEvents a)
  | MOE (ObserverEvents a)
  | MBE  BPError
  deriving (Generic, NFData)

mbeForgP, mbeObsvP, mbeErrP :: MachBlockEvents a -> Bool
mbeForgP = \case
  MFE{} -> True
  _ -> False
mbeObsvP = \case
  MOE{} -> True
  _ -> False
mbeErrP = \case
  MBE{} -> True
  _ -> False

mapMbe :: (ForgerEvents a -> b) -> (ObserverEvents a -> b) -> (BPError -> b)
       -> MachBlockEvents a -> b
mapMbe f o e = \case
  MFE x -> f x
  MOE x -> o x
  MBE x -> e x

mapMbeErrs :: ([BPError] -> [BPError]) -> MachBlockEvents a -> MachBlockEvents a
mapMbeErrs f = mapMbe (\x -> MFE x { bfeErrs=f $ bfeErrs x } )
                      (\x -> MOE x { boeErrs=f $ boeErrs x } )
                      MBE

partitionMbes :: [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
partitionMbes = go [] [] []
  where
    go :: [ForgerEvents a] -> [ObserverEvents a] -> [BPError] -> [MachBlockEvents a] -> ([ForgerEvents a], [ObserverEvents a], [BPError])
    go as bs cs [] = (reverse as, reverse bs, reverse cs)
    go as bs cs (MFE a:xs) = go (a:as) bs cs xs
    go as bs cs (MOE b:xs) = go as (b:bs) cs xs
    go as bs cs (MBE c:xs) = go as bs (c:cs) xs

errorMbes :: [MachBlockEvents a] -> [BPError]
errorMbes = go []
  where
    go :: [BPError] -> [MachBlockEvents a] -> [BPError]
    go cs [] = reverse cs
    go cs (MBE c:xs) = go (c:cs) xs
    go cs (_:xs)     = go    cs  xs

trimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> (BPError -> BPError)
  -> MachBlockEvents a -> MachBlockEvents a
trimapMbe f o e = mapMbe (MFE . f) (MOE . o) (MBE . e)

bimapMbe ::
     (ForgerEvents a -> ForgerEvents a)
  -> (ObserverEvents a -> ObserverEvents a)
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe f o = trimapMbe f o id

bimapMbe' ::
     (ForgerEvents   a -> Either BPError (ForgerEvents   a))
  -> (ObserverEvents a -> Either BPError (ObserverEvents a))
  -> MachBlockEvents a -> MachBlockEvents a
bimapMbe' f o = \case
  MFE x -> either MBE MFE (f x)
  MOE x -> either MBE MOE (o x)
  x@MBE{} -> x

ordBlockEv :: MachBlockEvents a -> MachBlockEvents a -> Ordering
ordBlockEv l r
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) l r = GT
  | (on (>) $ mapMbe bfeBlockNo boeBlockNo (const 0)) r l = LT
  | mbeForgP l = GT
  | mbeForgP r = LT
  | mbeObsvP l = GT
  | mbeObsvP r = LT
  | otherwise  = EQ

mbeSlotStart :: MachBlockEvents a -> SlotStart
mbeSlotStart = mapMbe bfeSlotStart boeSlotStart (SlotStart . const zeroUTCTime)

mbeNoticed, mbeRequested, mbeAcquired, mbeAdopted, mbeAnnounced, mbeSending :: MachBlockEvents a -> Maybe a
mbeNoticed   = mapMbe (const Nothing)  boeNoticed   (const Nothing)
mbeRequested = mapMbe (const Nothing)  boeRequested (const Nothing)
mbeAcquired  = mapMbe bfeForged        boeFetched   (const Nothing)
mbeAdopted   = mapMbe bfeAdopted       boeAdopted   (const Nothing)
mbeAnnounced = mapMbe bfeAnnounced     boeAnnounced (const Nothing)
mbeSending   = mapMbe bfeSending       boeSending   (const Nothing)

mbeBlock :: MachBlockEvents a -> Hash
mbeBlock = mapMbe bfeBlock boeBlock eBlock

mbeBlockNo :: MachBlockEvents a -> BlockNo
mbeBlockNo = mapMbe bfeBlockNo boeBlockNo (const (-1))

mbeError :: MachBlockEvents a -> Maybe BPError
mbeError = mapMbe (const Nothing) (const Nothing) Just

mbeFailed :: MachBlockEvents a -> Bool
mbeFailed = isJust . mbeError

-- | Machine's private view of all the blocks.
type MachBlockMap a
  =  Map.Map Hash (MachBlockEvents a)

blockMapMaxBlock :: MachBlockMap a -> MachBlockEvents a
blockMapMaxBlock = maximumBy ordBlockEv . Map.elems

blockMapBlock :: Hash -> MachBlockMap a -> MachBlockEvents a
blockMapBlock h =
  fromMaybe (error $ "Invariant failed:  missing hash " <> show h) . Map.lookup h

-- | A completed, compactified version of ObserverEvents.
data BlockObservation
  =  BlockObservation
  { boObserver   :: !Host
  , boSlotStart  :: !SlotStart
  , boNoticed    :: !NominalDiffTime
  , boRequested  :: !NominalDiffTime
  , boFetched    :: !NominalDiffTime
  , boAdopted    :: !(Maybe NominalDiffTime)
  , boChainDelta :: !Int -- ^ ChainDelta during adoption
  , boAnnounced  :: !(Maybe NominalDiffTime)
  , boSending    :: !(Maybe NominalDiffTime)
  , boErrors     :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

-- | All events related to a block.
data BlockEvents
  =  BlockEvents
  { beForger       :: !Host
  , beBlock        :: !Hash
  , beBlockPrev    :: !Hash
  , beBlockNo      :: !BlockNo
  , beSlotNo       :: !SlotNo
  , beSlotStart    :: !SlotStart
  , beForged       :: !NominalDiffTime
  , beAdopted      :: !NominalDiffTime
  , beChainDelta   :: !Int -- ^ ChainDelta during adoption
  , beAnnounced    :: !NominalDiffTime
  , beSending      :: !NominalDiffTime
  , beObservations :: [BlockObservation]
  , beValidObservs :: [BlockObservation]
  , beOtherBlocks  :: [Hash]
  , beErrors       :: [BPError]
  }
  deriving (Generic, AE.FromJSON, AE.ToJSON, Show)

instance RenderTimeline BlockEvents where
  rtFields =
    --  Width LeftPad
    [ Field 5 0 "block"        "block" "no."    $ IWord64 (unBlockNo . beBlockNo)
    , Field 5 0 "abs.slot"     "abs."  "slot#"  $ IWord64 (unSlotNo . beSlotNo)
    , Field 6 0 "hash"         "block" "hash"   $ IText   (shortHash . beBlock)
    , Field 6 0 "hashPrev"     "prev"  "hash"   $ IText   (shortHash . beBlockPrev)
    , Field 6 0 "forger"       "forger" "host"  $ IText  (toText . unHost . beForger)
    , Field 6 0 "forged"        (f!!0) "Forge"  $ IDeltaT beForged
    , Field 6 0 "fAdopted"      (f!!1) "Adopt"  $ IDeltaT beAdopted
    , Field 6 0 "fAnnounced"    (f!!2) "Announ" $ IDeltaT beAnnounced
    , Field 6 0 "fSendStart"    (f!!3) "Sendin" $ IDeltaT beSending
    , Field 5 0 "valid.observ"  "valid" "obsrv" $ IInt    (length . beValidObservs)
    , Field 5 0 "noticedVal"    (p!!0) "Notic"  $ IDeltaT (af boNoticed . beValidObservs)
    , Field 5 0 "requestedVal"  (p!!1) "Requd"  $ IDeltaT (af boRequested . beValidObservs)
    , Field 5 0 "fetchedVal"    (p!!2) "Fetch"  $ IDeltaT (af boFetched . beValidObservs)
    , Field 5 0 "pAdoptedVal"   (p!!3) "Adopt"  $ IDeltaT (af' boAdopted . beValidObservs)
    , Field 5 0 "pAnnouncedVal" (p!!4) "Annou"  $ IDeltaT (af' boAnnounced . beValidObservs)
    , Field 5 0 "pSendStartVal" (p!!5) "Send"   $ IDeltaT (af' boSending . beValidObservs)
    , Field 5 0 "errors"        "all"  "errs"   $ IInt    (length . beErrors)
    , Field 5 0 "forks"         ""     "forks"  $ IInt   (count bpeIsFork . beErrors)
    , Field 5 0 "missAdopt"     (m!!0) "adopt"  $ IInt    (count (bpeIsMissing Adopt) . beErrors)
    , Field 5 0 "missAnnou"     (m!!1) "annou"  $ IInt    (count (bpeIsMissing Announce) . beErrors)
    , Field 5 0 "missSend"      (m!!2) "send"   $ IInt    (count (bpeIsMissing Send) . beErrors)
    , Field 5 0 "negAnnou"      (n!!0) "annou"  $ IInt    (count (bpeIsNegative Announce) . beErrors)
    , Field 5 0 "negSend"       (n!!1) "send"   $ IInt    (count (bpeIsNegative Send) . beErrors)
    ]
   where
     f = nChunksEachOf 4 7 "Forger event Δt:"
     p = nChunksEachOf 6 6 "Peer event Δt:"
     m = nChunksEachOf 3 6 "Missing phase"
     n = nChunksEachOf 2 6 "Negative phase"
     af  f = avg . fmap f
     af' f = avg . mapMaybe f
     avg :: [NominalDiffTime] -> NominalDiffTime
     avg [] = 0
     avg xs =  (/ fromInteger (fromIntegral $ length xs)) $ sum xs
     count :: (a -> Bool) -> [a] -> Int
     count f = length . filter f
  rtCommentary BlockEvents{..} = ("    " <>) . T.pack . show <$> beErrors

mapChainToForgerEventCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockEvents -> Maybe NominalDiffTime)
  -> Distribution Float NominalDiffTime
mapChainToForgerEventCDF percs cbe proj =
  computeDistribution percs (mapMaybe proj cbe)

mapChainToPeerBlockObservationCDF ::
     [PercSpec Float]
  -> [BlockEvents]
  -> (BlockObservation -> Maybe NominalDiffTime)
  -> String
  -> Distribution Float NominalDiffTime
mapChainToPeerBlockObservationCDF percs chainBlockEvents proj desc =
  computeDistribution percs allObservations
 where
   allObservations :: [NominalDiffTime]
   allObservations =
     concat $
     filter isValidBlockEvent chainBlockEvents
     <&> blockObservations

   blockObservations :: BlockEvents -> [NominalDiffTime]
   blockObservations be = mapMaybe proj (beValidObservs be)

isValidBlockEvent :: BlockEvents -> Bool
isValidBlockEvent BlockEvents{..} = beChainDelta == 1

isValidBlockObservation :: BlockObservation -> Bool
isValidBlockObservation BlockObservation{..} =
  -- 1. All phases are present
  null boErrors
  &&
  -- 2. All timings account for processing of a single block
  boChainDelta == 1

blockProp :: ChainInfo -> [(JsonLogfile, [LogObject])] -> IO BlockPropagation
blockProp ci xs = do
  putStrLn ("blockProp: recovering block event maps" :: String)
  doBlockProp =<< mapConcurrently (\x -> do
                                      let evs = fmap deltifyEvents $
                                                blockEventMapsFromLogObjects ci x
                                      pure $ DS.rnf evs
                                      pure evs) xs

doBlockProp :: [MachBlockMap NominalDiffTime] -> IO BlockPropagation
doBlockProp eventMaps = do
  putStrLn ("tip block: "    <> show tipBlock :: String)
  putStrLn ("chain length: " <> show (length chain) :: String)
  pure BlockPropagation
    { bpForgerForges      = forgerEventsCDF   (Just . beForged)
    , bpForgerAdoptions   = forgerEventsCDF   (\x -> if beChainDelta x == 1
                                                     then Just (beAdopted x)
                                                     else Nothing)
    , bpForgerAnnouncements
                          = forgerEventsCDF   (Just . beAnnounced)
    , bpForgerSends       = forgerEventsCDF   (Just . beSending)
    , bpPeerNotices       = observerEventsCDF (Just . boNoticed)   "noticed"
    , bpPeerRequests      = observerEventsCDF (Just . boRequested) "requested"
    , bpPeerFetches       = observerEventsCDF (Just . boFetched)   "fetched"
    , bpPeerAdoptions     = observerEventsCDF boAdopted            "adopted"
    , bpPeerAnnouncements = observerEventsCDF boAnnounced          "announced"
    , bpPeerSends         = observerEventsCDF boSending            "sending"
    , bpChainBlockEvents  = chain
    }
 where
   forgerEventsCDF   = mapChainToForgerEventCDF          stdPercentiles chain
   observerEventsCDF = mapChainToPeerBlockObservationCDF stdPercentiles chain

   chain          :: [BlockEvents]
   chain          = rebuildChain eventMaps tipHash
   heightMap      :: Map BlockNo (Set Hash)
   heightMap      = foldr (\em acc ->
                             Map.foldr
                             (\mbe -> Map.alter
                                      (maybe (Just $ Set.singleton (mbeBlock mbe))
                                             (Just . Set.insert (mbeBlock mbe)))
                                      (mbeBlockNo mbe))
                             acc em)
                    mempty eventMaps
   tipBlock       = getBlockForge eventMaps tipHash
   tipHash        = rewindChain eventMaps 1 (mbeBlock finalBlockEv)
   finalBlockEv   = maximumBy ordBlockEv $ blockMapMaxBlock <$> eventMaps

   rewindChain :: [MachBlockMap a] -> Int -> Hash -> Hash
   rewindChain eventMaps count tip = go tip count
    where go tip = \case
            0 -> tip
            n -> go (bfeBlockPrev $ getBlockForge eventMaps tip) (n - 1)

   getBlockForge :: [MachBlockMap a] -> Hash -> ForgerEvents a
   getBlockForge xs h =
     mapMaybe (Map.lookup h) xs
     & find mbeForgP
     & fromMaybe
        (error $ mconcat
         [ "Invariant failed: couldn't find a forge for hash ", show h
         , "\nErrors:\n", show (intercalate "\n" $ fmap show $ errorMbes $ mapMaybe (Map.lookup h) xs)
         ])
     & mapMbe id (error "Silly invariant failed.") (error "Silly invariant failed.")

   rebuildChain :: [MachBlockMap NominalDiffTime] -> Hash -> [BlockEvents]
   rebuildChain machBlockMaps tip = go (Just tip) []
    where go Nothing  acc = acc
          go (Just h) acc =
            case partitionMbes $ mapMaybe (Map.lookup h) machBlockMaps of
              ([], _, ers) -> error $ mconcat
                [ "No forger for hash ", show h
                , "\nErrors:\n"
                ] ++ intercalate "\n" (show <$> ers)
              blkEvs@(forgerEv:_, oEvs, ers) ->
                go (bfePrevBlock forgerEv) (liftBlockEvents forgerEv oEvs ers : acc)

   liftBlockEvents :: ForgerEventsRel -> [ObserverEvents NominalDiffTime] -> [BPError] -> BlockEvents
   liftBlockEvents ForgerEvents{..} os errs =
     BlockEvents
     { beForger     = bfeHost
     , beBlock      = bfeBlock
     , beBlockPrev  = bfeBlockPrev
     , beBlockNo    = bfeBlockNo
     , beSlotNo     = bfeSlotNo
     , beSlotStart  = bfeSlotStart
     , beForged     = bfeForged    & handleMiss "Δt Forged"
     , beAdopted    = bfeAdopted   & handleMiss "Δt Adopted (forger)"
     , beChainDelta = bfeChainDelta
     , beAnnounced  = bfeAnnounced & handleMiss "Δt Announced (forger)"
     , beSending    = bfeSending   & handleMiss "Δt Sending (forger)"
     , beObservations = observs
     , beValidObservs = observs & filter isValidBlockObservation
     , beOtherBlocks = otherBlocks
     , beErrors =
         errs
         <> (fail' bfeBlock . BPEFork <$> otherBlocks)
         <> bfeErrs
         <> concatMap boeErrs os
     }
    where
      observs =
        catMaybes $
        os <&> \ObserverEvents{..}->
          BlockObservation
            <$> Just boeHost
            <*> Just bfeSlotStart
            <*> boeNoticed
            <*> boeRequested
            <*> boeFetched
            <*> Just boeAdopted
            <*> Just boeChainDelta
            <*> Just boeAnnounced
            <*> Just boeSending
            <*> Just boeErrs
      otherBlocks = Map.lookup bfeBlockNo heightMap
                    & handleMiss "height map"
                    & Set.delete bfeBlock
                    & Set.toList
      fail' :: Hash -> BPErrorKind -> BPError
      fail' hash desc = BPError hash Nothing Nothing desc

      handleMiss :: String -> Maybe a -> a
      handleMiss slotDesc = fromMaybe $ error $ mconcat
       [ "While processing ", show bfeBlockNo, " hash ", show bfeBlock
       , " forged by ", show bfeHost
       , " -- missing: ", slotDesc
       ]

-- | Given a single machine's log object stream, recover its block map.
blockEventMapsFromLogObjects :: ChainInfo -> (JsonLogfile, [LogObject]) -> MachBlockMap UTCTime
blockEventMapsFromLogObjects ci (f@(unJsonLogfile -> fp), xs) =
  trace ("processing " <> fp)
  $ if Map.size machBlockMap == 0
    then error $ mconcat
         ["No block events in ",fp," : ","LogObject count: ",show (length xs)]
    else machBlockMap
 where
   machBlockMap = foldl (blockPropMachEventsStep ci f) mempty xs

blockPropMachEventsStep :: ChainInfo -> JsonLogfile -> MachBlockMap UTCTime -> LogObject -> MachBlockMap UTCTime
blockPropMachEventsStep ci (JsonLogfile fp) bMap lo = case lo of
  -- 0. Notice (observer only)
  LogObject{loAt, loHost, loBody=LOChainSyncClientSeenHeader{loBlock,loBlockNo,loSlotNo}} ->
    let mbe0 = Map.lookup loBlock bMap
    in if isJust mbe0 then bMap else
      MOE
       (ObserverEvents
        loHost loBlock loBlockNo loSlotNo
        (slotStart ci loSlotNo) (Just loAt)
        Nothing Nothing Nothing 0 Nothing Nothing [])
      & doInsert loBlock
  -- 1. Request (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientRequested{loBlock,loLength}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Request)
    in if isJust (mbeRequested mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loBlock $ BPEUnexpectedForForger Request)
      (\x -> Right x { boeRequested=Just loAt, boeChainDelta=loLength `max` boeChainDelta x })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Fetch (observer only)
  LogObject{loAt, loHost, loBody=LOBlockFetchClientCompletedFetch{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Fetch)
    in if isJust (mbeAcquired mbe0) then bMap else
      bimapMbe'
      (const . Left $ fail' loBlock (BPEUnexpectedForForger Fetch))
      (\x -> Right x { boeFetched=Just loAt })
      mbe0
      & doInsert loBlock
  -- 2. Acquire:Forge (forger only)
  LogObject{loAt, loHost, loBody=LOBlockForged{loBlock,loPrev,loBlockNo,loSlotNo}} ->
    Map.lookup loBlock bMap
    <&> bimapMbe'
          (const.Left $
           BPError loBlock (Just fp) (Just lo) BPEDuplicateForge)
          (const.Left $
           BPError loBlock (Just fp) (Just lo) (BPEUnexpectedForObserver Forge))
    & fromMaybe
      (MFE $ ForgerEvents
        loHost loBlock loPrev loBlockNo loSlotNo
        (slotStart ci loSlotNo) (Just loAt)
        Nothing 0 Nothing Nothing [])
    & doInsert loBlock
  -- 3. Adopt
  LogObject{loAt, loHost, loBody=LOBlockAddedToCurrentChain{loBlock,loLength}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Adopt)
    in if isJust (mbeAdopted mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeAdopted=Just loAt, bfeChainDelta=loLength })
      (\x -> x { boeAdopted=Just loAt, boeChainDelta=loLength `max` boeChainDelta x})
      mbe0
      & doInsert loBlock
  -- 4. Announce
  LogObject{loAt, loHost, loBody=LOChainSyncServerSendHeader{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Announce)
    in if isJust (mbeAnnounced mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeAnnounced=Just loAt })
      (\x -> x { boeAnnounced=Just loAt })
      mbe0
      & doInsert loBlock
  -- 5. Sending started
  LogObject{loAt, loHost, loBody=LOBlockFetchServerSending{loBlock}} ->
    let mbe0 = Map.lookup loBlock bMap
               & fromMaybe (fail loBlock $ BPEUnexpectedAsFirst Send)
    in if isJust (mbeSending mbe0) then bMap else
      bimapMbe
      (\x -> x { bfeSending=Just loAt })
      (\x -> x { boeSending=Just loAt })
      mbe0
      & doInsert loBlock
  _ -> bMap
 where
   fail' :: Hash -> BPErrorKind -> BPError
   fail' hash desc = BPError hash (Just fp) (Just lo) desc

   fail :: Hash -> BPErrorKind -> MachBlockEvents a
   fail hash desc = MBE $ fail' hash desc

   doInsert :: Hash -> MachBlockEvents UTCTime -> MachBlockMap UTCTime
   doInsert k x = Map.insert k x bMap

deltifyEvents :: MachBlockEvents UTCTime -> MachBlockEvents NominalDiffTime
deltifyEvents (MBE e) = MBE e
deltifyEvents (MFE x@ForgerEvents{..}) =
  MFE x
  { bfeForged    = bfeForged <&> (`sinceSlot` bfeSlotStart)
  , bfeAdopted   = diffUTCTime <$> bfeAdopted   <*> bfeForged
  , bfeAnnounced = diffUTCTime <$> bfeAnnounced <*> bfeAdopted
  , bfeSending   = diffUTCTime <$> bfeSending   <*> bfeAnnounced
  } & \case
  v@(MFE x') -> MFE x' { bfeErrs = collectEventErrors v
                         [Forge, Adopt, Announce, Send] }
  _ -> error "Impossible"
deltifyEvents (MOE x@ObserverEvents{..}) =
  MOE x
  { boeNoticed   = boeNoticed <&> (`sinceSlot` boeSlotStart)
  , boeRequested = diffUTCTime <$> boeRequested <*> boeNoticed
  , boeFetched   = diffUTCTime <$> boeFetched   <*> boeRequested
  , boeAdopted   = diffUTCTime <$> boeAdopted   <*> boeFetched
  , boeAnnounced = diffUTCTime <$> boeAnnounced <*> boeAdopted
  , boeSending   = diffUTCTime <$> boeSending   <*> boeAnnounced
  } & \case
  v@(MOE x') -> MOE x' { boeErrs = collectEventErrors v
                         [Notice, Request, Fetch, Adopt, Announce, Send] }
  _ -> error "Impossible"

collectEventErrors :: MachBlockEvents NominalDiffTime -> [Phase] -> [BPError]
collectEventErrors mbe phases =
  [ BPError (mbeBlock mbe) Nothing Nothing $
    case (miss, proj) of
      (,) True _       -> BPEMissingPhase phase
      (,) _ (Just neg) -> BPENegativePhase phase neg
      _ -> error "Impossible."
  | phase <- phases
  , let proj = mbeGetProjection phase mbe
  , let miss = isNothing proj
  , let neg  = ((< 0) <$> proj) == Just True
  , miss || neg
  ]
