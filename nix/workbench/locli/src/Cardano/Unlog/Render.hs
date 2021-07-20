{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Cardano.Unlog.Render (module Cardano.Unlog.Render) where

import           Prelude (head, id, tail)
import           Cardano.Prelude hiding (head)

import           Control.Arrow ((&&&))
import           Data.List (dropWhileEnd)
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Text.Printf (printf)

import           Data.Distribution


data RenderMode
  = RenderPretty
  | RenderCsv
  deriving (Eq, Show)

class Show a => RenderDistributions a where
  rdFields :: [DField a]

class Show a => RenderTimeline a where
  rtFields     :: [IField a]
  rtCommentary :: a -> [Text]
  rtCommentary _ = []

-- | Incapsulate all information necessary to render a column.
data Field s a
  = Field
  { fWidth   :: Int
  , fLeftPad :: Int
  , fId      :: Text
  , fHead1   :: Text
  , fHead2   :: Text
  , fSelect  :: s a
  }

type DField a = Field DSelect a
type IField a = Field ISelect a

data DSelect a
  = DInt    (a -> Distribution Float Int)
  | DWord64 (a -> Distribution Float Word64)
  | DFloat  (a -> Distribution Float Float)
  | DDeltaT (a -> Distribution Float NominalDiffTime)

data ISelect a
  = IInt    (a -> Int)
  | IWord64 (a -> Word64)
  | IFloat  (a -> Float)
  | IDeltaT (a -> NominalDiffTime)
  | IText   (a -> Text)

mapSomeFieldDistribution :: (forall b. Distribution Float b -> c) -> a -> DSelect a -> c
mapSomeFieldDistribution f a = \case
  DInt    s -> f (s a)
  DWord64 s -> f (s a)
  DFloat  s -> f (s a)
  DDeltaT s -> f (s a)

renderTimeline :: forall a. RenderTimeline a => [a] -> [Text]
renderTimeline xs =
  concatMap (uncurry fLine) $ zip xs [(0 :: Int)..]
 where
   fLine :: a -> Int -> [Text]
   fLine l i = (if i `mod` 33 == 0 then catMaybes [head1, head2] else [])
               <> (entry l : rtCommentary l)

   entry :: a -> Text
   entry v = renderLineDist $
     \Field{..} ->
       case fSelect of
         IInt    (($v)->x) -> T.pack $ printf "%*d" fWidth x
         IWord64 (($v)->x) -> T.pack $ printf "%*d" fWidth x
         IFloat  (($v)->x) -> T.pack $ take fWidth $ printf "%*F" (fWidth - 2) x
         IDeltaT (($v)->x) -> T.pack $ take fWidth $ printf "%-*s" fWidth $ dropWhileEnd (== 's') $ show x
         IText   (($v)->x) -> T.take fWidth . T.dropWhileEnd (== 's') $ x

   fields :: [IField a]
   fields = rtFields

   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))

   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
   renderLineDist = T.intercalate " " . renderLine' fLeftPad fWidth

   renderLine' ::
     (IField a -> Int) -> (IField a -> Int) -> (IField a -> Text) -> [Text]
   renderLine' lpfn wfn rfn = renderField lpfn wfn rfn <$> fields
   renderField lpfn wfn rfn f = T.replicate (lpfn f) " " <> T.center (wfn f) ' ' (rfn f)

renderDistributions :: forall a. RenderDistributions a => RenderMode -> a -> [Text]
renderDistributions mode x =
  case mode of
    RenderPretty -> catMaybes [head1, head2] <> pLines <> sizeAvg
    RenderCsv    -> headCsv : pLines
     where headCsv = T.intercalate "," $ fId <$> fields

 where
   pLines :: [Text]
   pLines = fLine <$> [0..(nPercs - 1)]

   fLine :: Int -> Text
   fLine pctIx = (if mode == RenderPretty
                  then renderLineDistPretty
                  else renderLineDistCsv) $
    \Field{..} ->
      let getCapPerc :: forall b c. Distribution b c -> c
          getCapPerc d = dPercIx d pctIx
      in T.pack $ case fSelect of
        DInt    (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") (getCapPerc d)
        DWord64 (($x)->d) -> (if mode == RenderPretty
                              then printf "%*d" fWidth
                              else printf "%d") (getCapPerc d)
        DFloat  (($x)->d) -> (if mode == RenderPretty
                              then take fWidth . printf "%*F" (fWidth - 2)
                              else printf "%F") (getCapPerc d)
        DDeltaT (($x)->d) -> (if mode == RenderPretty
                              then take fWidth else id)
                             . dropWhileEnd (== 's') . show $ getCapPerc d

   head1, head2 :: Maybe Text
   head1 = if all ((== 0) . T.length . fHead1) fields then Nothing
           else Just (renderLineHead1 (uncurry T.take . ((+1) . fWidth &&& fHead1)))
   head2 = if all ((== 0) . T.length . fHead2) fields then Nothing
           else Just (renderLineHead2 (uncurry T.take . ((+1) . fWidth &&& fHead2)))

   sizeAvg :: [Text]
   sizeAvg = fmap (T.intercalate " ")
     [ (T.center (fWidth (head fields)) ' ' "avg" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldDistribution
                  (T.take (fWidth f) .T.pack . printf "%F" . dAverage)  x (fSelect f))
       <$> tail fields
     , (T.center (fWidth (head fields)) ' ' "size" :) $
       (\f -> flip (renderField fLeftPad fWidth) f $ const $
                mapSomeFieldDistribution
                  (T.take (fWidth f) . T.pack . show . dSize)    x (fSelect f))
       <$> tail fields
     ]

   renderLineHead1 = mconcat . renderLine' (const 0) ((+ 1) . fWidth)
   renderLineHead2 = mconcat . renderLine' fLeftPad  ((+ 1) . fWidth)
   renderLineDistPretty = T.intercalate " " . renderLine' fLeftPad fWidth
   renderLineDistCsv    = T.intercalate "," . renderLine' (const 0) (const 0)

   renderLine' ::
     (DField a -> Int) -> (DField a -> Int) -> (DField a -> Text) -> [Text]
   renderLine' lpfn wfn rfn = renderField lpfn wfn rfn <$> fields
   renderField lpfn wfn rfn f = T.replicate (lpfn f) " " <> T.center (wfn f) ' ' (rfn f)

   fields :: [DField a]
   fields = percField : rdFields
   percField :: DField a
   percField = Field 6 0 "%tile" "" "%tile" (DFloat $ const percsDistrib)
   nPercs = length $ dPercentiles percsDistrib
   percsDistrib = mapSomeFieldDistribution
                    distribPercsAsDistrib x (fSelect $ head rdFields)

-- * Auxiliaries
--
distribPercsAsDistrib :: Distribution Float b -> Distribution Float Float
distribPercsAsDistrib Distribution{..} = Distribution 1 0.5 $
  (\p -> p {pctSample = psFrac (pctSpec p)}) <$> dPercentiles

nChunksEachOf :: Int -> Int -> Text -> [Text]
nChunksEachOf chunks each center =
  T.chunksOf each (T.center (each * chunks) ' ' center)
