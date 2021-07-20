{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-fields -Wno-orphans #-}

module Cardano.Unlog.LogObject (module Cardano.Unlog.LogObject) where

import           Prelude (String, error, id)
import qualified Prelude
import           Cardano.Prelude hiding (Text)

import           Control.Monad (fail)
import           Data.Aeson (FromJSON(..), ToJSON(..), Value(..), Object, (.:))
import           Data.Aeson.Types (Parser)
import qualified Data.Aeson as AE
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as LText
import qualified Data.Text.Short as Text
import           Data.Text.Short (ShortText, fromText, toText)
import           Data.Time.Clock (NominalDiffTime, UTCTime)
import qualified Data.Map as Map
import           Data.Vector (Vector)

import           Ouroboros.Network.Block (BlockNo(..), SlotNo(..))

import           Cardano.BM.Stats.Resources

import           Data.Accum (zeroUTCTime)


type Text = ShortText

readLogObjectStream :: JsonLogfile -> IO [LogObject]
readLogObjectStream (JsonLogfile f) =
  LBS.readFile f
    <&>
    fmap (either (LogObject zeroUTCTime "DecodeError" "" (TId "0") . LODecodeError)
                 id
          . AE.eitherDecode)
    . LBS.split (fromIntegral $ fromEnum '\n')

newtype JsonRunMetafile
  = JsonRunMetafile { unJsonRunMetafile :: FilePath }
  deriving (Show, Eq)

newtype JsonGenesisFile
  = JsonGenesisFile { unJsonGenesisFile :: FilePath }
  deriving (Show, Eq)

newtype JsonLogfile
  = JsonLogfile { unJsonLogfile :: FilePath }
  deriving (Show, Eq)

newtype JsonOutputFile
  = JsonOutputFile { unJsonOutputFile :: FilePath }
  deriving (Show, Eq)

newtype TextOutputFile
  = TextOutputFile { unTextOutputFile :: FilePath }
  deriving (Show, Eq)

newtype CsvOutputFile
  = CsvOutputFile { unCsvOutputFile :: FilePath }
  deriving (Show, Eq)

newtype OutputFile
  = OutputFile { unOutputFile :: FilePath }
  deriving (Show, Eq)

data LogObject
  = LogObject
    { loAt   :: !UTCTime
    , loKind :: !Text
    , loHost :: !Host
    , loTid  :: !TId
    , loBody :: !LOBody
    }
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LogObject

instance ToJSON ShortText where
  toJSON = String . toText

instance FromJSON ShortText where
  parseJSON = AE.withText "String" $ pure . fromText

instance Print ShortText where
  hPutStr   h = hPutStr   h . toText
  hPutStrLn h = hPutStrLn h . toText

newtype TId = TId { unTId :: ShortText }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData

newtype Hash = Hash { unHash :: ShortText }
  deriving (Eq, Generic, Ord)
  deriving newtype (FromJSON, ToJSON)
  deriving anyclass NFData

shortHash :: Hash -> LText.Text
shortHash = toText . Text.take 6 . unHash

instance Show Hash where show = LText.unpack . toText . unHash

instance AE.ToJSONKey Hash where
  toJSONKey = AE.toJSONKeyText (toText . unHash)

newtype Host = Host { unHost :: ShortText }
  deriving (Eq, Generic, Ord, Show)
  deriving newtype (IsString, FromJSON, ToJSON)
  deriving anyclass NFData

instance FromJSON BlockNo where
  parseJSON o = BlockNo <$> parseJSON o
instance ToJSON BlockNo where
  toJSON (BlockNo x) = toJSON x

deriving anyclass instance NFData BlockNo
deriving instance NFData a => NFData (Resources a)

--
-- LogObject stream interpretation
--

interpreters :: Map Text (Object -> Parser LOBody)
interpreters = Map.fromList
  [ (,) "TraceStartLeadershipCheck" $
    \v -> LOTraceStartLeadershipCheck
            <$> v .: "slot"
            <*> v .: "utxoSize"
            <*> v .: "chainDensity"

  , (,) "TraceBlockContext" $
    \v -> LOBlockContext
            <$> v .: "tipBlockNo"

  , (,) "TraceNodeIsLeader" $
    \v -> LOTraceNodeIsLeader
            <$> v .: "slot"

  , (,) "TraceNodeNotLeader" $
    \v -> LOTraceNodeNotLeader
            <$> v .: "slot"

  , (,) "TraceMempoolAddedTx" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRemoveTxs" $
    \v -> do
      x :: Object <- v .: "mempoolSize"
      LOMempoolTxs <$> x .: "numTxs"

  , (,) "TraceMempoolRejectedTx" $
    \_ -> pure LOMempoolRejectedTx

  , (,) "TraceLedgerEvent.TookSnapshot" $
    \_ -> pure LOLedgerTookSnapshot

  , (,) "TraceBenchTxSubSummary" $
    \v -> do
       x :: Object <- v .: "summary"
       LOGeneratorSummary
         <$> ((x .: "ssFailures" :: Parser [Text])
              <&> null)
         <*> x .: "ssTxSent"
         <*> x .: "ssElapsed"
         <*> x .: "ssThreadwiseTps"

  , (,) "TraceBenchTxSubServAck" $
    \v -> LOTxsAcked <$> v .: "txIds"

  , (,) "Resources" $
    \v -> LOResources <$> parsePartialResourceStates (Object v)

  , (,) "TraceTxSubmissionCollected" $
    \v -> LOTxsCollected
            <$> v .: "count"

  , (,) "TraceTxSubmissionProcessed" $
    \v -> LOTxsProcessed
            <$> v .: "accepted"
            <*> v .: "rejected"

  , (,) "TraceForgedBlock" $
    \v -> LOBlockForged
            <$> v .: "block"
            <*> v .: "blockPrev"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "TraceAddBlockEvent.AddedToCurrentChain" $
    \v -> LOBlockAddedToCurrentChain
            <$> ((v .: "newtip")     <&> hashFromPoint)
            <*> v .: "chainLengthDelta"
  -- TODO: we should clarify the distinction between the two cases (^ and v).
  , (,) "TraceAdoptedBlock" $
    \v -> LOBlockAddedToCurrentChain
            <$> v .: "blockHash"
            <*> pure 1
  , (,) "ChainSyncServerEvent.TraceChainSyncServerRead.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "ChainSyncServerEvent.TraceChainSyncServerReadBlocked.AddBlock" $
    \v -> LOChainSyncServerSendHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  -- v, but not ^ -- how is that possible?
  , (,) "TraceBlockFetchServerSendBlock" $
    \v -> LOBlockFetchServerSending
            <$> v .: "block"
  , (,) "SendFetchRequest" $
    \v -> LOBlockFetchClientRequested
            <$> v .: "head"
            <*> v .: "length"
  , (,) "ChainSyncClientEvent.TraceDownloadedHeader" $
    \v -> LOChainSyncClientSeenHeader
            <$> v .: "block"
            <*> v .: "blockNo"
            <*> v .: "slot"
  , (,) "CompletedBlockFetch" $
    \v -> LOBlockFetchClientCompletedFetch
            <$> v .: "block"
  ]
 where
   hashFromPoint :: LText.Text -> Hash
   hashFromPoint = Hash . fromText . Prelude.head . LText.splitOn "@"

logObjectStreamInterpreterKeys :: [Text]
logObjectStreamInterpreterKeys = Map.keys interpreters

data LOBody
  = LOTraceStartLeadershipCheck !SlotNo !Word64 !Float
  | LOTraceNodeIsLeader !SlotNo
  | LOTraceNodeNotLeader !SlotNo
  | LOResources !ResourceStats
  | LOMempoolTxs !Word64
  | LOMempoolRejectedTx
  | LOLedgerTookSnapshot
  | LOBlockContext !Word64
  | LOGeneratorSummary !Bool !Word64 !NominalDiffTime (Vector Float)
  | LOTxsAcked !(Vector Text)
  | LOTxsCollected !Word64
  | LOTxsProcessed !Word64 !Int
  | LOBlockForged
    { loBlock            :: !Hash
    , loPrev             :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockAddedToCurrentChain
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOChainSyncServerSendHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockFetchServerSending
    { loBlock            :: !Hash
    }
  | LOBlockFetchClientRequested
    { loBlock            :: !Hash
    , loLength           :: !Int
    }
  | LOChainSyncClientSeenHeader
    { loBlock            :: !Hash
    , loBlockNo          :: !BlockNo
    , loSlotNo           :: !SlotNo
    }
  | LOBlockFetchClientCompletedFetch
    { loBlock            :: !Hash
    }
  | LOAny !Object
  | LODecodeError !String
  deriving (Generic, Show)
  deriving anyclass NFData

instance ToJSON LOBody

instance FromJSON LogObject where
  parseJSON = AE.withObject "LogObject" $ \v -> do
    body :: Object <- v .: "data"
    -- XXX:  fix node causing the need for this workaround
    (,) unwrapped kind <- unwrap "credentials" "val" body
    LogObject
      <$> v .: "at"
      <*> pure kind
      <*> v .: "host"
      <*> v .: "thread"
      <*> case Map.lookup kind interpreters of
            Just interp -> interp unwrapped
            Nothing -> pure $ LOAny unwrapped
   where
     unwrap :: Text -> Text -> Object -> Parser (Object, Text)
     unwrap wrappedKeyPred unwrapKey v = do
       kind <- (fromText <$>) <$> v AE..:? "kind"
       wrapped   :: Maybe Text <-
         (fromText <$>) <$> v AE..:? toText wrappedKeyPred
       unwrapped :: Maybe Object <- v AE..:? toText unwrapKey
       case (kind, wrapped, unwrapped) of
         (Nothing, Just _, Just x) -> (,) <$> pure x <*> (fromText <$> x .: "kind")
         (Just kind0, _, _) -> pure (v, kind0)
         _ -> fail $ "Unexpected LogObject .data: " <> show v

extendObject :: Text -> Value -> Value -> Value
extendObject k v (Object hm) = Object $ hm <> HM.singleton (toText k) v
extendObject k _ _ = error . Text.unpack $ "Summary key '" <> k <> "' does not serialise to an Object."

parsePartialResourceStates :: Value -> Parser (Resources Word64)
parsePartialResourceStates =
  AE.withObject "NodeSetup" $
    \o ->
      Resources
      <$> o .: "CentiCpu"
      <*> o .: "CentiGC"
      <*> o .: "CentiMut"
      <*> o .: "GcsMajor"
      <*> o .: "GcsMinor"
      <*> o .: "Alloc"
      <*> o .: "Live"
      <*> (o AE..:? "Heap" & fmap (fromMaybe 0))
      <*> o .: "RSS"
      <*> o .: "CentiBlkIO"
      <*> o .: "Threads"
