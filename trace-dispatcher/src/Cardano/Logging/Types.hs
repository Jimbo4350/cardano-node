{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Cardano.Logging.Types (
    Trace(..)
  , LogFormatting(..)
  , Metric(..)
  , mkObject
  , emptyObject
  , Documented(..)
  , DocMsg(..)
  , LoggingContext(..)
  , emptyLoggingContext
  , Namespace
  , DetailLevel(..)
  , Privacy(..)
  , SeverityS(..)
  , SeverityF(..)
  , ConfigOption(..)
  , RemoteAddr(..)
  , FormatLogging(..)
  , TraceConfig(..)
  , emptyTraceConfig
  , FormattedMessage(..)
  , TraceControl(..)
  , DocCollector(..)
  , LogDoc(..)
  , emptyLogDoc
  , BackendConfig(..)
  , Folding(..)
  , unfold
  , TraceObject(..)
  , PreFormatted(..)
) where

import           Control.Tracer
import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Text as AE
import qualified Data.HashMap.Strict as HM
import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text, pack)
import           Data.Text.Lazy (toStrict)
import           Data.Time (UTCTime)
import           GHC.Generics
import           Network.HostName (HostName)

-- | The Trace carries the underlying tracer Tracer from the contra-tracer package.
--   It adds a 'LoggingContext' and maybe a 'TraceControl' to every message.
-- control object
newtype Trace m a = Trace
  {unpackTrace :: Tracer m (LoggingContext, Maybe TraceControl, a)}

-- | Contramap lifted to Trace
instance Monad m => Contravariant (Trace m) where
    contramap f (Trace tr) = Trace $
      T.contramap (\ (lc, mbC, a) -> (lc, mbC, f a)) tr

-- | @tr1 <> tr2@ will run @tr1@ and then @tr2@ with the same input.
instance Monad m => Semigroup (Trace m a) where
  Trace a1 <> Trace a2 = Trace (a1 <> a2)

instance Monad m => Monoid (Trace m a) where
    mappend = (<>)
    mempty  = Trace T.nullTracer

-- | Every message needs this to define how to represent itself
class LogFormatting a where
  -- | Machine readable representation with the possibility to represent
  -- with different details based on the detail level.
  -- No machine readable representation as default
  forMachine :: DetailLevel -> a -> AE.Object

  -- | Human readable representation.
  -- No human representation is represented by the empty text
  -- The default implementation returns no human representation
  forHuman :: a -> Text
  forHuman v = toStrict (AE.encodeToLazyText (forMachine DRegular v))

  -- | Metrics representation.
  -- No metrics by default
  asMetrics :: a -> [Metric]
  asMetrics _v = []

data Metric
  -- | An integer metric.
  -- If the text array is not empty it is used as namespace namespace
    = IntM Namespace Integer
  -- | A double metric.
  -- If the text array is not empty it is used as namespace
    | DoubleM Namespace Double
  -- | An counter metric.
  -- If the text array is not empty it is used as namespace namespace
    | CounterM Namespace (Maybe Int)
  deriving (Show, Eq)

-- | A helper function for creating an |Object| given a list of pairs, named items,
-- or the empty |Object|.
mkObject :: [(Text, a)] -> HM.HashMap Text a
mkObject = HM.fromList

-- | A helper function for creating an empty |Object|.
emptyObject :: HM.HashMap Text a
emptyObject = HM.empty

-- Document all log messages by providing a list of DocMsgs for all constructors.
-- Because it is not enforced by the type system, it is very
-- important to provide a complete list, as the prototypes are used as well for configuration.
-- If you don't want to add an item for documentation enter an empty text.
newtype Documented a = Documented {undoc :: [DocMsg a]}
  deriving Show

-- | A unique identifier for every message, composed of text
type Namespace = [Text]

-- | Document a message by giving a prototype, its most special name in the namespace
-- and a comment in markdown format
data DocMsg a = DocMsg {
    dmPrototype :: a
  , dmMetricsMD :: [(Namespace, Text)]
  , dmMarkdown  :: Text
} deriving (Show)

-- | Context any log message carries
data LoggingContext = LoggingContext {
    lcNamespace :: Namespace
  , lcSeverity  :: Maybe SeverityS
  , lcPrivacy   :: Maybe Privacy
  , lcDetails   :: Maybe DetailLevel
  } deriving (Eq, Show)

emptyLoggingContext :: LoggingContext
emptyLoggingContext = LoggingContext [] Nothing Nothing Nothing

-- | Formerly known as verbosity
data DetailLevel =
      DBrief
    | DRegular
    | DDetailed
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance AE.ToJSON DetailLevel where
    toEncoding = AE.genericToEncoding AE.defaultOptions
instance AE.FromJSON DetailLevel

-- | Privacy of a message. Default is Public
data Privacy =
      Confidential              -- ^ confidential information - handle with care
    | Public                    -- ^ can be public.
      deriving (Show, Eq, Ord, Bounded, Enum)

-- | Severity of a message
data SeverityS
    = Debug                   -- ^ Debug messages
    | Info                    -- ^ Information
    | Notice                  -- ^ Normal runtime Conditions
    | Warning                 -- ^ General Warnings
    | Error                   -- ^ General Errors
    | Critical                -- ^ Severe situations
    | Alert                   -- ^ Take immediate action
    | Emergency               -- ^ System is unusable
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Severity for a filter
data SeverityF
    = DebugF                   -- ^ Debug messages
    | InfoF                    -- ^ Information
    | NoticeF                  -- ^ Normal runtime Conditions
    | WarningF                 -- ^ General Warnings
    | ErrorF                   -- ^ General Errors
    | CriticalF                -- ^ Severe situations
    | AlertF                   -- ^ Take immediate action
    | EmergencyF               -- ^ System is unusable
    | SilenceF                 -- ^ Don't show anything
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance AE.ToJSON SeverityF where
    toEncoding = AE.genericToEncoding AE.defaultOptions
instance AE.FromJSON SeverityF

-- | Used as interface object for ForwarderTracer
data TraceObject = TraceObject {
    toHuman     :: Maybe Text
  , toMachine   :: Maybe Text
  , toNamespace :: Namespace
  , toSeverity  :: SeverityS
  , toDetails   :: DetailLevel
  , toTimestamp :: UTCTime
  , toHostname  :: HostName
  , toThreadId  :: Text
} deriving (Eq, Show)

----------------------------------------------------------------
-- Configuration

-- |
data FormattedMessage =
      FormattedHuman Bool Text
      -- ^ The bool specifies if the formatting includes colours
    | FormattedMachine Text
    | FormattedMetrics [Metric]
    | FormattedForwarder TraceObject
  deriving (Eq, Show)

-- |
data BackendConfig =
    Forwarder
  | Stdout FormatLogging
  | EKGBackend
  deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON BackendConfig where
  toJSON Forwarder  = AE.String "Forwarder"
  toJSON EKGBackend = AE.String "EKGBackend"
  toJSON (Stdout f) = AE.String $ "Stdout " <> (pack . show) f

instance AE.FromJSON BackendConfig where
  parseJSON (AE.String "Forwarder")            = pure Forwarder
  parseJSON (AE.String "EKGBackend")           = pure EKGBackend
  parseJSON (AE.String "Stdout HumanFormatColoured")
                                               = pure $ Stdout HumanFormatColoured
  parseJSON (AE.String "Stdout HumanFormatUncoloured")
                                               = pure $ Stdout HumanFormatUncoloured
  parseJSON (AE.String "Stdout MachineFormat") = pure $ Stdout MachineFormat
  parseJSON other                              = error (show other)

data FormatLogging =
    HumanFormatColoured
  | HumanFormatUncoloured
  | MachineFormat
  deriving (Eq, Ord, Show)

-- Configuration options for individual namespace elements
data ConfigOption =
    -- | Severity level for a filter (default is WarningF)
    CoSeverity SeverityF
    -- | Detail level (default is DRegular)
  | CoDetail DetailLevel
  -- | To which backend to pass
  --   Default is [EKGBackend, Forwarder, Stdout HumanFormatColoured]
  | CoBackend [BackendConfig]
  -- | Construct a limiter with name (Text) and limiting to the Double,
  -- which represents frequency in number of messages per second
  | CoLimiter Text Double
  deriving (Eq, Ord, Show)

newtype RemoteAddr
  = LocalSocket FilePath
  deriving (Eq, Ord, Show)

instance AE.FromJSON RemoteAddr where
  parseJSON = AE.withObject "RemoteAddr" $ \o -> LocalSocket <$> o AE..: "filePath"

data TraceConfig = TraceConfig {
     -- | Options specific to a certain namespace
    tcOptions            :: Map.Map Namespace [ConfigOption]
  , tcForwarder          :: RemoteAddr
  , tcForwarderQueueSize :: Int
}
  deriving (Eq, Ord, Show)

emptyTraceConfig :: TraceConfig
emptyTraceConfig = TraceConfig {
    tcOptions = Map.empty
  , tcForwarder = LocalSocket "forwarder.log"
  , tcForwarderQueueSize = 1500
  }

---------------------------------------------------------------------------
-- Control and Documentation

-- | When configuring a net of tracers, it should be run with Config on all
-- entry points first, and then with Optimize. When reconfiguring it needs to
-- run Reset followed by Config followed by Optimize
data TraceControl where
    Reset     :: TraceControl
    Config    :: TraceConfig -> TraceControl
    Optimize  :: TraceControl
    Document  :: Int -> Text -> [(Namespace, Text)] -> DocCollector -> TraceControl

newtype DocCollector = DocCollector (IORef (Map Int LogDoc))

data LogDoc = LogDoc {
    ldDoc        :: Text
  , ldMetricsDoc :: Map Namespace Text
  , ldNamespace  :: [Namespace]
  , ldSeverity   :: [SeverityS]
  , ldPrivacy    :: [Privacy]
  , ldDetails    :: [DetailLevel]
  , ldBackends   :: [(BackendConfig, FormattedMessage)]
  , ldFiltered   :: [SeverityF]
  , ldLimiter    :: [(Text, Double)]
} deriving(Eq, Show)

emptyLogDoc :: Text -> [(Namespace, Text)] -> LogDoc
emptyLogDoc d m = LogDoc d (Map.fromList m) [] [] [] [] [] [] []

-- | Type for a Fold
newtype Folding a b = Folding b

unfold :: Folding a b -> b
unfold (Folding b) = b

data PreFormatted a = PreFormatted {
    pfMessage    :: a
  , pfForHuman   :: Maybe Text
  , pfForMachine :: Maybe AE.Object
  }

instance LogFormatting a => LogFormatting (PreFormatted a) where
  forMachine dtal PreFormatted {..} =  case pfForMachine of
                                          Nothing -> forMachine dtal pfMessage
                                          Just obj -> obj
  forHuman PreFormatted {..}        =  case pfForHuman of
                                          Nothing  -> forHuman pfMessage
                                          Just txt -> txt
  asMetrics PreFormatted {..}       =  asMetrics pfMessage

---------------------------------------------------------------------------
-- LogFormatting instances

instance LogFormatting b => LogFormatting (Folding a b) where
  forMachine v (Folding b) =  forMachine v b
  forHuman (Folding b)     =  forHuman b
  asMetrics (Folding b)    =  asMetrics b

instance LogFormatting Double where
  forMachine _dtal d = mkObject [ "val" .= AE.String ((pack . show) d)]
  forHuman d         = (pack . show) d
  asMetrics d        = [DoubleM [] d]

instance LogFormatting Int where
  forMachine _dtal i = mkObject [ "val" .= AE.String ((pack . show) i)]
  forHuman i         = (pack . show) i
  asMetrics i        = [IntM [] (fromIntegral i)]

instance LogFormatting Integer where
  forMachine _dtal i = mkObject [ "val" .= AE.String ((pack . show) i)]
  forHuman i         = (pack . show) i
  asMetrics i        = [IntM [] i]
