{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Configuration
  ( Host
  , Port
  , Address (..)
  , Endpoint (..)
  , RotationParams (..)
  , LogMode (..)
  , LogFormat (..)
  , LoggingParams (..)
  , ConnectMode (..)
  , TracerConfig (..)
  , readTracerConfig
  ) where

import           Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict')
import           Data.Fixed (Pico)
import           Data.Word (Word16, Word64)
import           GHC.Generics (Generic)
import qualified System.Exit as Ex

type Host = String
type Port = Int

newtype Address = LocalSocket FilePath
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data Endpoint = Endpoint !Host !Port
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data RotationParams = RotationParams
  { rpLogLimitBytes :: !Word64  -- ^ Max size of file in bytes
  , rpMaxAgeHours   :: !Word    -- ^ Hours
  , rpKeepFilesNum  :: !Word    -- ^ Number of files to keep
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LogMode
  = FileMode
  | JournalMode
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LogFormat
  = ForHuman
  | ForMachine
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data LoggingParams = LoggingParams
  { logRoot   :: !FilePath
  , logMode   :: !LogMode
  , logFormat :: !LogFormat
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | 'cardano-tracer' can be both an initiator and a responder, from
--   networking point of view:
--   1. In 'Initiator' mode it tries to establish the connection with the node.
--   2. In 'Responder' mode it accepts the conection from the node.
data ConnectMode
  = Initiator
  | Responder
  deriving (Eq, Generic, FromJSON, Show, ToJSON)

data TracerConfig = TracerConfig
  { connectMode    :: !ConnectMode
  , acceptAt       :: ![Address]
  , loRequestNum   :: !Word16 -- ^ How many 'TraceObject's in one request.
  , ekgRequestFreq :: !Pico   -- ^ How often to request EKG-metrics.
  , hasEKG         :: !(Maybe Endpoint)
  , hasPrometheus  :: !(Maybe Endpoint)
  , logging        :: ![LoggingParams]
  , rotation       :: !(Maybe RotationParams)
  } deriving (Eq, Generic, FromJSON, Show, ToJSON)

-- | Reads the tracer's configuration file (path is passed via '--config' CLI option).
readTracerConfig :: FilePath -> IO TracerConfig
readTracerConfig pathToConfig =
  eitherDecodeFileStrict' pathToConfig >>= \case
    Left e -> Ex.die $ "Invalid tracer's configuration: " <> show e
    Right (config :: TracerConfig) -> return config
