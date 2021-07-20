{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Cardano.Tracer.CLI
  ( TracerParams (..)
  , parseTracerParams
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           GHC.Generics (Generic)
import           Options.Applicative (Parser, bashCompleter, completer,
                                      help, long, metavar, strOption)

-- | Type for CLI parameters required for the service.
newtype TracerParams = TracerParams
  { tracerConfig :: FilePath
  } deriving (Generic, FromJSON, ToJSON)

parseTracerParams :: Parser TracerParams
parseTracerParams =
  TracerParams
    <$> parseFilePath
          "config"
          "file"
          "Configuration file for cardano-tracer service"

-- Aux parsers

parseFilePath
  :: String
  -> String
  -> String
  -> Parser FilePath
parseFilePath optname completion desc = strOption flags
 where
  flags =
       long optname
    <> metavar "FILEPATH"
    <> help desc
    <> completer (bashCompleter completion)
