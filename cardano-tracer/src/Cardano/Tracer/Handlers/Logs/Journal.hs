{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

#if defined(linux_HOST_OS)
#define LINUX
#endif

module Cardano.Tracer.Handlers.Logs.Journal
  ( writeTraceObjectsToJournal
  ) where

#if defined(LINUX)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Systemd.Journal (Priority (..), message, mkJournalField,
                                  priority, sendJournalFields, syslogIdentifier)

import           Cardano.Logging (TraceObject (..))
import qualified Cardano.Logging as L

import           Cardano.Tracer.Types
#else
import           System.IO (hPutStrLn, stderr)

import           Cardano.Logging

import           Cardano.Tracer.Types
#endif

#if defined(LINUX)
writeTraceObjectsToJournal
  :: NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ [] = return ()
writeTraceObjectsToJournal nodeId nodeName traceObjects =
  mapM_ (sendJournalFields . mkJournalFields) traceObjects
 where
  mkJournalFields trOb =
    let msgForHuman   = fromMaybe "" $ L.toHuman trOb
        msgForMachine = fromMaybe "" $ L.toMachine trOb
    in if | not . T.null $ msgForHuman   -> mkJournalFields' trOb msgForHuman
          | not . T.null $ msgForMachine -> mkJournalFields' trOb msgForMachine
          | otherwise                    -> HM.empty -- Both messages are empty!

  mkJournalFields' trOb msg =
       syslogIdentifier (nodeName <> T.pack (show nodeId))
    <> message msg
    <> priority (mkPriority $ L.toSeverity trOb)
    <> HM.fromList [ (namespace, encodeUtf8 (mkName $ L.toNamespace trOb))
                   , (thread,    encodeUtf8 $ L.toThreadId trOb)
                   , (time,      encodeUtf8 . formatAsIso8601 $ L.toTimestamp trOb)
                   ]

  mkName [] = "noname"
  mkName names = T.intercalate "." names

  namespace = mkJournalField "namespace"
  thread    = mkJournalField "thread"
  time      = mkJournalField "time"

  formatAsIso8601 = T.pack . formatTime defaultTimeLocale "%F %T%12QZ"

mkPriority :: L.SeverityS -> Priority
mkPriority L.Debug     = Debug
mkPriority L.Info      = Info
mkPriority L.Notice    = Notice
mkPriority L.Warning   = Warning
mkPriority L.Error     = Error
mkPriority L.Critical  = Critical
mkPriority L.Alert     = Alert
mkPriority L.Emergency = Emergency
#else
writeTraceObjectsToJournal
  :: NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjectsToJournal _ _ _ =
  hPutStrLn stderr "Writing to systemd's journal is available on Linux only."
#endif
