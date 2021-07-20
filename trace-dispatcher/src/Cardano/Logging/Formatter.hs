{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Cardano.Logging.Formatter (
    humanFormatter
  , metricsFormatter
  , machineFormatter
  , forwardFormatter
  , preFormatted
) where

import qualified Control.Tracer as T
import           Data.Aeson ((.=))
import qualified Data.Aeson as AE
import qualified Data.Aeson.Encoding as AE
import qualified Data.ByteString.Lazy as BS
import           Data.Functor.Contravariant
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, pack, stripPrefix)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder as TB
import           Data.Time (defaultTimeLocale, formatTime, getCurrentTime)

import           Cardano.Logging.Types
import           Control.Concurrent (myThreadId)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Network.HostName


-- | Format this trace as metrics
metricsFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
metricsFormatter application (Trace tr) = do
  let trr = mkTracer
  pure $ Trace (T.arrow trr)
 where
    mkTracer = T.emit $
      \ case
        (lc, Nothing, v) ->
          let metrics = asMetrics v
          in T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                                , Nothing
                                , FormattedMetrics metrics)
        (lc, Just ctrl, v) ->
          let metrics = asMetrics v
          in T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                                , Just ctrl
                                , FormattedMetrics metrics)

-- | Format this trace as TraceObject for the trace forwarder
forwardFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
forwardFormatter application (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ case
        (lc, Nothing, v) -> do
          thid <- liftIO myThreadId
          time <- liftIO getCurrentTime
          let fh = forHuman v
              details = case lcDetails lc of
                          Nothing  -> DRegular
                          Just dtl -> dtl
              fm = forMachine details v
              nlc = lc { lcNamespace = application : lcNamespace lc}
              to = TraceObject {
                      toHuman     = if fh == "" then Nothing else Just fh
                    , toMachine   = if fm == mempty then Nothing else
                                    Just $ decodeUtf8 (BS.toStrict (AE.encode fm))
                    , toNamespace = lcNamespace nlc
                    , toSeverity  = case lcSeverity lc of
                                      Nothing -> Info
                                      Just s  -> s
                    , toDetails   = case lcDetails lc of
                                      Nothing -> DRegular
                                      Just d  -> d
                    , toTimestamp = time
                    , toHostname  = hn
                    , toThreadId  = (pack . show) thid
                  }
          T.traceWith tr ( nlc
                         , Nothing
                         , FormattedForwarder to)
        (lc, Just ctrl, _v) -> do
          thid <- liftIO myThreadId
          time <- liftIO getCurrentTime
          let nlc = lc { lcNamespace = application : lcNamespace lc}
              to  = TraceObject {
                      toHuman     = Nothing
                    , toMachine   = Nothing
                    , toNamespace = lcNamespace nlc
                    , toSeverity  = case lcSeverity lc of
                                      Nothing -> Info
                                      Just s  -> s
                    , toDetails   = case lcDetails lc of
                                      Nothing -> DRegular
                                      Just d  -> d
                    , toTimestamp = time
                    , toHostname  = hn
                    , toThreadId  = (pack . show) thid
                  }
          T.traceWith tr ( nlc
                         , Just ctrl
                         , FormattedForwarder to)

-- | Format this trace for human readability
-- The boolean value tells, if this representation is for the console and should be colored
-- The text argument gives the application name which is prepended to the namespace
humanFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Bool
  -> Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
humanFormatter withColor application (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \ case
        (lc, Nothing, v) -> do
          let fh = forHuman v
          text <- liftIO $ formatContextHuman withColor hn application lc fh
          T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                             , Nothing
                             , FormattedHuman withColor text)
        (lc, Just ctrl, _v) -> do
          T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                             , Just ctrl
                             , FormattedHuman withColor "")

formatContextHuman ::
     Bool
  -> String
  -> Text
  -> LoggingContext
  -> Text
  -> IO Text
formatContextHuman withColor hostname application LoggingContext {..}  txt = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = fromMaybe Info lcSeverity
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ts       = fromString $ formatTime defaultTimeLocale "%F %H:%M:%S%4Q" time
      ns       = colorBySeverity
                    withColor
                    severity
                    $ fromString hostname
                      <> singleton ':'
                      <> mconcat (intersperse (singleton '.')
                          (map fromText (application : lcNamespace)))
      tadd     = fromText " ("
                  <> fromString (show severity)
                  <> singleton ','
                  <> fromText tid
                  <> fromText ") "
  pure $ toStrict
          $ toLazyText
            $ squareBrackets ts
              <> singleton ' '
              <> squareBrackets ns
              <> tadd
              <> fromText txt
  where
    squareBrackets :: Builder -> Builder
    squareBrackets b = singleton '[' <> b <> singleton ']'

-- | Format this trace for machine readability
-- The detail level give a hint to the formatter
-- The text argument gives the application name which is prepended to the namespace
machineFormatter
  :: forall a m . (LogFormatting a, MonadIO m)
  => Text
  -> Trace m FormattedMessage
  -> m (Trace m a)
machineFormatter application (Trace tr) = do
  hn <- liftIO getHostName
  let trr = mkTracer hn
  pure $ Trace (T.arrow trr)
 where
    mkTracer hn = T.emit $
      \case
        (lc, Nothing, v) -> do
          let detailLevel = case lcDetails lc of
                              Nothing -> DRegular
                              Just dl -> dl
          obj <- liftIO $ formatContextMachine hn application lc (forMachine detailLevel v)
          T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                             , Nothing
                             , FormattedMachine (decodeUtf8 (BS.toStrict
                                    (AE.encodingToLazyByteString obj))))
        (lc, Just c, _v) -> do
          T.traceWith tr (lc { lcNamespace = application : lcNamespace lc}
                             , Just c
                             , FormattedMachine "")

formatContextMachine ::
     String
  -> Text
  -> LoggingContext
  -> AE.Object
  -> IO AE.Encoding
formatContextMachine hostname application LoggingContext {..} obj = do
  thid <- myThreadId
  time <- getCurrentTime
  let severity = (pack . show) (fromMaybe Info lcSeverity)
      tid      = fromMaybe ((pack . show) thid)
                    ((stripPrefix "ThreadId " . pack . show) thid)
      ns       = application : lcNamespace
      ts       = pack $ formatTime defaultTimeLocale "%F %H:%M:%S%4Q" time
  pure $ AE.pairs $    "at"      .= ts
                    <> "ns"      .= ns
                    <> "message" .= obj
                    <> "sev"     .= severity
                    <> "thread"  .= tid
                    <> "host"    .= hostname


-- | Transform this trace to a preformatted message, so that double serialization
-- is avoided
preFormatted ::
  (  LogFormatting a
  ,  Monad m)
  => [BackendConfig]
  -> Trace m (PreFormatted a)
  -> Trace m a
preFormatted backends tr@(Trace tr')=
  if elem Forwarder backends
    then if elem (Stdout HumanFormatUncoloured) backends
            || elem (Stdout HumanFormatColoured) backends
      then contramap (\msg -> PreFormatted msg (Just (forHuman msg)) Nothing) tr
      else if elem (Stdout MachineFormat) backends
        then Trace $ T.contramap
              (\ (lc, mbC, msg) ->
                let dtal = case lcDetails lc of
                            Nothing  -> DRegular
                            Just dtl -> dtl
                in (lc, mbC, PreFormatted msg Nothing (Just (forMachine dtal msg))))
              tr'
        else contramap (\msg -> PreFormatted msg Nothing Nothing) tr
    else contramap (\msg -> PreFormatted msg Nothing Nothing) tr

-- | Color a text message based on `Severity`. `Error` and more severe errors
-- are colored red, `Warning` is colored yellow, and all other messages are
-- rendered in the default color.
colorBySeverity :: Bool -> SeverityS -> Builder -> Builder
colorBySeverity withColor severity msg = case severity of
  Emergency -> red msg
  Alert     -> red msg
  Critical  -> red msg
  Error     -> red msg
  Warning   -> yellow msg
  _         -> msg
  where
    red = colorize "31"
    yellow = colorize "33"
    colorize c s
      | withColor = "\ESC["<> c <> "m" <> s <> "\ESC[0m"
      | otherwise = s
