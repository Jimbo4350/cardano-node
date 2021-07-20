{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.DocuGenerator (
    documentMarkdown
  , buildersToText
  , docIt
  , addFiltered
  , addLimiter
) where

import           Cardano.Logging.Types
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.Aeson.Text (encodeToLazyText)
import           Data.IORef (modifyIORef, newIORef, readIORef)
import           Data.List (intersperse, nub, sortBy)
import qualified Data.Map as Map
import           Data.Text (Text)
import           Data.Text.Internal.Builder (toLazyText)
import           Data.Text.Lazy (toStrict)
import           Data.Text.Lazy.Builder (Builder, fromString, fromText,
                     singleton)
import           Data.Time (getZonedTime)


documentTracers :: MonadIO m => Documented a -> [Trace m a] -> m DocCollector
documentTracers (Documented documented) tracers = do
    let docIdx = zip documented [0..]
    coll <- fmap DocCollector (liftIO $ newIORef Map.empty)
    mapM_ (docTrace docIdx coll) tracers
    pure coll
  where
    docTrace docIdx docColl (Trace tr) =
      mapM_
        (\ (DocMsg {..}, idx) -> do
          T.traceWith tr (emptyLoggingContext,
                          Just (Document idx dmMarkdown dmMetricsMD docColl),
                                dmPrototype))
        docIdx

addFiltered :: MonadIO m => TraceControl -> Maybe SeverityF -> m ()
addFiltered (Document idx mdText mdMetrics (DocCollector docRef)) (Just sev) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldFiltered = sev : ldFiltered e})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> emptyLogDoc mdText mdMetrics))
        docMap)
addFiltered _ _ = pure ()

addLimiter :: MonadIO m => TraceControl -> (Text, Double) -> m ()
addLimiter (Document idx mdText mdMetrics (DocCollector docRef)) (ln, lf) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldLimiter = (ln, lf) : ldLimiter e})
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> emptyLogDoc mdText mdMetrics))
        docMap)
addLimiter _ _ = pure ()

docIt :: MonadIO m =>
     BackendConfig
  -> FormattedMessage
  -> (LoggingContext, Maybe TraceControl, a)
  -> m ()
docIt backend formattedMessage (LoggingContext {..},
  Just (Document idx mdText mdMetrics (DocCollector docRef)), _msg) = do
  liftIO $ modifyIORef docRef (\ docMap ->
      Map.insert
        idx
        ((\e -> e { ldBackends  = (backend, formattedMessage) : ldBackends e
                  , ldNamespace = lcNamespace : ldNamespace e
                  , ldSeverity  = case lcSeverity of
                                    Nothing -> ldSeverity e
                                    Just s  -> s : ldSeverity e
                  , ldPrivacy   = case lcPrivacy of
                                    Nothing -> ldPrivacy e
                                    Just s  -> s : ldPrivacy e
                  , ldDetails   = case lcDetails of
                                    Nothing -> ldDetails e
                                    Just s  -> s : ldDetails e
                  })
          (case Map.lookup idx docMap of
                        Just e  -> e
                        Nothing -> emptyLogDoc mdText mdMetrics))
        docMap)

buildersToText :: [(Namespace, Builder)] -> TraceConfig -> IO Text
buildersToText builderList configuration = do
  time <- getZonedTime
--  tz   <- getTimeZone
  let sortedBuilders = sortBy (\ (l,_) (r,_) -> compare l r) builderList
      num = length builderList
      content = mconcat $ intersperse (fromText "\n\n") (map snd sortedBuilders)
      config  = fromString $ "\n\nConfiguration: " <> show configuration
      numbers = fromString $ "\n\n" <> show num <> " log messages."
      ts      = fromString $ "\nGenerated at " <> show time <> "."
  pure $ toStrict $ toLazyText (content <> config <> numbers <> ts)

documentMarkdown :: MonadIO m =>
     Documented a
  -> [Trace m a]
  -> m [(Namespace, Builder)]
documentMarkdown (Documented documented) tracers = do
    DocCollector docRef <- documentTracers (Documented documented) tracers
    items <- fmap Map.toList (liftIO (readIORef docRef))
    let sortedItems = sortBy
                        (\ (_,l) (_,r) -> compare (ldNamespace l) (ldNamespace r))
                        items
    pure $ map (\(i, ld) -> case ldNamespace ld of
                                []     -> (["No Namespace"], documentItem (i, ld))
                                (hn:_) -> (hn, documentItem (i, ld)))
               sortedItems
  where
    documentItem :: (Int, LogDoc) -> Builder
    documentItem (_idx, ld@LogDoc {..}) = mconcat $ intersperse (fromText "\n\n")
      [ namespacesBuilder (nub ldNamespace)
      , betweenLines (fromText ldDoc)
--      , representationBuilder (documented `listIndex` idx)
      , propertiesBuilder ld
      , configBuilder ld
      , metricsBuilder ldMetricsDoc (filter fMetrics (nub ldBackends))
      ]

    namespacesBuilder :: [Namespace] -> Builder
    namespacesBuilder [ns] = namespaceBuilder ns
    namespacesBuilder []   = fromText "__Warning__: Namespace missing"
    namespacesBuilder nsl  =
      mconcat (intersperse (singleton '\n')(map namespaceBuilder nsl))

    namespaceBuilder :: Namespace -> Builder
    namespaceBuilder ns = fromText "### " <>
      mconcat (intersperse (singleton '.') (map fromText ns))

    _representationBuilder :: LogFormatting a => Maybe (DocMsg a) -> Builder
    _representationBuilder Nothing = mempty
    _representationBuilder (Just DocMsg {..}) = mconcat
      $ intersperse (singleton '\n')
        [case forHuman dmPrototype of
          "" -> mempty
          t  -> fromText "For human:\n" <> asCode (fromText t)
        , let r1 = forMachine DBrief dmPrototype
              r2 = forMachine DRegular dmPrototype
              r3 = forMachine DDetailed dmPrototype
          in if r1 == mempty && r2 == mempty && r3 == mempty
            then mempty
              else if r1 == r2 && r2 == r3
                then fromText "For machine:\n"
                      <> asCode (fromText (toStrict (encodeToLazyText r1)))
                else if r1 == r2
                  then fromText "For machine regular:\n"
                        <> asCode (fromText (toStrict (encodeToLazyText r2)))
                        <> fromText "\nFor machine detailed: "
                        <> asCode (fromText (toStrict (encodeToLazyText r3)))
                  else if r2 == r3
                    then fromText "For machine brief:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r1)))
                          <> fromText "\nFor machine regular:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r2)))
                    else fromText "For machine brief:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r1)))
                          <> fromText "\nFor machine regular:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r2)))
                          <> fromText "\nFor machine detailed:\n"
                          <> asCode (fromText (toStrict (encodeToLazyText r3)))
        , case asMetrics dmPrototype of
            [] -> mempty
            l -> mconcat
                  (intersperse (singleton '\n')
                    (map
                      (\case
                        (IntM ns i) ->
                          fromText "Integer metrics:\n"
                          <> asCode (mconcat $ intersperse (singleton '.')
                                              (map fromText ns))
                          <> singleton ' '
                          <> fromString (show i)
                        (DoubleM ns i) ->
                          fromText "Double metrics:\n"
                          <> asCode (mconcat $ intersperse (singleton '.')
                                              (map fromText ns))
                          <> singleton ' '
                          <> fromString (show i))
                      l))
        ]

    propertiesBuilder :: LogDoc -> Builder
    propertiesBuilder LogDoc {..} =
        case nub ldSeverity of
          []  -> fromText "> Severity:   " <> asCode (fromString (show Info))
          [s] -> fromText "> Severity:   " <> asCode (fromString (show s))
          l   -> fromText "> Severities: "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))
      <>
        case nub ldPrivacy of
          []  -> fromText "\nPrivacy:   " <> asCode (fromString (show Public))
          [p] -> fromText "\nPrivacy:   " <> asCode (fromString (show p))
          l   -> fromText "\nPrivacies: "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))

    configBuilder :: LogDoc -> Builder
    configBuilder LogDoc {..} =
      fromText "From current configuration:\n"
      <> case nub ldDetails of
          []  -> fromText "Details:   " <> asCode (fromString (show DRegular))
          [d] -> fromText "Details:   " <> asCode (fromString (show d))
          l   -> fromText "Details:   "
                  <> mconcat (intersperse (singleton ',')
                        (map (asCode . fromString . show) l))
      <> fromText "\n"
      <> backendsBuilder (nub ldBackends)
      <> fromText "\n"
      <> filteredBuilder (nub ldFiltered) (nub ldSeverity)
      <> limiterBuilder (nub ldLimiter)

    backendsBuilder :: [(BackendConfig, FormattedMessage)] -> Builder
    backendsBuilder [] = fromText "No backends found"
    backendsBuilder l  = fromText "Backends:\n\t\t\t"
                          <> mconcat (intersperse (fromText ",\n\t\t\t")
                                (map backendFormatToText l))

    backendFormatToText :: (BackendConfig, FormattedMessage) -> Builder
    backendFormatToText (be, FormattedMetrics _) = asCode (fromString   (show be))

    backendFormatToText (be, FormattedHuman _c _) = asCode (fromString (show be))
    backendFormatToText (be, FormattedMachine _) = asCode (fromString (show be))

    filteredBuilder :: [SeverityF] -> [SeverityS] -> Builder
    filteredBuilder [] _ = mempty
    filteredBuilder l r =
      fromText "Filtered: "
      <> case (l, r) of
            ([lh], [rh]) ->
              if fromEnum rh >= fromEnum lh
                then (asCode . fromString) "Visible"
                else (asCode . fromString) "Invisible"
            _ -> mempty
      <> fromText " ~ "
      <> mconcat (intersperse (fromText ", ")
          (map (asCode . fromString . show) l))

    limiterBuilder ::
         [(Text, Double)]
      -> Builder
    limiterBuilder [] = mempty
    limiterBuilder l  =
      fromText "\nLimiters: "
        <> mconcat (intersperse (fromText ", ")
            (map (\ (n, d) ->  fromText "Limiter "
                            <> (asCode . fromText) n
                            <> fromText " with frequency "
                            <> (asCode . fromString. show) d)
                 l))

    fMetrics :: (BackendConfig, FormattedMessage) -> Bool
    fMetrics (EKGBackend, FormattedMetrics (_hd:_tl)) = True
    fMetrics _                                        = False

    metricsBuilder ::
         Map.Map Namespace Text
      -> [(BackendConfig, FormattedMessage)]
      -> Builder
    metricsBuilder _ [] = mempty
    metricsBuilder metricsDoc l  =
      mconcat $ map (metricsFormatToText metricsDoc) l

    metricsFormatToText ::
         Map.Map Namespace Text
      -> (BackendConfig, FormattedMessage)
      -> Builder
    metricsFormatToText metricsDoc (_be, FormattedMetrics l) =
      mconcat (intersperse (fromText ",\n")
        (map (metricFormatToText metricsDoc) l))

    metricFormatToText :: Map.Map Namespace Text -> Metric -> Builder
    metricFormatToText metricsDoc (IntM ns _) =
      fromText "#### _Int metric:_ "
        <> (mconcat (intersperse (singleton '.') (map fromText ns)))
          <> fromText "\n"
            <> case Map.lookup ns metricsDoc of
                        Just ""   -> mempty
                        Just text -> betweenLines (fromText text)
                        Nothing   -> mempty

    metricFormatToText metricsDoc (DoubleM ns _) =
      fromText "#### _Double metric:_ "
          <> mconcat (intersperse (singleton '.') (map fromText ns))
            <> fromText "\n"
              <> case Map.lookup ns metricsDoc of
                        Just ""   -> mempty
                        Just text -> betweenLines (fromText text)
                        Nothing   -> mempty
    metricFormatToText metricsDoc (CounterM ns _) =
      fromText "#### _Counter metric:_ "
          <> mconcat (intersperse (singleton '.') (map fromText ns))
            <> fromText "\n"
              <> case Map.lookup ns metricsDoc of
                        Just ""   -> mempty
                        Just text -> betweenLines (fromText text)
                        Nothing   -> mempty

asCode :: Builder -> Builder
asCode b = singleton '`' <> b <> singleton '`'

betweenLines :: Builder -> Builder
betweenLines b = fromText "\n***\n" <> b <> fromText "\n***\n"

_listIndex :: [a] -> Int -> Maybe a
_listIndex l i = if i >= length l
                  then Nothing
                  else Just (l !! i)
