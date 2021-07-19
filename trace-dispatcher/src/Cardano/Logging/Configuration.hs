{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Cardano.Logging.Configuration
  ( configureTracers
  , withNamespaceConfig
  , filterSeverityFromConfig
  , withDetailsFromConfig
  , withBackendsFromConfig
  , withLimitersFromConfig
  , readConfiguration
  ) where

import           Control.Exception (throwIO)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Control.Tracer as T
import qualified Data.Aeson as AE
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (foldl', maximumBy, nub)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe, mapMaybe)
import           Data.Text (Text, split)
import           Data.Yaml
import           GHC.Generics

import           Cardano.Logging.FrequencyLimiter (LimitingMessage (..),
                     limitFrequency)
import           Cardano.Logging.Trace (filterTraceBySeverity, setDetails)
import           Cardano.Logging.Types

-- | Call this function at initialisation, and later for reconfiguration
configureTracers :: Monad m => TraceConfig -> Documented a -> [Trace m a]-> m ()
configureTracers config (Documented documented) tracers = do
    mapM_ (configureTrace Reset) tracers
    mapM_ (configureAllTrace (Config config)) tracers
    mapM_ (configureTrace Optimize) tracers
  where
    configureTrace control (Trace tr) =
      T.traceWith tr (emptyLoggingContext, Just control, dmPrototype (head documented))
    configureAllTrace control (Trace tr) =
      mapM
        ((\ m -> T.traceWith tr (emptyLoggingContext, Just control, m)) . dmPrototype)
        documented

-- | Take a selector function called 'extract'.
-- Take a function from trace to trace with this config dependent value.
-- In this way construct a trace transformer with a config value
withNamespaceConfig :: forall m a b c. (MonadIO m, Ord b) =>
     (TraceConfig -> Namespace -> m b)
  -> (Maybe b -> Trace m c -> m (Trace m a))
  -> Trace m c
  -> m (Trace m a)
withNamespaceConfig extract withConfig tr = do
    ref  <- liftIO (newIORef (Left (Map.empty, Nothing)))
    pure $ Trace $ T.arrow $ T.emit $ mkTrace ref
  where
    mkTrace ::
         IORef (Either (Map.Map Namespace b, Maybe b) b)
      -> (LoggingContext, Maybe TraceControl, a)
      -> m ()
    mkTrace ref (lc, Nothing, a) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Right val -> do
          tt <- withConfig (Just val) tr
          T.traceWith
            (unpackTrace tt) (lc, Nothing, a)
        Left (cmap, Just v) ->
          case Map.lookup (lcNamespace lc) cmap of
                Just val -> do
                  tt <- withConfig (Just val) tr
                  T.traceWith (unpackTrace tt) (lc, Nothing, a)
                Nothing  -> do
                  tt <- withConfig (Just v) tr
                  T.traceWith (unpackTrace tt) (lc, Nothing, a)
        Left (_cmap, Nothing) -> error ("Missing configuration " <> show (lcNamespace lc))
    mkTrace ref (lc, Just Reset, a) = do
      liftIO $ writeIORef ref (Left (Map.empty, Nothing))
      tt <- withConfig Nothing tr
      T.traceWith (unpackTrace tt) (lc, Just Reset, a)

    mkTrace ref (lc, Just (Config c), m) = do
      ! val <- extract c (lcNamespace lc)
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case Map.lookup (lcNamespace lc) cmap of
            Nothing -> do
              liftIO
                  $ writeIORef ref
                  $ Left (Map.insert (lcNamespace lc) val cmap, Nothing)
              tt <- withConfig (Just val) tr
              T.traceWith (unpackTrace tt) (lc, Just (Config c), m)
            Just v  -> do
              if v == val
                then do
                  tt <- withConfig (Just val) tr
                  T.traceWith (unpackTrace tt) (lc, Just (Config c), m)
                else error $ "Inconsistent trace configuration with context "
                                  ++ show (lcNamespace lc)
        Right _val -> error $ "Trace not reset before reconfiguration (1)"
                            ++ show (lcNamespace lc)
        Left (_cmap, Just _v) -> error $ "Trace not reset before reconfiguration (2)"
                            ++ show (lcNamespace lc)

    mkTrace ref (lc, Just Optimize, m) = do
      eitherConf <- liftIO $ readIORef ref
      case eitherConf of
        Left (cmap, Nothing) ->
          case nub (Map.elems cmap) of
            []     ->  -- This will never be called!?
                      pure ()
            [val]  -> do
                        liftIO $ writeIORef ref $ Right val
                        tt <- withConfig (Just val) tr
                        T.traceWith (unpackTrace tt) (lc, Just Optimize, m)
            _      -> let decidingDict =
                            foldl
                              (\acc e -> Map.insertWith (+) e (1 :: Int) acc)
                              Map.empty
                              (Map.elems cmap)
                          (mostCommon, _) = maximumBy
                                              (\(_, n') (_, m') -> compare n' m')
                                              (Map.assocs decidingDict)
                          newmap = Map.filter (/= mostCommon) cmap
                      in do
                        liftIO $ writeIORef ref (Left (newmap, Just mostCommon))
                        tt <- withConfig Nothing tr
                        T.traceWith (unpackTrace tt) (lc, Just Optimize, m)
        Right _val -> error $ "Trace not reset before reconfiguration (3)"
                            ++ show (lcNamespace lc)
        Left (_cmap, Just _v) ->
                      error $ "Trace not reset before reconfiguration (4)"
                                  ++ show (lcNamespace lc)

-- | Filter a trace by severity and take the filter value from the config
filterSeverityFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
filterSeverityFromConfig =
    withNamespaceConfig
      getSeverity
      (\ a b -> pure $ filterTraceBySeverity a b)

-- | Set detail level of a trace from the config
withDetailsFromConfig :: (MonadIO m) =>
     Trace m a
  -> m (Trace m a)
withDetailsFromConfig =
  withNamespaceConfig
    getDetails
    (\mbDtl b -> case mbDtl of
              Just dtl -> pure $ setDetails dtl b
              Nothing  -> pure $ setDetails DRegular b)

-- | Routing and formatting of a trace from the config
withBackendsFromConfig :: (MonadIO m) =>
  (Maybe [BackendConfig] -> Trace m FormattedMessage -> m (Trace m a))
  -> m (Trace m a)
withBackendsFromConfig routerAndFormatter =
  withNamespaceConfig
    getBackends
    routerAndFormatter
    (Trace T.nullTracer)

data Limiter m a = Limiter Text (Trace m a)

instance Eq (Limiter m a) where
  Limiter t1 _ == Limiter t2 _ = t1 == t2

instance Ord (Limiter m a) where
  Limiter t1 _ <= Limiter t2 _ = t1 <= t2


-- | Routing and formatting of a trace from the config
withLimitersFromConfig :: forall a m .(MonadUnliftIO m) =>
     Trace m a
  -> Trace m LimitingMessage
  -> m (Trace m a)
withLimitersFromConfig tr trl = do
    ref <- liftIO $ newIORef Map.empty
    withNamespaceConfig
      (getLimiter ref)
      applyLimiter
      tr
  where
    -- | May return a limiter, which is a stateful transformation from trace to trace
    getLimiter ::
         IORef (Map.Map Text (Limiter m a))
      -> TraceConfig
      -> Namespace
      -> m (Limiter m a)
    getLimiter stateRef config ns =
      case getLimiterSpec config ns of
        Nothing -> pure (Limiter "noop" tr)
        Just (name, frequency) -> do
          state <- liftIO $ readIORef stateRef
          case Map.lookup name state of
            Just limiter -> pure limiter
            Nothing -> do
              limiterTrace <- limitFrequency frequency name tr trl
              let limiter = Limiter name limiterTrace
              liftIO $ writeIORef stateRef (Map.insert name limiter state)
              pure $ limiter

    applyLimiter ::
         Maybe (Limiter m a)
      -> Trace m a
      -> m (Trace m a)
    applyLimiter Nothing trace                     = pure trace
    applyLimiter (Just (Limiter _n trace')) _trace = pure trace'


_allLimiters :: TraceConfig -> [(Text, Double)]
_allLimiters TraceConfig {..} = Map.foldrWithKey' extractor [] tcOptions
  where
    extractor ns configOptions limiterSpecs =
      foldr (extractor' ns) limiterSpecs configOptions
    extractor' _ns (CoLimiter name freq) accu = (name, freq) : accu

-- -----------------------------------------------------------------------------
-- Configuration file

readConfiguration :: FilePath -> IO TraceConfig
readConfiguration fp =
    either throwIO pure =<< parseRepresentation <$> BS.readFile fp

parseRepresentation :: ByteString -> Either ParseException TraceConfig
parseRepresentation bs = fill (decodeEither' bs)
  where
    fill ::
         Either ParseException ConfigRepresentation
      -> Either ParseException TraceConfig
    fill (Left e)   = Left e
    fill (Right rl) = Right $ fill' emptyTraceConfig rl
    fill' :: TraceConfig -> ConfigRepresentation -> TraceConfig
    fill' (TraceConfig tc _fc _fcc) cr =
      let tc'  = foldl' (\ tci (TraceOptionSeverity ns severity') ->
                          let ns' = split (=='.') ns
                              ns'' = if ns' == [""] then [] else ns'
                          in Map.insertWith (++) ns'' [CoSeverity severity'] tci)
                        tc
                        (traceOptionSeverity cr)
          tc'' = foldl' (\ tci (TraceOptionDetail ns detail') ->
                          let ns' = split (=='.') ns
                              ns'' = if ns' == [""] then [] else ns'
                          in Map.insertWith (++) ns'' [CoDetail detail'] tci)
                        tc'
                        (traceOptionDetail cr)
          tc''' = foldl' (\ tci (TraceOptionBackend ns backend') ->
                          let ns' = split (=='.') ns
                              ns'' = if ns' == [""] then [] else ns'
                          in Map.insertWith (++) ns'' [CoBackend backend'] tci)
                        tc''
                        (traceOptionBackend cr)
          tc'''' = foldl' (\ tci (TraceOptionLimiter ns name frequ) ->
                          let ns' = split (=='.') ns
                              ns'' = if ns' == [""] then [] else ns'
                          in Map.insertWith (++) ns'' [CoLimiter name frequ] tci)
                        tc'''
                        (traceOptionLimiter cr)
      in TraceConfig
          tc''''
          (traceOptionForwarder cr)
          (traceOptionForwardCache cr)

data TraceOptionSeverity = TraceOptionSeverity {
      nsS      :: Text
    , severity :: SeverityF
    } deriving (Eq, Ord, Show)

instance AE.ToJSON TraceOptionSeverity where
    toJSON tos = object [ "ns" .= nsS tos
                        , "severity" .= AE.toJSON (severity tos)
                        ]

instance AE.FromJSON TraceOptionSeverity where
    parseJSON (Object obj) = TraceOptionSeverity
                           <$> obj .: "ns"
                           <*> obj .: "severity"

data TraceOptionDetail = TraceOptionDetail {
      nsD    :: Text
    , detail :: DetailLevel
    } deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON TraceOptionDetail where
    toJSON tos = object [ "ns" .= nsD tos
                        , "detail" .= AE.toJSON (detail tos)
                        ]

instance AE.FromJSON TraceOptionDetail where
    parseJSON (Object obj) = TraceOptionDetail
                           <$> obj .: "ns"
                           <*> obj .: "detail"

data TraceOptionBackend = TraceOptionBackend {
      nsB      :: Text
    , backends :: [BackendConfig]
    } deriving (Eq, Ord, Show, Generic)

instance AE.ToJSON TraceOptionBackend where
    toJSON tos = object [ "ns" .= nsB tos
                        , "backends" .= AE.toJSON (backends tos)
                        ]

instance AE.FromJSON TraceOptionBackend where
    parseJSON (Object obj) = TraceOptionBackend
                           <$> obj .: "ns"
                           <*> obj .: "backends"


data TraceOptionLimiter = TraceOptionLimiter {
      nsL              :: Text
    , limiterName      :: Text
    , limiterFrequency :: Double
    } deriving (Eq, Ord, Show)

instance AE.ToJSON TraceOptionLimiter where
    toJSON tos = object [ "ns" .= nsL tos
                        , "limiterName" .= limiterName tos
                        , "limiterFrequency" .= limiterFrequency tos
                        ]

instance AE.FromJSON TraceOptionLimiter where
    parseJSON (Object obj) = TraceOptionLimiter
                           <$> obj .: "ns"
                           <*> obj .: "limiterName"
                           <*> obj .: "limiterFrequency"

data ConfigRepresentation = ConfigRepresentation {
    traceOptionSeverity     :: [TraceOptionSeverity]
  , traceOptionDetail       :: [TraceOptionDetail]
  , traceOptionBackend      :: [TraceOptionBackend]
  , traceOptionLimiter      :: [TraceOptionLimiter]
  , traceOptionForwarder    :: RemoteAddr
  , traceOptionForwardCache :: Int
  }
  deriving (Eq, Ord, Show)

instance AE.FromJSON ConfigRepresentation where
    parseJSON (Object obj) = ConfigRepresentation
                           <$> obj .: "TraceOptionSeverity"
                           <*> obj .: "TraceOptionDetail"
                           <*> obj .: "TraceOptionBackend"
                           <*> obj .: "TraceOptionLimiter"
                           <*> obj .: "TraceOptionForwarder"
                           <*> obj .: "TraceOptionForwardCache"


--------------------------------------------------------
-- Internal

-- | If no severity can be found in the config, it is set to Warning
getSeverity :: Applicative m => TraceConfig -> Namespace -> m SeverityF
getSeverity config ns = pure $
    fromMaybe WarningF (getOption severitySelector config ns)
  where
    severitySelector :: ConfigOption -> Maybe SeverityF
    severitySelector (CoSeverity s) = Just s
    severitySelector _              = Nothing

-- | If no details can be found in the config, it is set to DRegular
getDetails :: Applicative m => TraceConfig -> Namespace -> m DetailLevel
getDetails config ns = pure $
    fromMaybe DRegular (getOption detailSelector config ns)
  where
    detailSelector :: ConfigOption -> Maybe DetailLevel
    detailSelector (CoDetail d) = Just d
    detailSelector _            = Nothing

-- | If no backends can be found in the config, it is set to
-- [EKGBackend, Forwarder, Stdout HumanFormatColoured]
getBackends :: Applicative m => TraceConfig -> Namespace -> m [BackendConfig]
getBackends config ns = pure $
    fromMaybe [EKGBackend, Forwarder, Stdout HumanFormatColoured]
      (getOption backendSelector config ns)
  where
    backendSelector :: ConfigOption -> Maybe [BackendConfig]
    backendSelector (CoBackend s) = Just s
    backendSelector _             = Nothing

-- | May return a limiter specification
getLimiterSpec :: TraceConfig -> Namespace -> Maybe (Text, Double)
getLimiterSpec = getOption limiterSelector
  where
    limiterSelector :: ConfigOption -> Maybe (Text, Double)
    limiterSelector (CoLimiter n f) = Just (n, f)
    limiterSelector _               = Nothing


-- | Searches in the config to find an option
getOption :: (ConfigOption -> Maybe a) -> TraceConfig -> Namespace -> Maybe a
getOption sel config [] =
  case Map.lookup [] (tcOptions config) of
    Nothing -> Nothing
    Just options -> case mapMaybe sel options of
                      []        -> Nothing
                      (opt : _) -> Just opt
getOption sel config ns =
  case Map.lookup ns (tcOptions config) of
    Nothing -> getOption sel config (init ns)
    Just options -> case mapMaybe sel options of
                      []        -> getOption sel config (init ns)
                      (opt : _) -> Just opt
