{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.Logging.Trace (
    traceNamed
  , traceWith
  , filterTrace
  , filterTraceMaybe
  , filterTraceBySeverity
  , withLoggingContext
  , appendName
  , withNamesAppended
  , setSeverity
  , withSeverity
  , privately
  , setPrivacy
  , withPrivacy
  , allPublic
  , allConfidential
  , filterTraceByPrivacy
  , setDetails
  , withDetails
  , foldTraceM
  , foldMTraceM
  , routingTrace
)

where

import           Control.Monad (join)
import           Control.Monad.IO.Unlift
import qualified Control.Tracer as T
import           Data.Maybe (isJust)
import           Data.Text (Text)
import           UnliftIO.MVar

import           Cardano.Logging.Types

-- | Adds a message object to a trace
traceWith :: Monad m => Trace m a -> a -> m ()
traceWith (Trace tr) a =
    T.traceWith tr (emptyLoggingContext, Nothing, a)

-- | Convenience function for tracing a message with a name
--   As the simple name suggest, this should be the standard function
traceNamed :: Monad m => Trace m a -> Text -> a -> m ()
traceNamed tr n = traceWith (appendName n tr)

--- | Don't process further if the result of the selector function
---   is False.
filterTrace :: (Monad m) =>
     ((LoggingContext, Maybe TraceControl, a) -> Bool)
  -> Trace m a
  -> Trace m a
filterTrace ff (Trace tr) = Trace $ T.squelchUnless
    (\case
      (_lc, Just _, _a)     -> True
      (lc, mbC, a)          -> ff (lc, mbC, a))
      tr

--- | Keep the Just values and forget about the Nothings
filterTraceMaybe :: Monad m =>
     Trace m a
  -> Trace m (Maybe a)
filterTraceMaybe (Trace tr) = Trace $
    T.squelchUnless
      (\case
        (_lc, _mbC, Just _a) -> True
        (_lc, _mbC, Nothing) -> False)
      (T.contramap
          (\case
            (lc, mbC, Just a)     -> (lc, mbC, a)
            (_lc, _mbC, Nothing)  -> error "filterTraceMaybe: impossible")
          tr)

--- | Only processes messages further with a severity equal or greater as the
--- given one
filterTraceBySeverity :: Monad m =>
     Maybe SeverityF
  -> Trace m a
  -> Trace m a
filterTraceBySeverity (Just minSeverity) =
    filterTrace $
      \case
        (_lc, Just _, _a)  -> True
        (lc, _, _e)                ->
          case lcSeverity lc of
            Just s  -> fromEnum s >= fromEnum minSeverity
            Nothing -> True
filterTraceBySeverity Nothing = id

-- | Sets a new logging context for this message
withLoggingContext :: Monad m => LoggingContext -> Trace m a -> Trace m a
withLoggingContext lc (Trace tr) = Trace $
    T.contramap
      (\
        (_lc, mbC, e) -> (lc, mbC, e))
      tr

-- | Appends a name to the context.
-- E.g. appendName "specific" $ appendName "middle" $ appendName "general" tracer
-- give the result: `general.middle.specific`.
appendName :: Monad m => Text -> Trace m a -> Trace m a
appendName name (Trace tr) = Trace $
    T.contramap
      (\
        (lc, mbC, e) -> (lc {lcNamespace = name : lcNamespace lc}, mbC, e))
      tr

-- | Sets names for the messages in this trace based on the selector function
withNamesAppended :: Monad m => (a -> [Text]) -> Trace m a -> Trace m a
withNamesAppended func (Trace tr) = Trace $
    T.contramap
      (\
        (lc, mbC, e) -> (lc {lcNamespace = func e ++ lcNamespace lc}, mbC, e))
      tr

-- | Sets severity for the messages in this trace
setSeverity :: Monad m => SeverityS -> Trace m a -> Trace m a
setSeverity s (Trace tr) = Trace $ T.contramap
  (\ (lc, mbC, e) -> if isJust (lcSeverity lc)
                        then (lc, mbC, e)
                        else (lc {lcSeverity = Just s}, mbC, e))
  tr

-- | Sets severities for the messages in this trace based on the selector function
withSeverity :: Monad m => (a -> SeverityS) -> Trace m a -> Trace m a
withSeverity fs (Trace tr) = Trace $
    T.contramap
      (\
        (lc, mbC, e) -> if isJust (lcSeverity lc)
                            then (lc, mbC, e)
                            else (lc {lcSeverity = Just (fs e)}, mbC, e))
      tr

--- | Only processes messages further with a privacy greater then the given one
filterTraceByPrivacy :: (Monad m) =>
     Maybe Privacy
  -> Trace m a
  -> Trace m a
filterTraceByPrivacy (Just minPrivacy) = filterTrace $
    \case
      (_lc, Just _, _a)     -> True
      (c, _mbC, _e) ->
        case lcPrivacy c of
          Just s  -> fromEnum s >= fromEnum minPrivacy
          Nothing -> True
filterTraceByPrivacy Nothing = id

allPublic :: a -> Privacy
allPublic _ = Public

allConfidential :: a -> Privacy
allConfidential _ = Confidential


-- | Sets privacy Confidential for the messages in this trace
privately :: Monad m => Trace m a -> Trace m a
privately = setPrivacy Confidential

-- | Sets privacy for the messages in this trace
setPrivacy :: Monad m => Privacy -> Trace m a -> Trace m a
setPrivacy p (Trace tr) = Trace $
  T.contramap
    (\ (lc, mbC, v) -> if isJust (lcPrivacy lc)
                      then (lc, mbC, v)
                      else (lc {lcPrivacy = Just p}, mbC, v))
    tr

-- | Sets privacy for the messages in this trace based on the message
withPrivacy :: Monad m => (a -> Privacy) -> Trace m a -> Trace m a
withPrivacy fs (Trace tr) = Trace $
    T.contramap
      (\ (lc, mbC, e) -> if isJust (lcPrivacy lc)
                            then (lc, mbC, e)
                            else (lc {lcPrivacy = Just (fs e)}, mbC, e))
      tr

-- | Sets detail level for the messages in this trace
setDetails :: Monad m => DetailLevel -> Trace m a -> Trace m a
setDetails p (Trace tr) = Trace $
    T.contramap
      (\ (lc, mbC, v) -> if isJust (lcDetails lc)
                        then (lc, mbC, v)
                        else (lc {lcDetails = Just p}, mbC, v))
      tr

-- | Sets detail level for the messages in this trace based on the message
withDetails :: Monad m => (a -> DetailLevel) -> Trace m a -> Trace m a
withDetails fs (Trace tr) = Trace $
  T.contramap
    (\
      (lc, mbC, e) -> if isJust (lcDetails lc)
                          then (lc, mbC, e)
                          else (lc {lcDetails = Just (fs e)}, mbC, e))
    tr

-- | Folds the cata function with acc over a.
-- Uses an MVar to store the state
foldTraceM
  :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldTraceM cata initial (Trace tr) = do
  ref <- liftIO (newMVar initial)
  let trr = mkTracer ref
  pure $ Trace (T.Tracer trr)
 where
    mkTracer ref = T.emit $
      \case
        (lc, Nothing, v) -> do
          x' <- modifyMVar ref $ \x ->
            let ! accu = cata x lc v
            in pure $ join (,) accu
          T.traceWith tr (lc, Nothing, Folding x')
        (lc, Just control, _v) -> do
          T.traceWith tr (lc, Just control, Folding initial)

-- | Folds the monadic cata function with acc over a.
-- Uses an IORef to store the state
foldMTraceM
  :: forall a acc m . (MonadUnliftIO m)
  => (acc -> LoggingContext -> a -> m acc)
  -> acc
  -> Trace m (Folding a acc)
  -> m (Trace m a)
foldMTraceM cata initial (Trace tr) = do
  ref <- liftIO (newMVar initial)
  let trr = mkTracer ref
  pure $ Trace (T.arrow trr)
 where
    mkTracer ref = T.emit $
      \case
        (lc, Nothing, v) -> do
          x' <- modifyMVar ref $ \x -> do
            ! accu <- cata x lc v
            pure $ join (,) accu
          T.traceWith tr (lc, Nothing, Folding x')
        (lc, Just control, _v) -> do
          T.traceWith tr (lc, Just control, Folding initial)

-- | Allows to route to different tracers, based on the message being processed.
--   The second argument must mappend all possible tracers of the first
--   argument to one tracer. This is required for the configuration!
routingTrace
  :: forall m a. Monad m
  => (a -> m (Trace m a))
  -> Trace m a
  -> m (Trace m a)
routingTrace rf rc = pure $ Trace $ T.arrow $ T.emit $
    \case
      (lc, Nothing, a) -> do
          nt <- rf a
          T.traceWith (unpackTrace nt) (lc, Nothing, a)
      (lc, Just control, a) ->
          T.traceWith (unpackTrace rc) (lc, Just control, a)
