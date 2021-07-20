{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.Standard (
    standardTracer
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan.Unagi.Bounded
import           Control.Monad (forever)
import           Control.Monad.IO.Class
import           Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           GHC.Conc (ThreadId)
import           System.IO (hFlush, stdout)

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

import qualified Control.Tracer as T

-- | Do we log to stdout or to a file?
data LogTarget = LogStdout | LogFile FilePath
  deriving (Eq, Show)

-- | The state of a standard tracer
data StandardTracerState a =  StandardTracerState {
    stRunning :: Maybe (InChan Text, OutChan Text, ThreadId)
  , stTarget  :: LogTarget
}

emptyStandardTracerState :: Maybe FilePath -> StandardTracerState a
emptyStandardTracerState Nothing   = StandardTracerState Nothing LogStdout
emptyStandardTracerState (Just fp) = StandardTracerState Nothing (LogFile fp)


standardTracer :: forall m. (MonadIO m)
  => Maybe FilePath
  -> m (Trace m FormattedMessage)
standardTracer mbFilePath = do
    stateRef <- liftIO $ newIORef (emptyStandardTracerState mbFilePath)
    pure $ Trace $ T.arrow $ T.emit $ uncurry3 (output stateRef)
  where
    output ::
         IORef (StandardTracerState a)
      -> LoggingContext
      -> Maybe TraceControl
      -> FormattedMessage
      -> m ()
    output stateRef LoggingContext {} Nothing (FormattedHuman _c msg) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext {} Nothing (FormattedMachine msg) = liftIO $ do
      st  <- readIORef stateRef
      case stRunning st of
        Just (inChannel, _, _) -> writeChan inChannel msg
        Nothing                -> pure ()
    output stateRef LoggingContext {} (Just Reset) _msg = liftIO $ do
      st <- readIORef stateRef
      case stRunning st of
        Nothing -> initLogging stateRef
        Just _  -> pure ()
    output _ lk (Just c@Document {}) (FormattedHuman co msg) =
       docIt
        (Stdout (if co then HumanFormatColoured else HumanFormatUncoloured))
        (FormattedHuman co "")
        (lk, Just c, msg)
    output _ lk (Just c@Document {}) (FormattedMachine msg) =
       docIt (Stdout MachineFormat) (FormattedMachine "") (lk, Just c, msg)
    output _stateRef LoggingContext {} _ _a = pure ()

-- TODO: care about reconfiguration
initLogging :: IORef (StandardTracerState a) -> IO ()
initLogging stateRef = do
  (inChan, outChan) <- newChan 2048
  threadId <- forkIO $ forever $ do
    msg   <- readChan outChan
    state <- readIORef stateRef
    case stTarget state of
        LogFile f -> do
                        TIO.appendFile f msg
                        TIO.appendFile f "\n"
        LogStdout -> do
                        TIO.putStrLn msg
                        hFlush stdout
  modifyIORef stateRef (\ st ->
    st {stRunning = Just (inChan, outChan, threadId)})

-- | Converts a curried function to a function on a triple.
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f ~(a,b,c) = f a b c
