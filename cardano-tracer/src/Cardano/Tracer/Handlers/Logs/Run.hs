{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Run
  ( runLogsHandler
  ) where

import           Control.Exception (SomeException, try)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, forConcurrently_,
                                           uninterruptibleCancel)
import           Control.Concurrent.STM (STM, atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, tryReadTBQueue)
import           Control.Monad (forM_, forever, void)
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Data.Maybe (fromMaybe)
import           System.IO (hPutStrLn, stderr)

import           Cardano.Logging (TraceObject)

import           Trace.Forward.Protocol.Type (NodeInfoStore)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems, TraceObjects, Metrics,
                                       NodeId, NodeName, getNodeName)
import           Cardano.Tracer.Handlers.Logs.File (writeTraceObjectsToFile)
import           Cardano.Tracer.Handlers.Logs.Journal (writeTraceObjectsToJournal)
import           Cardano.Tracer.Handlers.Logs.Rotator (runLogsRotator)

runLogsHandler
  :: TracerConfig
  -> AcceptedItems
  -> IO ()
runLogsHandler config acceptedItems = do
  rotThr <- async $ runLogsRotator config
  void . forever $ do
    itemsFromAllNodes <- HM.toList <$> readIORef acceptedItems
    forConcurrently_ itemsFromAllNodes $ handleItemsFromNode config
    threadDelay 1000000 -- Take 'TraceObject's from the queue every second.
  uninterruptibleCancel rotThr

handleItemsFromNode
  :: TracerConfig
  -> (NodeId, (NodeInfoStore, TraceObjects, Metrics))
  -> IO ()
handleItemsFromNode config (nodeId, (niStore, loQueue, _)) = do
  nodeName <- fromMaybe "" <$> getNodeName niStore
  atomically (getAllTraceObjects loQueue) >>= writeTraceObjects config nodeId nodeName

getAllTraceObjects :: TBQueue lo -> STM [lo]
getAllTraceObjects loQueue =
  tryReadTBQueue loQueue >>= \case
    Just lo' -> (:) lo' <$> getAllTraceObjects loQueue
    Nothing  -> return []

writeTraceObjects
  :: TracerConfig
  -> NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjects _ _ _ [] = return ()
writeTraceObjects config nodeId nodeName logObjects =
  try (writeTraceObjects' config nodeId nodeName logObjects) >>= \case
    Left (e :: SomeException) ->
      hPutStrLn stderr $ "writeTraceObjects error: " <> show e
    Right _ -> return ()

writeTraceObjects'
  :: TracerConfig
  -> NodeId
  -> NodeName
  -> [TraceObject]
  -> IO ()
writeTraceObjects' config nodeId nodeName logObjects =
  forM_ (logging config) $ \LoggingParams{..} ->
    case logMode of
      FileMode    -> writeTraceObjectsToFile nodeId nodeName logRoot logFormat logObjects
      JournalMode -> writeTraceObjectsToJournal nodeId nodeName logObjects
