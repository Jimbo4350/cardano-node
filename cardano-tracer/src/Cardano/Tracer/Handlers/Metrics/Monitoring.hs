{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Monitoring
  ( runMonitoringServer
  ) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (SomeException, try)
import           Control.Monad (forever, unless, void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           System.IO (hPutStrLn, stderr)
import           System.Remote.Monitoring (forkServerWith)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types (AcceptedItems)

runMonitoringServer
  :: Endpoint
  -> AcceptedItems
  -> IO ()
runMonitoringServer (Endpoint host port) acceptedItems = forever $ do
  (try serveEKGPage) >>= \case
    Left (e :: SomeException) ->
      hPutStrLn stderr $ "Problem with EKG web server: " <> show e
    Right _ -> return ()
  threadDelay 1000000
 where
  serveEKGPage = do
    items <- readIORef acceptedItems
    unless (HM.null items) $ do
      -- TODO: temporary solution for testing
      -- (serve the metrics received from the first found node only).
      let (_, _, (storeForFirstNode, _)) = snd . head . HM.toList $ items
      void $ forkServerWith storeForFirstNode (BSC.pack host) port
      waitForever

  waitForever = forever $ threadDelay 1000000000
