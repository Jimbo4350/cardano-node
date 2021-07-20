{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Logs.File
  ( tests
  ) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Control.Monad (filterM)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.FilePath

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (isItLog, isItSymLink)
import           Cardano.Tracer.Run (runCardanoTracerWithConfig)

import           Cardano.Tracer.Test.Forwarder (launchForwardersSimple)

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs.File"
  [ testProperty ".log"  $ propFile ForHuman   "text" "cardano-tracer-log.sock"
  , testProperty ".json" $ propFile ForMachine "json" "cardano-tracer-json.sock"
  ]

propFile
  :: LogFormat
  -> FilePath
  -> String
  -> Property
propFile format suffix localSockName = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  let rootDir = tmpDir </> ("test-logs-" <> suffix)
      localSock = tmpDir </> localSockName
  -- Remove rootDir if needed.
  removePathForcibly rootDir
  -- Remove localSock if needed.
  removePathForcibly localSock
  -- Run cardano-tracer and demo-forwarder-mux.
  tracerThr <- forkIO $ runCardanoTracerWithConfig (config rootDir localSock)
  threadDelay 500000
  forwarderThr <- forkIO $ launchForwardersSimple localSock
  -- Wait for some 'TraceObject's...
  threadDelay 5000000
  -- Stop both sides.
  killThread forwarderThr
  killThread tracerThr
  threadDelay 100000
  -- Check that rootDir exists...
  doesDirectoryExist rootDir >>= \case
    True ->
      -- ... and contains one node's subdir...
      listDirectory rootDir >>= \case
        []  -> false "root dir is empty"
        [subDir] ->
          withCurrentDirectory rootDir $
            -- ... with *.log-files inside...
            listDirectory subDir >>= \case
              [] -> false "subdir is empty"
              logsAndSymLink ->
                withCurrentDirectory subDir $
                  case filter (isItLog format) logsAndSymLink of
                    [] -> false "subdir doesn't contain expected logs"
                    logsWeNeed ->
                      -- ... and one symlink...
                      filterM (isItSymLink format) logsAndSymLink >>= \case
                        [] -> false "subdir doesn't contain a symlink"
                        [symLink] -> do
                          -- ... to the latest *.log-file.
                          maybeLatestLog <- getSymbolicLinkTarget symLink
                          -- The logs' names contain timestamps, so the
                          -- latest log is the maximum one.
                          let latestLog = maximum logsWeNeed
                          return $ latestLog === maybeLatestLog
                        _ -> false "there is more than one symlink"
        _ -> false "root dir contains more than one subdir"
    False -> false "root dir doesn't exist"
 where
  config rootDir' localSock' = TracerConfig
    { connectMode    = Initiator
    , acceptAt       = [LocalSocket localSock']
    , loRequestNum   = 1
    , ekgRequestFreq = 1.0
    , hasEKG         = Nothing
    , hasPrometheus  = Nothing
    , logging        = [LoggingParams rootDir' FileMode format]
    , rotation       = Nothing
    }

  false :: String -> IO Property
  false msg = return . counterexample msg $ property False
