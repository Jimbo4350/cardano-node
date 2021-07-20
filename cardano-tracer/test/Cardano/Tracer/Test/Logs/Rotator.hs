{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Test.Logs.Rotator
  ( tests
  ) where

import           Control.Concurrent (forkIO, killThread, threadDelay)
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           System.Directory
import           System.FilePath

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log (isItLog)
import           Cardano.Tracer.Run (runCardanoTracerWithConfig)

import           Cardano.Tracer.Test.Forwarder (launchForwardersSimple)

tests :: TestTree
tests = localOption (QuickCheckTests 1) $ testGroup "Test.Logs.Rotator"
  [ testProperty "basic" $ propRotator "cardano-tracer-totator.sock"
  ]

propRotator :: String -> Property
propRotator localSockName = ioProperty $ do
  tmpDir <- getTemporaryDirectory
  let rootDir = tmpDir </> "test-logs-rotator"
      localSock = tmpDir </> localSockName
  -- Remove rootDir if needed.
  removePathForcibly rootDir
  -- Remove localSock if needed.
  removePathForcibly localSock
  -- Run cardano-tracer and demo-forwarder-mux.
  tracerThr <- forkIO $ runCardanoTracerWithConfig (config rootDir localSock)
  threadDelay 500000
  forwarderThr <- forkIO $ launchForwardersSimple localSock
  -- Wait while rotation will occure...
  threadDelay 25000000
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
                    logsWeNeed -> do
                      let thereAreMoreThanOneLog = length logsWeNeed > 1
                      return $ thereAreMoreThanOneLog === True
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
    , rotation       = Just $
        RotationParams
          { rpLogLimitBytes = 100
          , rpMaxAgeHours   = 1
          , rpKeepFilesNum  = 10
          }
    }

  format = ForHuman

  false :: String -> IO Property
  false msg = return . counterexample msg $ property False
