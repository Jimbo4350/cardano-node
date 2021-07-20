{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Logs.Rotator
  ( runLogsRotator
  ) where

import           Control.Exception (SomeException, try)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad (forM_, forever, when)
import           Control.Monad.Extra (whenM)
import           Data.List (find, nub, sort)
import           Data.Time (diffUTCTime, getCurrentTime)
import           Data.Word (Word64)
import           System.Directory
import           System.Directory.Extra (listDirectories, listFiles)
import           System.FilePath ((</>), takeDirectory, takeFileName)
import           System.IO (hPutStrLn, stderr)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Handlers.Logs.Log

runLogsRotator :: TracerConfig -> IO ()
runLogsRotator TracerConfig{..} =
  case rotation of
    Nothing -> return () -- No rotation parameters are defined.
    Just rotParams -> launchRotator rotParams rootDirsWithFormats
 where
  rootDirsWithFormats = nub . map getRootAndFormat . filter fileParamsOnly $ logging
  fileParamsOnly   LoggingParams{..} = logMode == FileMode
  getRootAndFormat LoggingParams{..} = (logRoot, logFormat)

-- | All the logs with 'TraceObject's received from particular node
-- will be stored in a separate directory, so they can be checked
-- concurrently.
launchRotator
  :: RotationParams
  -> [(FilePath, LogFormat)]
  -> IO ()
launchRotator _ [] = return ()
launchRotator rotParams rootDirsWithFormats = forever $ do
  try (forM_ rootDirsWithFormats $ checkRootDir rotParams) >>= \case
    Left (e :: SomeException) ->
      hPutStrLn stderr $ "Problem with rotation of log files: " <> show e
    Right _ -> return ()
  threadDelay 10000000

checkRootDir
  :: RotationParams
  -> (FilePath, LogFormat)
  -> IO ()
checkRootDir rotParams (rootDir, format) =
  whenM (doesDirectoryExist rootDir) $
    -- All the logs received from particular node will be stored in corresponding subdir.
    listDirectories rootDir >>= \case
      [] ->
        -- There are no nodes' subdirs yet (or they were deleted),
        -- so no rotation can be performed.
        return ()
      subDirs -> do
        let fullPathsToSubDirs = map (rootDir </>) subDirs  
        -- Ok, list of subdirs is here, check each of them in parallel.
        forConcurrently_ fullPathsToSubDirs $ checkLogsFromNode rotParams format

checkLogsFromNode
  :: RotationParams
  -> LogFormat
  -> FilePath
  -> IO ()
checkLogsFromNode RotationParams{..} format subDirForLogs =
  listFiles subDirForLogs >>= \case
    [] ->
      -- There are no logs in this subdir (probably they were deleted),
      -- so no rotation can be performed.
      return ()
    [oneFile] ->
      -- At least two files must be there: one log and one symlink.
      -- So if there is only one file, it's a weird situation,
      -- (probably invalid symlink only), we have to try to fix it.
      fixLog (subDirForLogs </> oneFile) format
    logs -> do
      let fullPathsToLogs = map (subDirForLogs </>) logs
      checkIfCurrentLogIsFull fullPathsToLogs format rpLogLimitBytes
      checkIfThereAreOldLogs  fullPathsToLogs format rpMaxAgeHours rpKeepFilesNum

fixLog
  :: FilePath
  -> LogFormat
  -> IO ()
fixLog oneFile format =
  isItSymLink format oneFile >>= \case
    True -> do
      -- It is a symlink, but corresponding log was deleted,
      -- whch means that symlink is already invalid.
      removeFile oneFile
      createLogAndSymLink (takeDirectory oneFile) format
    False ->
      when (isItLog format oneFile) $
        -- It is a single log, but its symlink was deleted.
        withCurrentDirectory (takeDirectory oneFile) $
          createFileLink (takeFileName oneFile) (symLinkName format)

checkIfCurrentLogIsFull
  :: [FilePath]
  -> LogFormat
  -> Word64
  -> IO ()
checkIfCurrentLogIsFull [] _ _ = return ()
checkIfCurrentLogIsFull logs format maxSizeInBytes =
  case find (\logPath -> takeFileName logPath == symLinkName format) logs of
    Just symLink ->
      doesSymLinkValid symLink >>= \case
        True ->
          whenM (isLogFull =<< getPathToLatestLog symLink) $
            createLogAndUpdateSymLink subDirForLogs format
        False ->
          -- Remove invalid symlink.
          removeFile symLink
    Nothing ->
      -- There is no symlink we need, so skip check for now:
      -- this symlink will be created when the new 'TraceObject's will be received.
      return ()
 where
  subDirForLogs = takeDirectory $ head logs -- All these logs are in the same subdir.

  getPathToLatestLog symlink = do
    logName <- getSymbolicLinkTarget symlink
    return $ subDirForLogs </> logName

  isLogFull logName = do
    sz <- getFileSize logName
    return $ fromIntegral sz >= maxSizeInBytes

checkIfThereAreOldLogs
  :: [FilePath]
  -> LogFormat
  -> Word
  -> Word
  -> IO ()
checkIfThereAreOldLogs [] _ _ _ = return ()
checkIfThereAreOldLogs logs format maxAgeInHours keepFilesNum = do
  now <- getCurrentTime
  let logsWeNeed = filter (isItLog format) logs
      -- Sort by name with timestamp, so the latest logs will always be in the end.
      oldLogs = sort . filter (oldLog now) $ logsWeNeed
      remainingLogsNum = length logsWeNeed - length oldLogs
  if remainingLogsNum >= fromIntegral keepFilesNum
    then mapM_ removeFile oldLogs
    else removeSomeOldLogs oldLogs remainingLogsNum
 where
  oldLog now' logName =
    case getTimeStampFromLog logName of
      Just timeStamp ->
        let logAge = now' `diffUTCTime` timeStamp
        in toSeconds logAge >= maxAgeInSecs
      Nothing -> False

  maxAgeInSecs = fromIntegral maxAgeInHours * 3600
  toSeconds age = fromEnum age `div` 1000000000000

  removeSomeOldLogs [] _ = return ()
  removeSomeOldLogs oldLogs remainingLogsNum = do
    -- Too many logs are old, so make sure we keep enough latest logs.
    let oldLogsNumToKeep = fromIntegral keepFilesNum - remainingLogsNum
    -- Reverse logs to place the latest ones in the beginning and drop
    -- 'oldLogsNumToKeep' to keep them.
    let oldLogsToRemove = drop oldLogsNumToKeep . reverse $ oldLogs
    -- If the total num of old logs is less than 'keepFilesNum', all
    -- of them will be kept.
    mapM_ removeFile oldLogsToRemove
