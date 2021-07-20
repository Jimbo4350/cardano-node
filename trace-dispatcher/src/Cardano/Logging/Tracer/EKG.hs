{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Logging.Tracer.EKG (
  ekgTracer
) where

import           Cardano.Logging.DocuGenerator
import           Cardano.Logging.Types

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import           Data.Text (intercalate, pack)
import qualified System.Metrics as Metrics
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Label as Label
import           System.Remote.Monitoring (Server, getCounter, getGauge,
                     getLabel)


ekgTracer :: MonadIO m => Either Metrics.Store Server-> m (Trace m FormattedMessage)
ekgTracer storeOrServer = liftIO $ do
    rgsGauges   <- newIORef Map.empty
    rgsLabels   <- newIORef Map.empty
    rgsCounters <- newIORef Map.empty
    pure $ Trace $ T.arrow $ T.emit $
      output rgsGauges rgsLabels rgsCounters
  where
    output rgsGauges rgsLabels rgsCounters
      (LoggingContext{..}, Nothing, FormattedMetrics m) =
        liftIO $ mapM_
          (setIt rgsGauges rgsLabels rgsCounters lcNamespace) m
    output _ _ _ p@(_, Just Document {}, FormattedMetrics m) =
      docIt EKGBackend (FormattedMetrics m) p
    output _ _ _ (LoggingContext{}, Just _c, _v) =
      pure ()

    setIt rgsGauges _rgsLabels _rgsCounters _namespace
      (IntM ns theInt) = do
        rgsMap <- readIORef rgsGauges
        let name = intercalate "." ns
        case Map.lookup name rgsMap of
          Just gauge -> Gauge.set gauge (fromIntegral theInt)
          Nothing -> do
            gauge <- case storeOrServer of
                        Left store   -> Metrics.createGauge name store
                        Right server -> getGauge name server
            let rgsGauges' = Map.insert name gauge rgsMap
            writeIORef rgsGauges rgsGauges'
            Gauge.set gauge (fromIntegral theInt)
    setIt _rgsGauges rgsLabels _rgsCounters _namespace
      (DoubleM ns theDouble) = do
        rgsMap <- readIORef rgsLabels
        let name = intercalate "." ns
        case Map.lookup name rgsMap of
          Just label -> Label.set label ((pack . show) theDouble)
          Nothing -> do
            label <- case storeOrServer of
                        Left store   -> Metrics.createLabel name store
                        Right server -> getLabel name server
            let rgsLabels' = Map.insert name label rgsMap
            writeIORef rgsLabels rgsLabels'
            Label.set label ((pack . show) theDouble)
    setIt _rgsGauges _rgsLabels rgsCounters _namespace
      (CounterM ns mbInt) = do
        rgsMap <- readIORef rgsCounters
        let name = intercalate "." ns
        case Map.lookup name rgsMap of
          Just counter -> case mbInt of
                            Nothing -> Counter.inc counter
                            Just i  -> Counter.add counter (fromIntegral i)
          Nothing -> do
            counter <- case storeOrServer of
                        Left store   -> Metrics.createCounter name store
                        Right server -> getCounter name server
            let rgsCounters' = Map.insert name counter rgsMap
            writeIORef rgsCounters rgsCounters'
            case mbInt of
              Nothing -> Counter.inc counter
              Just i  -> Counter.add counter (fromIntegral i)
