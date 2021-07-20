{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Tracer.Handlers.Metrics.Prometheus
  ( runPrometheusServer
  ) where

import           Prelude hiding (head)
import           Control.Monad (forM, forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import           Data.IORef (readIORef)
import           Data.List (find)
import qualified Data.Map.Strict as M
import           Data.String (IsString (..))
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Snap.Blaze (blaze)
import           Snap.Core (Snap, getRequest, route, rqParams, writeText)
import           Snap.Http.Server (Config, ConfigLog (..), defaultConfig, setAccessLog,
                                   setBind, setErrorLog, setPort, simpleHttpServe)
import           System.Metrics (Sample, Value (..), sampleAll)
import           Text.Blaze.Html
import           Text.Blaze.Html5 hiding (map)
import           Text.Blaze.Html5.Attributes hiding (title)

import           Trace.Forward.Protocol.Type (NodeInfoStore)

import           Cardano.Tracer.Configuration
import           Cardano.Tracer.Types

runPrometheusServer
  :: Endpoint
  -> AcceptedItems
  -> IO ()
runPrometheusServer (Endpoint host port) acceptedItems = forever $
  -- If everything is okay, the function 'simpleHttpServe' never returns.
  -- But if there is some problem, it never throws an exception, but just stops.
  -- So if it stopped - it will be re-started.
  simpleHttpServe config $
    route [ ("metrics", renderListOfNodes)
          , ("metrics/:nodefullid", renderMetricsFromNode)
          ]
 where
  config :: Config Snap ()
  config =
      setPort port
    . setBind (BSC.pack host)
    . setAccessLog ConfigNoLog
    . setErrorLog ConfigNoLog
    $ defaultConfig

  renderListOfNodes :: Snap ()
  renderListOfNodes =
    HM.toList <$> liftIO (readIORef acceptedItems) >>= \case
      []    -> writeText "There are no connected nodes yet."
      items -> blaze =<< liftIO (mkListOfHrefs items)

  mkListOfHrefs :: [(NodeId, (NodeInfoStore, TraceObjects, Metrics))] -> IO Html
  mkListOfHrefs items = do
    nodeHrefs <- forM items $ \(nodeId, (niStore, _, _)) -> do
      maybeName <- getNodeName niStore
      let nodeFullId =
            case maybeName of
              Nothing    -> show nodeId
              Just aName -> T.unpack aName <> "-" <> show nodeId
      return $ a ! href (mkURL nodeFullId) $ toHtml nodeFullId
    return $ mkPage nodeHrefs

  mkURL :: String -> AttributeValue
  mkURL nodeFullId = fromString $
    "http://" <> host <> ":" <> show port <> "/metrics/" <> nodeFullId

  mkPage :: [Html] -> Html
  mkPage hrefs = html $ do
    head $ title "Prometheus metrics"
    body $ ol $ mapM_ li hrefs

  renderMetricsFromNode :: Snap ()
  renderMetricsFromNode =
    getRequest >>= return . M.lookup "nodefullid" . rqParams >>= \case
      Nothing ->
        writeText "No such a node!"
      Just nodeFullId ->
        writeText =<< liftIO (getMetricsFromNode nodeFullId acceptedItems)

type MetricName  = Text
type MetricValue = Text
type MetricsList = [(MetricName, MetricValue)]

getMetricsFromNode
  :: [BS.ByteString]
  -> AcceptedItems
  -> IO Text
getMetricsFromNode [] _ = return "No such a node!"
getMetricsFromNode (nodeFullId':_) acceptedItems = do
  items <- readIORef acceptedItems
  if HM.null items
    then return "No such a node!"
    else do
      case find nodeIdWeNeed $ HM.keys items of
        Nothing -> return "No such a node!"
        Just nodeId -> do
          let (_, _, (ekgStore, _)) = items HM.! nodeId
          sampleAll ekgStore >>= return . renderListOfMetrics . getListOfMetrics
 where
  -- For example, "127.0.0.1-17890" is suffix of "node-1-127.0.0.1-17890"
  nodeIdWeNeed nodeId = T.pack (show nodeId) `T.isSuffixOf` nodeFullId
  nodeFullId = decodeUtf8 nodeFullId'

  getListOfMetrics :: Sample -> MetricsList
  getListOfMetrics = filter (not . T.null . fst) . map metricsWeNeed . HM.toList

  metricsWeNeed (mName, mValue) =
    case mValue of
      Counter c -> (mName, T.pack $ show c)
      Gauge g   -> (mName, T.pack $ show g)
      Label l   -> (mName, l)
      _         -> ("",    "") -- ekg-forward doesn't support 'Distribution' yet.

  renderListOfMetrics :: MetricsList -> Text
  renderListOfMetrics [] = "No metrics were received from this node."
  renderListOfMetrics mList = T.intercalate "\n" $
    map (\(mName, mValue) -> prepareName mName <> " " <> mValue) mList

  prepareName =
      T.filter (`elem` (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']))
    . T.replace " " "_"
    . T.replace "-" "_"
    . T.replace "." "_"
