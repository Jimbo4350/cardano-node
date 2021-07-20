{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Node.Run
  ( runNode
  , checkVRFFilePermissions
  ) where

import           Cardano.Prelude hiding (ByteString, atomically, take, trace, STM)
import           Prelude (String)
import           Data.IP (toSockAddr)

import qualified Control.Concurrent.Async as Async
import           Control.Monad.Trans.Except.Extra (left)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer
import qualified Data.Map.Strict as Map
import           Data.Text (breakOn, pack, take)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Data.Version (showVersion)
import           Network.HostName (getHostName)
import           Network.Socket (Socket)
import           System.Directory (canonicalizePath, createDirectoryIfMissing, makeAbsolute)
import           System.Environment (lookupEnv)
#ifdef UNIX
import           System.Posix.Files
import           System.Posix.Types (FileMode)
import qualified System.Posix.Signals as Signals
#else
import           System.Win32.File
#endif

#ifdef UNIX
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                     mkLOMeta, LOMeta)
#else
import           Cardano.BM.Data.LogItem (LOContent (..), LogObject (..), PrivacyAnnotation (..),
                     mkLOMeta)
#endif
import           Cardano.BM.Data.Tracer (ToLogObject (..), TracingVerbosity (..))
import           Cardano.BM.Data.Transformers (setHostname)
import           Cardano.BM.Trace
import           Paths_cardano_node (version)

import qualified Cardano.Crypto.Libsodium as Crypto

import           Cardano.Node.Configuration.Logging (LoggingLayer (..), Severity (..),
                   createLoggingLayer, nodeBasicInfo, shutdownLoggingLayer)
import           Cardano.Node.Configuration.POM (NodeConfiguration (..),
                   PartialNodeConfiguration (..), SomeNetworkP2PMode (..),
                   defaultPartialNodeConfiguration, makeNodeConfiguration,
                   parseNodeConfigurationFP)
import           Cardano.Node.Types
import           Cardano.Tracing.Config (TraceOptions (..), TraceSelection (..))
import           Cardano.Tracing.Constraints (TraceConstraints)
import           Cardano.Tracing.Metrics (HasKESMetricsData (..), HasKESInfo (..))

import qualified Ouroboros.Consensus.Config as Consensus
import           Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (..), getNetworkMagic)
import           Ouroboros.Consensus.Node (RunNode, RunNodeArgs (..)
                   , StdRunNodeArgs (..), NetworkP2PMode (..))
import qualified Ouroboros.Consensus.Node as Node (getChainDB, run)
import           Ouroboros.Consensus.Node.ProtocolInfo
import           Ouroboros.Consensus.Node.NetworkProtocolVersion
import           Ouroboros.Consensus.Util.Orphans ()
import           Ouroboros.Network.Subscription
                   ( DnsSubscriptionTarget (..)
                   , IPSubscriptionTarget (..)
                   )
import qualified Ouroboros.Network.Diffusion as Diffusion
import qualified Ouroboros.Network.Diffusion.P2P as P2P
import qualified Ouroboros.Network.Diffusion.NonP2P as NonP2P
import           Ouroboros.Network.Magic (NetworkMagic (..))
import           Ouroboros.Network.NodeToClient (LocalAddress (..), LocalSocket (..))
import           Ouroboros.Network.NodeToNode (RemoteAddress, AcceptedConnectionsLimit (..),
                   PeerSelectionTargets (..))
import           Ouroboros.Network.PeerSelection.LedgerPeers (UseLedgerAfter (..), RelayAddress (..))

import qualified Cardano.Api.Protocol.Types as Protocol
import           Cardano.Node.Configuration.Socket (SocketOrSocketInfo (..),
                     gatherConfiguredSockets, getSocketOrSocketInfoAddr, renderSocketConfigError)
import qualified Cardano.Node.Configuration.TopologyP2P as TopologyP2P
import           Cardano.Node.Configuration.TopologyP2P
import qualified Cardano.Node.Configuration.Topology as TopologyNonP2P
import           Cardano.Node.Handlers.Shutdown
import           Cardano.Node.Protocol (mkConsensusProtocol, renderProtocolInstantiationError)
import           Cardano.Node.Protocol.Types
import           Cardano.Tracing.Kernel
import           Cardano.Tracing.Peer
import           Cardano.Tracing.Tracers

{- HLINT ignore "Use fewer imports" -}

runNode
  :: PartialNodeConfiguration
  -> IO ()
runNode cmdPc = do
    -- TODO: Remove sodiumInit: https://github.com/input-output-hk/cardano-base/issues/175
    Crypto.sodiumInit

    configYamlPc <- parseNodeConfigurationFP . getLast $ pncConfigFile cmdPc

    nc <- case makeNodeConfiguration $ defaultPartialNodeConfiguration <> configYamlPc <> cmdPc of
            Left err -> panic $ "Error in creating the NodeConfiguration: " <> Text.pack err
            Right nc' -> return nc'

    case shelleyVRFFile $ ncProtocolFiles nc of
      Just vrfFp -> do vrf <- runExceptT $ checkVRFFilePermissions vrfFp
                       case vrf of
                         Left err ->
                           putTextLn (renderVRFPrivateKeyFilePermissionError err) >> exitFailure
                         Right () ->
                           pure ()
      Nothing -> pure ()

    eitherSomeProtocol <- runExceptT $ mkConsensusProtocol nc

    p :: SomeConsensusProtocol <-
      case eitherSomeProtocol of
        Left err -> putTextLn (renderProtocolInstantiationError err) >> exitFailure
        Right p -> pure p

    eLoggingLayer <- runExceptT $ createLoggingLayer
                     (Text.pack (showVersion version))
                     nc
                     p

    loggingLayer <- case eLoggingLayer of
                      Left err  -> putTextLn (show err) >> exitFailure
                      Right res -> return res

    !trace <- setupTrace loggingLayer
    let tracer = contramap pack $ toLogObject trace

    logTracingVerbosity nc tracer

    let handleNodeWithTracers
          :: ( HasKESMetricsData blk
             , HasKESInfo blk
             , TraceConstraints blk
             , Protocol.Protocol IO blk
             )
          => Protocol.ProtocolInfoArgs IO blk
          -> IO ()
        handleNodeWithTracers runP = do
          -- This IORef contains node kernel structure which holds node kernel.
          -- Used for ledger queries and peer connection status.
          nodeKernelData <- mkNodeKernelData
          let ProtocolInfo { pInfoConfig = cfg } = Protocol.protocolInfo runP
          case ncEnableP2P nc of
            SomeNetworkP2PMode p2pMode -> do
              tracers <- mkTracers
                          (Consensus.configBlock cfg)
                          (ncTraceConfig nc)
                          trace
                          nodeKernelData
                          (llEKGDirect loggingLayer)
                          p2pMode
              Async.withAsync (handlePeersListSimple trace nodeKernelData)
                  $ \_peerLogingThread ->
                    -- We ignore peer loging thread if it dies, but it will be killed
                    -- when 'handleSimpleNode' terminates.
                        handleSimpleNode p runP p2pMode trace tracers nc
                                        (setNodeKernel nodeKernelData)
                        `finally`
                        shutdownLoggingLayer loggingLayer

    case p of
      SomeConsensusProtocol _ runP -> handleNodeWithTracers runP

logTracingVerbosity :: NodeConfiguration -> Tracer IO String -> IO ()
logTracingVerbosity nc tracer =
  case ncTraceConfig nc of
    TracingOff -> return ()
    TracingOn traceConf ->
      case traceVerbosity traceConf of
        NormalVerbosity -> traceWith tracer "tracing verbosity = normal verbosity "
        MinimalVerbosity -> traceWith tracer "tracing verbosity = minimal verbosity "
        MaximalVerbosity -> traceWith tracer "tracing verbosity = maximal verbosity "

-- | Add the application name and unqualified hostname to the logging
-- layer basic trace.
--
-- If the @CARDANO_NODE_LOGGING_HOSTNAME@ environment variable is set,
-- it overrides the system hostname. This is useful when running a
-- local test cluster with all nodes on the same host.
setupTrace
  :: LoggingLayer
  -> IO (Trace IO Text)
setupTrace loggingLayer = do
    hn <- maybe hostname (pure . pack) =<< lookupEnv "CARDANO_NODE_LOGGING_HOSTNAME"
    return $
        setHostname hn $
        llAppendName loggingLayer "node" (llBasicTrace loggingLayer)
  where
    hostname = do
      hn0 <- pack <$> getHostName
      return $ take 8 $ fst $ breakOn "." hn0

handlePeersListSimple
  :: Trace IO Text
  -> NodeKernelData blk
  -> IO ()
handlePeersListSimple tr nodeKern = forever $ do
  getCurrentPeers nodeKern >>= tracePeers tr
  threadDelay 2000000 -- 2 seconds.


-- | Sets up a simple node, which will run the chain sync protocol and block
-- fetch protocol, and, if core, will also look at the mempool when trying to
-- create a new block.
--
handleSimpleNode
  :: forall blk p2p
  . ( RunNode blk
    , Protocol.Protocol IO blk
    )
  => SomeConsensusProtocol
  -> Protocol.ProtocolInfoArgs IO blk
  -> NetworkP2PMode p2p
  -> Trace IO Text
  -> Tracers RemoteAddress LocalAddress blk p2p
  -> NodeConfiguration
  -> (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO ())
  -- ^ Called on the 'NodeKernel' after creating it, but before the network
  -- layer is initialised.  This implies this function must not block,
  -- otherwise the node won't actually start.
  -> IO ()
handleSimpleNode scp runP p2pMode trace nodeTracers nc onKernel = do
  meta <- mkLOMeta Notice Public

  traceNamedObject
          (appendName "p2p-mode" trace)
          (meta, LogMessage (Text.pack "Enabled"))

  let pInfo = Protocol.protocolInfo runP
      tracer = toLogObject trace

  createTracers nc trace tracer

  (publicIPv4SocketOrAddr, publicIPv6SocketOrAddr, localSocketOrPath) <- do
    result <- runExceptT (gatherConfiguredSockets nc)
    case result of
      Right triplet -> return triplet
      Left error -> do
        traceNamedObject
          (appendName "error" trace)
          (meta, LogMessage (Text.pack (renderSocketConfigError error)))
        throwIO error

  dbPath <- canonDbPath nc

  let diffusionArguments :: Diffusion.Arguments Socket      RemoteAddress
                                                LocalSocket LocalAddress
      diffusionArguments =
        Diffusion.Arguments {
            Diffusion.daIPv4Address  =
              case publicIPv4SocketOrAddr of
                Just (ActualSocket socket) -> Just (Left socket)
                Just (SocketInfo addr)     -> Just (Right addr)
                Nothing                    -> Nothing
          , Diffusion.daIPv6Address  =
              case publicIPv6SocketOrAddr of
                Just (ActualSocket socket) -> Just (Left socket)
                Just (SocketInfo addr)     -> Just (Right addr)
                Nothing                    -> Nothing
          , Diffusion.daLocalAddress =
              case localSocketOrPath of  -- TODO allow expressing the Nothing case in the config
                Just (ActualSocket socket)          -> Just (Left $ LocalSocket socket)
                Just (SocketInfo (SocketPath path)) -> Just (Right $ LocalAddress path)
                Nothing                             -> Nothing
          , Diffusion.daAcceptedConnectionsLimit =
              AcceptedConnectionsLimit
                { acceptedConnectionsHardLimit = 512
                , acceptedConnectionsSoftLimit = 384
                , acceptedConnectionsDelay     = 5
                }
          , Diffusion.daDiffusionMode = ncDiffusionMode nc
          }

  ipv4 <- traverse getSocketOrSocketInfoAddr publicIPv4SocketOrAddr
  ipv6 <- traverse getSocketOrSocketInfoAddr publicIPv6SocketOrAddr

  traceNamedObject
    (appendName "addresses" trace)
    (meta, LogMessage . Text.pack . show $ catMaybes [ipv4, ipv6])
  traceNamedObject
    (appendName "diffusion-mode" trace)
    (meta, LogMessage . Text.pack . show . ncDiffusionMode $ nc)
  traceNamedObject
    (appendName "local-socket" trace)
    (meta, LogMessage . Text.pack . show $ localSocketOrPath)
  traceNamedObject
    (appendName "node-to-node-versions" trace)
    (meta, LogMessage . Text.pack . show . supportedNodeToNodeVersions $ Proxy @blk)
  traceNamedObject
    (appendName "node-to-client-versions" trace)
    (meta, LogMessage . Text.pack . show . supportedNodeToClientVersions $ Proxy @blk)


  withShutdownHandling nc trace $ \sfds ->
    let nodeArgs = RunNodeArgs
          { rnTraceConsensus = consensusTracers nodeTracers
          , rnTraceNTN       = nodeToNodeTracers nodeTracers
          , rnTraceNTC       = nodeToClientTracers nodeTracers
          , rnProtocolInfo   = pInfo
          , rnNodeKernelHook = \registry nodeKernel -> do
              maybeSpawnOnSlotSyncedShutdownHandler nc sfds trace registry
                (Node.getChainDB nodeKernel)
              onKernel nodeKernel
          , rnEnableP2P      = p2pMode
          }
    in case p2pMode of
      EnabledP2PMode -> do
        nt <- TopologyP2P.readTopologyFileOrError nc
        let (localRoots, publicRoots) = producerAddresses nt
        traceNamedObject
                (appendName "topology-file" trace)
                (meta, LogMessage (Text.pack "Successfully read topology configuration file"))
        traceNamedObject
          (appendName "local-roots" trace)
          (meta, LogMessage . Text.pack . show $ localRoots)
        traceNamedObject
          (appendName "public-roots" trace)
          (meta, LogMessage . Text.pack . show $ publicRoots)
        traceNamedObject
          (appendName "use-ledger-after-slot" trace)
          (meta, LogMessage . Text.pack . show $ useLedgerAfterSlot nt)
        (localRootsVar :: StrictTVar IO [(Int, Map RelayAddress PeerAdvertise)])  <- newTVarIO localRoots
        publicRootsVar <- newTVarIO publicRoots
        useLedgerVar   <- newTVarIO (useLedgerAfterSlot nt)
#ifdef UNIX
        _ <- Signals.installHandler
              Signals.sigHUP
              (updateVars meta localRootsVar publicRootsVar useLedgerVar)
              Nothing
        traceNamedObject
                (appendName "signal-handler" trace)
                (meta, LogMessage (Text.pack "Installed signal handler"))
#endif
        void $
          Node.run
            nodeArgs
            StdRunNodeArgs
              { srnBfcMaxConcurrencyBulkSync   = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
              , srnBfcMaxConcurrencyDeadline   = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
              , srnChainDbValidateOverride     = ncValidateDB nc
              , srnSnapshotInterval            = ncSnapshotInterval nc
              , srnDatabasePath                = dbPath
              , srnDiffusionArguments          = diffusionArguments
              , srnDiffusionArgumentsExtra     = mkP2PArguments nc (readTVar localRootsVar)
              (readTVar publicRootsVar)
              (readTVar useLedgerVar)
              , srnDiffusionTracers            = diffusionTracers nodeTracers
              , srnDiffusionTracersExtra       = diffusionTracersExtra nodeTracers
              , srnEnableInDevelopmentVersions = False -- TODO get this value from the node configuration
              , srnTraceChainDB                = chainDBTracer nodeTracers
              }
      DisabledP2PMode -> do
        eitherTopology <- TopologyNonP2P.readTopologyFile nc
        nt <- either (\err -> panic $ "Cardano.Node.Run.handleSimpleNodeNonP2P.readTopologyFile: " <> err) pure eitherTopology
        let (ipProducerAddrs, dnsProducerAddrs) = producerAddressesNonP2P nt

            dnsProducers :: [DnsSubscriptionTarget]
            dnsProducers = [ DnsSubscriptionTarget (Text.encodeUtf8 addr) port v
                           | (NodeAddress (NodeHostDnsAddress addr) port, v) <- dnsProducerAddrs
                           ]

            ipProducers :: IPSubscriptionTarget
            ipProducers = IPSubscriptionTarget
                           [ toSockAddr (addr, port)
                           | (NodeAddress (NodeHostIPAddress addr) port) <- ipProducerAddrs
                           ]
                           (length ipProducerAddrs)
        void $
          Node.run
            nodeArgs
            StdRunNodeArgs
              { srnBfcMaxConcurrencyBulkSync   = unMaxConcurrencyBulkSync <$> ncMaxConcurrencyBulkSync nc
              , srnBfcMaxConcurrencyDeadline   = unMaxConcurrencyDeadline <$> ncMaxConcurrencyDeadline nc
              , srnChainDbValidateOverride     = ncValidateDB nc
              , srnSnapshotInterval            = ncSnapshotInterval nc
              , srnDatabasePath                = dbPath
              , srnDiffusionArguments          = diffusionArguments
              , srnDiffusionArgumentsExtra     = mkNonP2PArguments ipProducers dnsProducers
              , srnDiffusionTracers            = diffusionTracers nodeTracers
              , srnDiffusionTracersExtra       = diffusionTracersExtra nodeTracers
              , srnEnableInDevelopmentVersions = False -- TODO get this value from the node configuration
              , srnTraceChainDB                = chainDBTracer nodeTracers
              }
 where
  createTracers
    :: NodeConfiguration
    -> Trace IO Text
    -> Tracer IO Text
    -> IO ()
  createTracers NodeConfiguration { ncValidateDB }
                tr tracer = do
    let ProtocolInfo{ pInfoConfig = cfg } = Protocol.protocolInfo runP

    meta <- mkLOMeta Notice Public
    traceNamedObject (appendName "networkMagic" tr)
                     (meta, LogMessage ("NetworkMagic " <> show (unNetworkMagic . getNetworkMagic $ Consensus.configBlock cfg)))

    startTime <- getCurrentTime
    traceNodeBasicInfo tr =<< nodeBasicInfo nc scp startTime
    traceCounter "nodeStartTime" tr (ceiling $ utcTimeToPOSIXSeconds startTime)

    when ncValidateDB $ traceWith tracer "Performing DB validation"

  traceNodeBasicInfo :: Trace IO Text -> [LogObject Text] -> IO ()
  traceNodeBasicInfo tr basicInfoItems =
    forM_ basicInfoItems $ \(LogObject nm mt content) ->
      traceNamedObject (appendName nm tr) (mt, content)

#ifdef UNIX
  updateVars :: LOMeta
             -> StrictTVar IO [(Int, Map RelayAddress PeerAdvertise)]
             -> StrictTVar IO [RelayAddress]
             -> StrictTVar IO UseLedgerAfter
             -> Signals.Handler
  updateVars meta localRootsVar publicRootsVar useLedgerVar =
    Signals.Catch $ do
      traceNamedObject
          (appendName "signal-handler" trace)
          (meta, LogMessage (Text.pack "SIGHUP signal received - Performing topology configuration update"))

      result <- try $ readTopologyFileOrError nc

      case result of
        Left (FatalError err) ->
          traceNamedObject
              (appendName "topology-file" trace)
              (meta, LogMessage (Text.pack "Error reading topology configuration file:" <> err))
        Right nt -> do
          traceNamedObject
              (appendName "topology-file" trace)
              (meta, LogMessage (Text.pack "Successfully read topology configuration file"))

          let (localRoots, publicRoots) = producerAddresses nt

          atomically $ do
            writeTVar localRootsVar localRoots
            writeTVar publicRootsVar publicRoots
            writeTVar useLedgerVar (useLedgerAfterSlot nt)

          traceNamedObject
            (appendName "local-roots" trace)
            (meta, LogMessage . Text.pack . show $ localRoots)
          traceNamedObject
            (appendName "public-roots" trace)
            (meta, LogMessage . Text.pack . show $ publicRoots)
          traceNamedObject
            (appendName "use-ledger-after-slot" trace)
            (meta, LogMessage . Text.pack . show $ useLedgerAfterSlot nt)

{-# ANN handleSimpleNode ("HLint: ignore Reduce duplication" :: Text) #-}
#endif

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

canonDbPath :: NodeConfiguration -> IO FilePath
canonDbPath NodeConfiguration{ncDatabaseFile = DbFile dbFp} = do
  fp <- canonicalizePath =<< makeAbsolute dbFp
  createDirectoryIfMissing True fp
  return fp


-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVRFFilePermissions :: FilePath -> ExceptT VRFPrivateKeyFilePermissionError IO ()
#ifdef UNIX
checkVRFFilePermissions vrfPrivKey = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the the VRF private key file does not give read/write/exec permissions to others.
  when (hasOtherPermissions fm)
       (left $ OtherPermissionsExist vrfPrivKey)
  -- Check the the VRF private key file does not give read/write/exec permissions to any group.
  when (hasGroupPermissions fm)
       (left $ GroupPermissionsExist vrfPrivKey)
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#else
checkVRFFilePermissions vrfPrivKey = do
  attribs <- liftIO $ getFileAttributes vrfPrivKey
  -- https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
  -- https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/standard-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/generic-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/access-mask
  when (attribs `hasPermission` genericPermissions)
       (left $ GenericPermissionsExist vrfPrivKey)
 where
  genericPermissions = gENERIC_ALL .|. gENERIC_READ .|. gENERIC_WRITE .|. gENERIC_EXECUTE
  hasPermission fModeA fModeB = fModeA .&. fModeB /= gENERIC_NONE
#endif


mkP2PArguments
  :: NodeConfiguration
  -> STM IO [(Int, Map RelayAddress PeerAdvertise)]
  -> STM IO [RelayAddress]
  -> STM IO UseLedgerAfter
  -> Diffusion.ExtraArguments 'Diffusion.P2P IO
mkP2PArguments NodeConfiguration {
                 ncTargetNumberOfRootPeers,
                 ncTargetNumberOfKnownPeers,
                 ncTargetNumberOfEstablishedPeers,
                 ncTargetNumberOfActivePeers,
                 ncProtocolIdleTimeout,
                 ncTimeWaitTimeout
               }
               daReadLocalRootPeers
               daReadPublicRootPeers
               daReadUseLedgerAfter =
    Diffusion.P2PArguments P2P.ArgumentsExtra
      { P2P.daPeerSelectionTargets
      , P2P.daReadLocalRootPeers
      , P2P.daReadPublicRootPeers
      , P2P.daReadUseLedgerAfter
      , P2P.daProtocolIdleTimeout  = ncProtocolIdleTimeout
      , P2P.daTimeWaitTimeout      = ncTimeWaitTimeout
      }
  where
    -- TODO: this should be configurable; the following gives something similar
    -- to the current node setup for pool operators.  It's rather conservative,
    -- just for start.
    daPeerSelectionTargets = PeerSelectionTargets {
        targetNumberOfRootPeers        = ncTargetNumberOfRootPeers,
        targetNumberOfKnownPeers       = ncTargetNumberOfKnownPeers,
        targetNumberOfEstablishedPeers = ncTargetNumberOfEstablishedPeers,
        targetNumberOfActivePeers      = ncTargetNumberOfActivePeers
    }

mkNonP2PArguments
  :: IPSubscriptionTarget
  -> [DnsSubscriptionTarget]
  -> Diffusion.ExtraArguments 'Diffusion.NonP2P m
mkNonP2PArguments daIpProducers daDnsProducers =
    Diffusion.NonP2PArguments NonP2P.ArgumentsExtra
      { NonP2P.daIpProducers
      , NonP2P.daDnsProducers
      }

-- | TODO: Only needed for enabling P2P switch
--
producerAddressesNonP2P
  :: TopologyNonP2P.NetworkTopology
  -> ( [NodeIPAddress]
     , [(NodeDnsAddress, Int)])
producerAddressesNonP2P nt =
  case nt of
    TopologyNonP2P.RealNodeTopology producers' ->
        partitionEithers
      . mapMaybe TopologyNonP2P.remoteAddressToNodeAddress
      $ producers'
    TopologyNonP2P.MockNodeTopology nodeSetup ->
        partitionEithers
      . mapMaybe TopologyNonP2P.remoteAddressToNodeAddress
      . concatMap TopologyNonP2P.producers
      $ nodeSetup

producerAddresses
  :: NetworkTopology
  -> ([(Int, Map RelayAddress PeerAdvertise)], [RelayAddress])
producerAddresses nt =
  case nt of
    RealNodeTopology lrpg prp _ ->
      ( map (\lrp -> ( valency lrp
                     , Map.fromList $ rootAddressToRelayAddress
                                    $ localRoots lrp
                     )
            )
            (groups lrpg)
      , concatMap (map fst . rootAddressToRelayAddress)
                  (map publicRoots prp)
      )

useLedgerAfterSlot
  :: NetworkTopology
  -> UseLedgerAfter
useLedgerAfterSlot (RealNodeTopology _ _ (UseLedger ul)) = ul
