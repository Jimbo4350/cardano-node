{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.CLI.Shelley.Run.Query
  ( ShelleyQueryCmdError
  , renderShelleyQueryCmdError
  , runQueryCmd
  , percentage
  ) where

import           Cardano.Api
import           Cardano.Api.Byron
import           Cardano.Api.Shelley
import           Cardano.Binary (decodeFull)
import           Cardano.CLI.Environment (EnvSocketError, readEnvSocketPath, renderEnvSocketError)
import           Cardano.CLI.Helpers (HelpersError (..), pPrintCBOR, renderHelpersError, hushM)
import           Cardano.CLI.Mary.RenderValue (defaultRenderValueOptions, renderValue)
import           Cardano.CLI.Shelley.Orphans ()
import           Cardano.CLI.Shelley.Parsers (OutputFile (..), QueryCmd (..))
import           Cardano.CLI.Types
import           Cardano.Crypto.Hash (hashToBytesAsHex)
import           Cardano.Ledger.Coin
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (KeyHash (..), KeyRole (..))
import           Cardano.Prelude hiding (atomically)
import           Control.Concurrent.STM
import           Control.Monad.Trans.Except (except)
import           Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistMaybe, left)
import           Data.Aeson (ToJSON (..), (.=))
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.List (nub)
import           Data.Time.Clock
import           Numeric (showEFloat)
import           Ouroboros.Consensus.BlockchainTime.WallClock.Types (RelativeTime (..), SystemStart (..), toRelativeTime)
import           Ouroboros.Consensus.Cardano.Block as Consensus (EraMismatch (..))
import           Ouroboros.Network.Block (Serialised (..))
import           Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure (..))
import           Prelude (String, id)
import           Shelley.Spec.Ledger.EpochBoundary
import           Shelley.Spec.Ledger.LedgerState hiding (_delegations)
import           Shelley.Spec.Ledger.Scripts ()
import           Text.Printf (printf)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Cardano.CLI.Shelley.Output as O
import qualified Cardano.Ledger.Crypto as Crypto
import qualified Cardano.Ledger.Era as Era
import qualified Cardano.Ledger.Shelley.Constraints as Ledger
import qualified Data.Text.IO as T
import qualified Ouroboros.Consensus.HardFork.History.Qry as Qry
import qualified Ouroboros.Network.Protocol.ChainSync.Client as Net.Sync
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as LocalStateQuery
import qualified Shelley.Spec.Ledger.API.Protocol as Ledger
import qualified System.IO as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use let" -}

data ShelleyQueryCmdError
  = ShelleyQueryCmdEnvVarSocketErr !EnvSocketError
  | ShelleyQueryCmdLocalStateQueryError !ShelleyQueryCmdLocalStateQueryError
  | ShelleyQueryCmdWriteFileError !(FileError ())
  | ShelleyQueryCmdHelpersError !HelpersError
  | ShelleyQueryCmdAcquireFailure !AcquireFailure
  | ShelleyQueryCmdEraConsensusModeMismatch !AnyConsensusMode !AnyCardanoEra
  | ShelleyQueryCmdByronEra
  | ShelleyQueryCmdPoolIdError (Hash StakePoolKey)
  | ShelleyQueryCmdEraMismatch !EraMismatch
  | ShelleyQueryCmdUnsupportedMode !AnyConsensusMode
  | ShelleyQueryCmdPastHorizon !Qry.PastHorizonException
  | ShelleyQueryCmdSystemStartUnavailable
  deriving Show

renderShelleyQueryCmdError :: ShelleyQueryCmdError -> Text
renderShelleyQueryCmdError err =
  case err of
    ShelleyQueryCmdEnvVarSocketErr envSockErr -> renderEnvSocketError envSockErr
    ShelleyQueryCmdLocalStateQueryError lsqErr -> renderLocalStateQueryError lsqErr
    ShelleyQueryCmdWriteFileError fileErr -> Text.pack (displayError fileErr)
    ShelleyQueryCmdHelpersError helpersErr -> renderHelpersError helpersErr
    ShelleyQueryCmdAcquireFailure aqFail -> Text.pack $ show aqFail
    ShelleyQueryCmdByronEra -> "This query cannot be used for the Byron era"
    ShelleyQueryCmdPoolIdError poolId -> "The pool id does not exist: " <> show poolId
    ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) (AnyCardanoEra era) ->
      "Consensus mode and era mismatch. Consensus mode: " <> show cMode <>
      " Era: " <> show era
    ShelleyQueryCmdEraMismatch (EraMismatch ledgerEra queryEra) ->
      "\nAn error mismatch occured." <> "\nSpecified query era: " <> queryEra <>
      "\nCurrent ledger era: " <> ledgerEra
    ShelleyQueryCmdUnsupportedMode mode -> "Unsupported mode: " <> renderMode mode
    ShelleyQueryCmdPastHorizon e -> "Past horizon: " <> show e
    ShelleyQueryCmdSystemStartUnavailable -> "System start unavailable"

runQueryCmd :: QueryCmd -> ExceptT ShelleyQueryCmdError IO ()
runQueryCmd cmd =
  case cmd of
    QueryProtocolParameters' consensusModeParams network mOutFile ->
      runQueryProtocolParameters consensusModeParams network mOutFile
    QueryTip consensusModeParams network mOutFile ->
      runQueryTip consensusModeParams network mOutFile
    QueryStakeDistribution' consensusModeParams network mOutFile ->
      runQueryStakeDistribution consensusModeParams network mOutFile
    QueryStakeAddressInfo consensusModeParams addr network mOutFile ->
      runQueryStakeAddressInfo consensusModeParams addr network mOutFile
    QueryDebugLedgerState' consensusModeParams network mOutFile ->
      runQueryLedgerState consensusModeParams network mOutFile
    QueryStakeSnapshot' consensusModeParams network poolid ->
      runQueryStakeSnapshot consensusModeParams network poolid
    QueryPoolParams' consensusModeParams network poolid ->
      runQueryPoolParams consensusModeParams network poolid
    QueryProtocolState' consensusModeParams network mOutFile ->
      runQueryProtocolState consensusModeParams network mOutFile
    QueryUTxO' consensusModeParams qFilter networkId mOutFile ->
      runQueryUTxO consensusModeParams qFilter networkId mOutFile

runQueryProtocolParameters
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolParameters (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr
                           readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    $ QueryInShelleyBasedEra sbe QueryProtocolParameters
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeProtocolParameters mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE
 where
  writeProtocolParameters
    :: Maybe OutputFile
    -> ProtocolParameters
    -> ExceptT ShelleyQueryCmdError IO ()
  writeProtocolParameters mOutFile' pparams =
    case mOutFile' of
      Nothing -> liftIO $ LBS.putStrLn (encodePretty pparams)
      Just (OutputFile fpath) ->
        handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath) $
          LBS.writeFile fpath (encodePretty pparams)

-- | Calculate the percentage sync rendered as text.
percentage
  :: RelativeTime
  -- ^ 'tolerance'.  If 'b' - 'a' < 'tolerance', then 100% is reported.  This even if we are 'tolerance' seconds
  -- behind, we are still considered fully synced.
  -> RelativeTime
  -- ^ 'nowTime'.  The time of the most recently synced block.
  -> RelativeTime
  -- ^ 'tipTime'.  The time of the tip of the block chain to which we need to sync.
  -> Text
percentage tolerance a b = Text.pack (printf "%.2f" pc)
  where -- All calculations are in seconds (Integer)
        t  = relativeTimeSeconds tolerance
        -- Plus 1 to prevent division by zero.  The 's' prefix stands for strictly-positive.
        sa = relativeTimeSeconds a + 1
        sb = relativeTimeSeconds b + 1
        -- Fast forward the 'nowTime` by the tolerance, but don't let the result exceed the tip time.
        ua = min (sa + t) sb
        ub = sb
        -- Final percentage to render as text.
        pc = id @Double (fromIntegral ua / fromIntegral  ub) * 100.0

relativeTimeSeconds :: RelativeTime -> Integer
relativeTimeSeconds (RelativeTime dt) = floor (nominalDiffTimeToSeconds dt)

runQueryTip
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryTip (AnyConsensusModeParams cModeParams) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath

  case consensusModeOnly cModeParams of
    CardanoMode -> do
      let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

      (chainTip, emLocalState) <- liftIO $ queryQueryTip localNodeConnInfo Nothing

      mLocalState <- fmap join . hushM emLocalState $ \e -> do
        liftIO . T.hPutStrLn IO.stderr $
          "Warning: Local state unavailable: " <> renderShelleyQueryCmdError (ShelleyQueryCmdAcquireFailure e)

      let tipSlotNo = case chainTip of
            ChainTipAtGenesis -> 0
            ChainTip slotNo _ _ -> slotNo

      mLocalStateOutput :: Maybe O.QueryTipLocalStateOutput <- fmap join . forM mLocalState $ \localState -> do
        case slotToEpoch tipSlotNo (O.eraHistory localState) of
          Left e -> do
            liftIO . T.hPutStrLn IO.stderr $
              "Warning: Epoch unavailable: " <> renderShelleyQueryCmdError (ShelleyQueryCmdPastHorizon e)
            return Nothing
          Right (epochNo, _, _) -> do
            syncProgressResult <- runExceptT $ do
              systemStart <- fmap getSystemStart (O.mSystemStart localState) & hoistMaybe ShelleyQueryCmdSystemStartUnavailable
              nowSeconds <- toRelativeTime (SystemStart systemStart) <$> liftIO getCurrentTime
              tipTimeResult <- getProgress tipSlotNo (O.eraHistory localState) & bimap ShelleyQueryCmdPastHorizon fst & except

              let tolerance = RelativeTime (secondsToNominalDiffTime 600)

              return $ flip (percentage tolerance) nowSeconds tipTimeResult

            mSyncProgress <- hushM syncProgressResult $ \e -> do
              liftIO . T.hPutStrLn IO.stderr $ "Warning: Sync progress unavailable: " <> renderShelleyQueryCmdError e
            
            return $ Just $ O.QueryTipLocalStateOutput
              { O.mEra = Just (O.era localState)
              , O.mEpoch = Just epochNo
              , O.mSyncProgress = mSyncProgress
              }

      let jsonOutput = encodePretty $ O.QueryTipOutput
            { O.chainTip = chainTip
            , O.mLocalState = mLocalStateOutput
            }

      case mOutFile of
        Just (OutputFile fpath) -> liftIO $ LBS.writeFile fpath jsonOutput
        Nothing                 -> liftIO $ LBS.putStrLn        jsonOutput

    mode -> left (ShelleyQueryCmdUnsupportedMode (AnyConsensusMode mode))

queryQueryTip
  :: LocalNodeConnectInfo CardanoMode
  -> Maybe ChainPoint
  -> IO (ChainTip, Either AcquireFailure (Maybe O.QueryTipLocalState))
queryQueryTip connectInfo mpoint = do
  resultVarQueryTipLocalState <- newEmptyTMVarIO
  resultVarChainTip <- newEmptyTMVarIO

  waitResult <- pure $ (,)
    <$> readTMVar resultVarChainTip
    <*> (sequence <$> readTMVar resultVarQueryTipLocalState)

  connectToLocalNodeWithVersion
    connectInfo
    (\ntcVersion ->
      LocalNodeClientProtocols
      { localChainSyncClient    = LocalChainSyncClient $ chainSyncGetCurrentTip waitResult resultVarChainTip
      , localStateQueryClient   = Just $ setupLocalStateQueryScript waitResult mpoint ntcVersion resultVarQueryTipLocalState $ do
          era <- sendMsgQuery (QueryCurrentEra CardanoModeIsMultiEra)
          eraHistory <- sendMsgQuery (QueryEraHistory CardanoModeIsMultiEra)
          mSystemStart <- if ntcVersion >= NodeToClientV_9
            then Just <$> sendMsgQuery QuerySystemStart
            else return Nothing
          return O.QueryTipLocalState
            { O.era = era
            , O.eraHistory = eraHistory
            , O.mSystemStart = mSystemStart
            }

      , localTxSubmissionClient = Nothing
      }
    )

  atomically waitResult

  where
    chainSyncGetCurrentTip
      :: forall mode a
      .  STM a
      -> TMVar  ChainTip
      -> ChainSyncClient (BlockInMode mode) ChainPoint ChainTip IO ()
    chainSyncGetCurrentTip waitDone tipVar =
      ChainSyncClient $ pure clientStIdle
      where
        clientStIdle :: Net.Sync.ClientStIdle (BlockInMode mode) ChainPoint ChainTip IO ()
        clientStIdle =
          Net.Sync.SendMsgRequestNext clientStNext (pure clientStNext)

        clientStNext :: Net.Sync.ClientStNext (BlockInMode mode) ChainPoint ChainTip IO ()
        clientStNext = Net.Sync.ClientStNext
          { Net.Sync.recvMsgRollForward = \_block tip -> ChainSyncClient $ do
              void . atomically $ putTMVar tipVar tip
              void $ atomically waitDone
              pure $ Net.Sync.SendMsgDone ()
          , Net.Sync.recvMsgRollBackward = \_point tip -> ChainSyncClient $ do
              void . atomically $ putTMVar tipVar tip
              void $ atomically waitDone
              pure $ Net.Sync.SendMsgDone ()
          }

-- | Query the UTxO, filtered by a given set of addresses, from a Shelley node
-- via the local state query protocol.
--

runQueryUTxO
  :: AnyConsensusModeParams
  -> QueryUTxOFilter
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryUTxO (AnyConsensusModeParams cModeParams)
             qfilter network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query   = QueryInShelleyBasedEra sbe (QueryUTxO qfilter)
          qInMode = QueryInEra eInMode query
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeFilteredUTxOs sbe mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


-- | Query the current and future parameters for a stake pool, including the retirement date.
-- Any of these may be empty (in which case a null will be displayed).
--

runQueryPoolParams
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryPoolParams (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writePoolParams poolid) result


-- | Obtain stake snapshot information for a pool, plus information about the total active stake.
-- This information can be used for leader slot calculation, for example, and has been requested by SPOs.
-- Obtaining the information directly is significantly more time and memory efficient than using a full ledger state dump.
runQueryStakeSnapshot
  :: AnyConsensusModeParams
  -> NetworkId
  -> Hash StakePoolKey
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeSnapshot (AnyConsensusModeParams cModeParams) network poolid = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  eInMode <- toEraInMode era cMode
    & hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE)

  let qInMode = QueryInEra eInMode . QueryInShelleyBasedEra sbe $ QueryDebugLedgerState
  result <- executeQuery era cModeParams localNodeConnInfo qInMode
  obtainLedgerEraClassConstraints sbe (writeStakeSnapshot poolid) result


runQueryLedgerState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryLedgerState (AnyConsensusModeParams cModeParams)
                    network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryDebugLedgerState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      obtainLedgerEraClassConstraints sbe (writeLedgerState mOutFile) result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


runQueryProtocolState
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryProtocolState (AnyConsensusModeParams cModeParams)
                      network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let qInMode = QueryInEra eInMode
                      . QueryInShelleyBasedEra sbe
                      $ QueryProtocolState
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  qInMode
      writeProtocolState mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- | Query the current delegations and reward accounts, filtered by a given
-- set of addresses, from a Shelley node via the local state query protocol.

runQueryStakeAddressInfo
  :: AnyConsensusModeParams
  -> StakeAddress
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeAddressInfo (AnyConsensusModeParams cModeParams)
                         (StakeAddress _ addr) network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let stakeAddr = Set.singleton $ fromShelleyStakeCredential addr
          query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeAddresses stakeAddr network

      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeAddressInfo mOutFile $ DelegationsAndRewards result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE

-- -------------------------------------------------------------------------------------------------

-- | An error that can occur while querying a node's local state.
data ShelleyQueryCmdLocalStateQueryError
  = AcquireFailureError !LocalStateQuery.AcquireFailure
  | EraMismatchError !EraMismatch
  -- ^ A query from a certain era was applied to a ledger from a different
  -- era.
  | ByronProtocolNotSupportedError
  -- ^ The query does not support the Byron protocol.
  | ShelleyProtocolEraMismatch
  -- ^ The Shelley protocol only supports the Shelley era.
  deriving (Eq, Show)

renderLocalStateQueryError :: ShelleyQueryCmdLocalStateQueryError -> Text
renderLocalStateQueryError lsqErr =
  case lsqErr of
    AcquireFailureError err -> "Local state query acquire failure: " <> show err
    EraMismatchError err ->
      "A query from a certain era was applied to a ledger from a different era: " <> show err
    ByronProtocolNotSupportedError ->
      "The attempted local state query does not support the Byron protocol."
    ShelleyProtocolEraMismatch ->
        "The Shelley protocol mode can only be used with the Shelley era, "
     <> "i.e. with --shelley-mode use --shelley-era flag"

writeStakeAddressInfo
  :: Maybe OutputFile
  -> DelegationsAndRewards
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeAddressInfo mOutFile delegsAndRewards =
  case mOutFile of
    Nothing -> liftIO $ LBS.putStrLn (encodePretty delegsAndRewards)
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath (encodePretty delegsAndRewards)

writeLedgerState :: forall era ledgerera.
                    ShelleyLedgerEra era ~ ledgerera
                 => ToJSON (DebugLedgerState era)
                 => FromCBOR (DebugLedgerState era)
                 => Maybe OutputFile
                 -> SerialisedDebugLedgerState era
                 -> ExceptT ShelleyQueryCmdError IO ()
writeLedgerState mOutFile qState@(SerialisedDebugLedgerState serLedgerState) =
  case mOutFile of
    Nothing -> case decodeLedgerState qState of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right ledgerState -> liftIO . LBS.putStrLn $ encodePretty ledgerState
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        $ LBS.writeFile fpath $ unSerialised serLedgerState

writeStakeSnapshot :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => Era.Crypto ledgerera ~ StandardCrypto
  => FromCBOR (DebugLedgerState era)
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeSnapshot (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      -- Ledger State
      let (DebugLedgerState snapshot) = ledgerState

      -- The three stake snapshots, obtained from the ledger state
      let (SnapShots markS setS goS _) = esSnapshots $ nesEs snapshot

      -- Calculate the three pool and active stake values for the given pool
      liftIO . LBS.putStrLn $ encodePretty $ Stakes
        { markPool = getPoolStake hk markS
        , setPool = getPoolStake hk setS
        , goPool = getPoolStake hk goS
        , markTotal = getAllStake markS
        , setTotal = getAllStake setS
        , goTotal = getAllStake goS
        }

-- | Sum all the stake that is held by the pool
getPoolStake :: KeyHash Cardano.Ledger.Keys.StakePool crypto -> SnapShot crypto -> Integer
getPoolStake hash ss = pStake
  where
    Coin pStake = fold s
    (Stake s) = poolStake hash (_delegations ss) (_stake ss)

-- | Sum the active stake from a snapshot
getAllStake :: SnapShot crypto -> Integer
getAllStake (SnapShot stake _ _) = activeStake
  where
    Coin activeStake = fold . unStake $ stake

-- | This function obtains the pool parameters, equivalent to the following jq query on the output of query ledger-state
--   .nesEs.esLState._delegationState._pstate._pParams.<pool_id>
writePoolParams :: forall era ledgerera. ()
  => ShelleyLedgerEra era ~ ledgerera
  => FromCBOR (DebugLedgerState era)
  => Crypto.Crypto (Era.Crypto ledgerera)
  => Era.Crypto ledgerera ~ StandardCrypto
  => PoolId
  -> SerialisedDebugLedgerState era
  -> ExceptT ShelleyQueryCmdError IO ()
writePoolParams (StakePoolKeyHash hk) qState =
  case decodeLedgerState qState of
    -- In the event of decode failure print the CBOR instead
    Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs

    Right ledgerState -> do
      let DebugLedgerState snapshot = ledgerState
      let poolState = _pstate $ _delegationState $ esLState $ nesEs snapshot

      -- Pool parameters
      let poolParams = Map.lookup hk $ _pParams poolState
      let fPoolParams = Map.lookup hk $ _fPParams poolState
      let retiring = Map.lookup hk $ _retiring poolState

      liftIO . LBS.putStrLn $ encodePretty $ Params poolParams fPoolParams retiring

decodeLedgerState :: forall era. ()
  => FromCBOR (DebugLedgerState era)
  => SerialisedDebugLedgerState era
  -> Either LBS.ByteString (DebugLedgerState era)
decodeLedgerState (SerialisedDebugLedgerState (Serialised ls)) = first (const ls) (decodeFull ls)

writeProtocolState :: Crypto.Crypto StandardCrypto
                   => Maybe OutputFile
                   -> ProtocolState era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeProtocolState mOutFile ps@(ProtocolState pstate) =
  case mOutFile of
    Nothing -> case decodeProtocolState ps of
                 Left bs -> firstExceptT ShelleyQueryCmdHelpersError $ pPrintCBOR bs
                 Right chainDepstate -> liftIO . LBS.putStrLn $ encodePretty chainDepstate
    Just (OutputFile fpath) ->
      handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
        . LBS.writeFile fpath $ unSerialised pstate
 where
  decodeProtocolState
    :: ProtocolState era
    -> Either LBS.ByteString (Ledger.ChainDepState StandardCrypto)
  decodeProtocolState (ProtocolState (Serialised pbs)) =
    first (const pbs) (decodeFull pbs)

writeFilteredUTxOs :: ShelleyBasedEra era
                   -> Maybe OutputFile
                   -> UTxO era
                   -> ExceptT ShelleyQueryCmdError IO ()
writeFilteredUTxOs shelleyBasedEra' mOutFile utxo =
    case mOutFile of
      Nothing -> liftIO $ printFilteredUTxOs shelleyBasedEra' utxo
      Just (OutputFile fpath) ->
        case shelleyBasedEra' of
          ShelleyBasedEraShelley -> writeUTxo fpath utxo
          ShelleyBasedEraAllegra -> writeUTxo fpath utxo
          ShelleyBasedEraMary -> writeUTxo fpath utxo
          ShelleyBasedEraAlonzo -> writeUTxo fpath utxo
 where
   writeUTxo fpath utxo' =
     handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError fpath)
       $ LBS.writeFile fpath (encodePretty utxo')

printFilteredUTxOs :: ShelleyBasedEra era -> UTxO era -> IO ()
printFilteredUTxOs shelleyBasedEra' (UTxO utxo) = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAllegra ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraMary    ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
    ShelleyBasedEraAlonzo ->
      mapM_ (printUtxo shelleyBasedEra') $ Map.toList utxo
 where
   title :: Text
   title =
     "                           TxHash                                 TxIx        Amount"

printUtxo
  :: ShelleyBasedEra era
  -> (TxIn, TxOut era)
  -> IO ()
printUtxo shelleyBasedEra' txInOutTuple =
  case shelleyBasedEra' of
    ShelleyBasedEraShelley ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]

    ShelleyBasedEraAllegra ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraMary ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value _) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value
             ]
    ShelleyBasedEraAlonzo ->
      let (TxIn (TxId txhash) (TxIx index), TxOut _ value mDatum) = txInOutTuple
      in Text.putStrLn $
           mconcat
             [ Text.decodeLatin1 (hashToBytesAsHex txhash)
             , textShowN 6 index
             , "        " <> printableValue value <> " + " <> Text.pack (show mDatum)
             ]
 where
  textShowN :: Show a => Int -> a -> Text
  textShowN len x =
    let str = show x
        slen = length str
    in Text.pack $ replicate (max 1 (len - slen)) ' ' ++ str

  printableValue :: TxOutValue era -> Text
  printableValue (TxOutValue _ val) = renderValue defaultRenderValueOptions val
  printableValue (TxOutAdaOnly _ (Lovelace i)) = Text.pack $ show i


runQueryStakeDistribution
  :: AnyConsensusModeParams
  -> NetworkId
  -> Maybe OutputFile
  -> ExceptT ShelleyQueryCmdError IO ()
runQueryStakeDistribution (AnyConsensusModeParams cModeParams)
                          network mOutFile = do
  SocketPath sockPath <- firstExceptT ShelleyQueryCmdEnvVarSocketErr readEnvSocketPath
  let localNodeConnInfo = LocalNodeConnectInfo cModeParams network sockPath

  anyE@(AnyCardanoEra era) <- determineEra cModeParams localNodeConnInfo
  let cMode = consensusModeOnly cModeParams
  sbe <- getSbe $ cardanoEraStyle era

  case toEraInMode era cMode of
    Just eInMode -> do
      let query = QueryInEra eInMode
                    . QueryInShelleyBasedEra sbe
                    $ QueryStakeDistribution
      result <- executeQuery
                  era
                  cModeParams
                  localNodeConnInfo
                  query
      writeStakeDistribution mOutFile result
    Nothing -> left $ ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode cMode) anyE


writeStakeDistribution
  :: Maybe OutputFile
  -> Map PoolId Rational
  -> ExceptT ShelleyQueryCmdError IO ()
writeStakeDistribution (Just (OutputFile outFile)) stakeDistrib =
  handleIOExceptT (ShelleyQueryCmdWriteFileError . FileIOError outFile) $
    LBS.writeFile outFile (encodePretty stakeDistrib)

writeStakeDistribution Nothing stakeDistrib =
  liftIO $ printStakeDistribution stakeDistrib


printStakeDistribution :: Map PoolId Rational -> IO ()
printStakeDistribution stakeDistrib = do
  Text.putStrLn title
  putStrLn $ replicate (Text.length title + 2) '-'
  sequence_
    [ putStrLn $ showStakeDistr poolId stakeFraction
    | (poolId, stakeFraction) <- Map.toList stakeDistrib ]
 where
   title :: Text
   title =
     "                           PoolId                                 Stake frac"

   showStakeDistr :: PoolId
                  -> Rational
                  -- ^ Stake fraction
                  -> String
   showStakeDistr poolId stakeFraction =
     concat
       [ Text.unpack (serialiseToBech32 poolId)
       , "   "
       , showEFloat (Just 3) (fromRational stakeFraction :: Double) ""
       ]

-- | A mapping of Shelley reward accounts to both the stake pool that they
-- delegate to and their reward account balance.
newtype DelegationsAndRewards
  = DelegationsAndRewards (Map StakeAddress Lovelace, Map StakeAddress PoolId)


mergeDelegsAndRewards :: DelegationsAndRewards -> [(StakeAddress, Maybe Lovelace, Maybe PoolId)]
mergeDelegsAndRewards (DelegationsAndRewards (rewardsMap, delegMap)) =
 [ (stakeAddr, Map.lookup stakeAddr rewardsMap, Map.lookup stakeAddr delegMap)
 | stakeAddr <- nub $ Map.keys rewardsMap ++ Map.keys delegMap
 ]


instance ToJSON DelegationsAndRewards where
  toJSON delegsAndRwds =
      Aeson.Array . Vector.fromList
        . map delegAndRwdToJson $ mergeDelegsAndRewards delegsAndRwds
    where
      delegAndRwdToJson :: (StakeAddress, Maybe Lovelace, Maybe PoolId) -> Aeson.Value
      delegAndRwdToJson (addr, mRewards, mPoolId) =
        Aeson.object
          [ "address" .= serialiseAddress addr
          , "delegation" .= mPoolId
          , "rewardAccountBalance" .= mRewards
          ]

-- Helpers

calcEraInMode
  :: CardanoEra era
  -> ConsensusMode mode
  -> ExceptT ShelleyQueryCmdError IO (EraInMode era mode)
calcEraInMode era mode=
  hoistMaybe (ShelleyQueryCmdEraConsensusModeMismatch (AnyConsensusMode mode) (anyCardanoEra era))
                   $ toEraInMode era mode

determineEra
  :: ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> ExceptT ShelleyQueryCmdError IO AnyCardanoEra
determineEra cModeParams localNodeConnInfo =
  case consensusModeOnly cModeParams of
    ByronMode -> return $ AnyCardanoEra ByronEra
    ShelleyMode -> return $ AnyCardanoEra ShelleyEra
    CardanoMode -> do
      eraQ <- liftIO . queryNodeLocalState localNodeConnInfo Nothing
                     $ QueryCurrentEra CardanoModeIsMultiEra
      case eraQ of
        Left acqFail -> left $ ShelleyQueryCmdAcquireFailure acqFail
        Right anyCarEra -> return anyCarEra

executeQuery
  :: forall result era mode. CardanoEra era
  -> ConsensusModeParams mode
  -> LocalNodeConnectInfo mode
  -> QueryInMode mode (Either EraMismatch result)
  -> ExceptT ShelleyQueryCmdError IO result
executeQuery era cModeP localNodeConnInfo q = do
  eraInMode <- calcEraInMode era $ consensusModeOnly cModeP
  case eraInMode of
    ByronEraInByronMode -> left ShelleyQueryCmdByronEra
    _ -> liftIO execQuery >>= queryResult
 where
   execQuery :: IO (Either AcquireFailure (Either EraMismatch result))
   execQuery = queryNodeLocalState localNodeConnInfo Nothing q

getSbe :: CardanoEraStyle era -> ExceptT ShelleyQueryCmdError IO (ShelleyBasedEra era)
getSbe LegacyByronEra = left ShelleyQueryCmdByronEra
getSbe (ShelleyBasedEra sbe) = return sbe

queryResult
  :: Either AcquireFailure (Either EraMismatch a)
  -> ExceptT ShelleyQueryCmdError IO a
queryResult eAcq =
  case eAcq of
    Left acqFailure -> left $ ShelleyQueryCmdAcquireFailure acqFailure
    Right eResult ->
      case eResult of
        Left err -> left . ShelleyQueryCmdLocalStateQueryError $ EraMismatchError err
        Right result -> return result

obtainLedgerEraClassConstraints
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> ((Ledger.ShelleyBased ledgerera
      , ToJSON (DebugLedgerState era)
      , FromCBOR (DebugLedgerState era)
      , Era.Crypto ledgerera ~ StandardCrypto
      ) => a) -> a
obtainLedgerEraClassConstraints ShelleyBasedEraShelley f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAllegra f = f
obtainLedgerEraClassConstraints ShelleyBasedEraMary    f = f
obtainLedgerEraClassConstraints ShelleyBasedEraAlonzo  f = f
