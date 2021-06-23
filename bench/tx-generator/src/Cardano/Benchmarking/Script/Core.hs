{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Use uncurry" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-} --
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Benchmarking.Script.Core
where

import           Prelude
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Control.Concurrent (threadDelay)
import           Control.Tracer (traceWith)

import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (..))
import           Cardano.Api ( AsType(..), CardanoEra(..), InAnyCardanoEra(..), AnyCardanoEra(..), IsShelleyBasedEra, Tx
                             , Lovelace, NetworkId(..), cardanoEra
                             , CardanoMode, LocalNodeConnectInfo
                             , PaymentKey
                             , SigningKey
                             , TxInMode
                             , TxValidationErrorInMode
                             , getLocalChainTip, queryNodeLocalState, QueryInMode( QueryCurrentEra), ConsensusModeIsMultiEra( CardanoModeIsMultiEra )
                             , chainTipToChainPoint )

import qualified Cardano.Benchmarking.FundSet as FundSet
import           Cardano.Benchmarking.FundSet (FundInEra(..), Validity(..), liftAnyEra )
import qualified Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   (asyncBenchmark, waitBenchmark, walletBenchmark
                   , readSigningKey, secureGenesisFund, splitFunds, txGenerator)
import           Cardano.Benchmarking.GeneratorTx as GeneratorTx
                   (AsyncBenchmarkControl, TxGenError)

import           Cardano.Benchmarking.GeneratorTx.Tx as Core (keyAddress, txInModeCardano)
import           Cardano.Benchmarking.GeneratorTx.LocalProtocolDefinition as Core (startProtocol)
import           Cardano.Benchmarking.GeneratorTx.NodeToNode (ConnectClient, benchmarkConnectTxSubmit)
import           Cardano.Benchmarking.OuroborosImports as Core
                   (LocalSubmitTx, SigningKeyFile
                   , getGenesis, protocolToNetworkId, protocolToCodecConfig, makeLocalConnectInfo, submitTxToNodeLocal)
import           Cardano.Benchmarking.PlutusExample as PlutusExample
import           Cardano.Benchmarking.Tracer as Core
                   ( TraceBenchTxSubmit (..)
                   , createTracers, btTxSubmit_, btN2N_, btConnect_, btSubmission_)
import           Cardano.Benchmarking.Types as Core (NumberOfTxs(..), SubmissionErrorPolicy(..), TPSRate)
import           Cardano.Benchmarking.Wallet

import           Cardano.Benchmarking.Script.Env
import           Cardano.Benchmarking.Script.Setters
import           Cardano.Benchmarking.Script.Store as Store

liftCoreWithEra :: (forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO x) -> ActionM (Either TxGenError x)
liftCoreWithEra coreCall = withEra ( liftIO . runExceptT . coreCall)

withEra :: (forall era. IsShelleyBasedEra era => AsType era -> ActionM x) -> ActionM x
withEra action = do
  era <- get $ User TEra
  case era of
    AnyCardanoEra AlonzoEra  -> action AsAlonzoEra
    AnyCardanoEra MaryEra    -> action AsMaryEra
    AnyCardanoEra AllegraEra -> action AsAllegraEra
    AnyCardanoEra ShelleyEra -> action AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"

startProtocol :: FilePath -> ActionM ()
startProtocol filePath = do
  liftIO (runExceptT $ Core.startProtocol filePath) >>= \case
    Left err -> throwE $ CliError err
    Right (loggingLayer, protocol) -> do
      set LoggingLayer loggingLayer
      set Protocol protocol
      set BenchTracers $ Core.createTracers loggingLayer
      set Genesis $ Core.getGenesis protocol
      set NetworkId $ protocolToNetworkId protocol

readSigningKey :: KeyName -> SigningKeyFile -> ActionM ()
readSigningKey name filePath =
  liftIO ( runExceptT $ GeneratorTx.readSigningKey filePath) >>= \case
    Left err -> liftTxGenError err
    Right key -> setName name key

getLocalSubmitTx :: ActionM LocalSubmitTx
getLocalSubmitTx = submitTxToNodeLocal <$> getLocalConnectInfo

--obsolete use importGenesisFund
secureGenesisFund
   :: FundName
   -> KeyName
   -> KeyName
   -> ActionM ()
secureGenesisFund fundName destKey genesisKeyName = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> do
      -- Todo : user only of two methods
      setName fundName fund -- Old method

splitFundN
   :: NumberOfTxs
   -> KeyName
   -> FundName
   -> ActionM [Store.Fund]
splitFundN count destKeyName sourceFund = do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  fee      <- getUser TFee
  destKey  <- getName destKeyName
  (fund, fundKey) <- consumeName sourceFund
  txIn     <- getUser TNumberOfInputsPerTx
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO [Store.Fund]
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.splitFunds tracer localSubmit fee count txIn fundKey addr fund
      return $ zip f $ repeat destKey
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right funds -> return funds

splitFund
   :: [FundName]
   -> KeyName
   -> FundName
   -> ActionM ()
splitFund newFunds destKey sourceFund = do
  funds <- splitFundN (NumberOfTxs $ fromIntegral $ length newFunds) destKey sourceFund
  forM_ (zip newFunds funds) $ \(name, f) -> setName name f

splitFundToList
   :: FundListName
   -> KeyName
   -> FundName
   -> ActionM ()
splitFundToList newFunds destKey sourceFund = do
  count <- getUser TNumberOfTxs
  funds <- splitFundN count destKey sourceFund
  setName newFunds funds

delay :: Double -> ActionM ()
delay t = liftIO $ threadDelay $ floor $ 1000000 * t

prepareTxList
   :: TxListName
   -> KeyName
   -> FundListName
   -> ActionM ()
prepareTxList name destKey srcFundName = do
  tracer   <- btTxSubmit_ <$> get BenchTracers
  networkId <- get NetworkId
  fee      <- getUser TFee
  fundList <- consumeName srcFundName
  key      <- getName destKey
  txIn     <- getUser TNumberOfInputsPerTx
  txOut    <- getUser TNumberOfOutputsPerTx
  count    <- getUser TNumberOfTxs
  payload  <- getUser TTxAdditionalSize
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO (InAnyCardanoEra TxList)
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId key
      ----------------------------------------------------TODO : Constant 1 ???
      l <- GeneratorTx.txGenerator tracer fee count txIn txOut payload addr (snd $ head fundList) 1 (map fst fundList)
      return $ InAnyCardanoEra cardanoEra $ TxList l
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right l -> setName name l

waitBenchmarkCore :: AsyncBenchmarkControl ->  ActionM ()
waitBenchmarkCore ctl = do
  tracers  <- get BenchTracers
  _ <- liftIO $ runExceptT $ GeneratorTx.waitBenchmark (btTxSubmit_ tracers) ctl
  return ()

-- This the benchmark based on transaction lists.
-- It is obsolte when the tx-list are replaced with the wallet data type.
asyncBenchmarkCore :: ThreadName -> TxListName -> TPSRate -> ActionM AsyncBenchmarkControl
asyncBenchmarkCore (ThreadName threadName) transactions tps = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  txs      <- getName transactions
  (Testnet networkMagic) <- get NetworkId
  protocol <- get Protocol
  ioManager <- askIOManager
  let
    connectClient :: ConnectClient
    connectClient  = benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       (btSubmission_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic

    coreCall :: forall era. IsShelleyBasedEra era => [Tx era] -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall l = GeneratorTx.asyncBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient threadName targets tps LogErrors l
  ret <- liftIO $ runExceptT $ case txs of
    InAnyCardanoEra AlonzoEra  (TxList l) -> coreCall l
    InAnyCardanoEra MaryEra    (TxList l) -> coreCall l
    InAnyCardanoEra AllegraEra (TxList l) -> coreCall l
    InAnyCardanoEra ShelleyEra (TxList l) -> coreCall l
    InAnyCardanoEra ByronEra   _ -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> return ctl

asyncBenchmark :: ThreadName -> TxListName -> TPSRate -> ActionM ()
asyncBenchmark controlName txList tps = asyncBenchmarkCore controlName txList tps >>= setName controlName

waitBenchmark :: ThreadName -> ActionM ()
waitBenchmark n = getName n >>= waitBenchmarkCore

cancelBenchmark :: ThreadName -> ActionM ()
cancelBenchmark n = do
  ctl@(_, _ , _ , shutdownAction) <- getName n
  liftIO shutdownAction
  waitBenchmarkCore ctl

getLocalConnectInfo :: ActionM  (LocalNodeConnectInfo CardanoMode)
getLocalConnectInfo = makeLocalConnectInfo <$> get NetworkId <*> getUser TLocalSocket

queryEra :: ActionM AnyCardanoEra
queryEra = do
  localNodeConnectInfo <- getLocalConnectInfo
  chainTip  <- liftIO $ getLocalChainTip localNodeConnectInfo
  ret <- liftIO $ queryNodeLocalState localNodeConnectInfo (Just $ chainTipToChainPoint chainTip) $ QueryCurrentEra CardanoModeIsMultiEra
  case ret of
    Right era -> return era
    Left err -> throwE $ ApiError $ show err

waitForEra :: AnyCardanoEra -> ActionM ()
waitForEra era = do
  currentEra <- queryEra
  if currentEra == era
    then return ()
    else do
      traceError $ "Current era: " ++ show currentEra ++ " Waiting for: " ++ show era
      liftIO $ threadDelay 1_000_000
      waitForEra era

localSubmitTx :: TxInMode CardanoMode -> ActionM (SubmitResult (TxValidationErrorInMode CardanoMode))
localSubmitTx tx = do
  submitTracer <- btTxSubmit_ <$> get BenchTracers
  submit <- getLocalSubmitTx
  ret <- liftIO $ submit tx
  let
    msg = case ret of
      SubmitSuccess -> mconcat
        [ "local submit success (" , show tx , ")"]
      SubmitFail e -> mconcat
        [ "local submit failed: " , show e , " (" , show tx , ")"]
  liftIO $ traceWith submitTracer $ TraceBenchTxSubDebug msg
  return ret

runBenchmark :: ThreadName -> NumberOfTxs -> TPSRate -> ActionM ()
runBenchmark (ThreadName threadName) txCount tps = do
  tracers  <- get BenchTracers
  targets  <- getUser TTargets
  (Testnet networkMagic) <- get NetworkId
  protocol <- get Protocol
  ioManager <- askIOManager
  era <- get $ User TEra
  walletRef <- get GlobalWallet
  let
    connectClient :: ConnectClient
    connectClient  = benchmarkConnectTxSubmit
                       ioManager
                       (btConnect_ tracers)
                       (btSubmission_ tracers)
                       (protocolToCodecConfig protocol)
                       networkMagic
    walletScript :: forall era. IsShelleyBasedEra era => FundSet.Target -> WalletScript era
    walletScript = benchmarkWalletScript walletRef txCount 2

    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO AsyncBenchmarkControl
    coreCall eraProxy = GeneratorTx.walletBenchmark (btTxSubmit_ tracers) (btN2N_ tracers) connectClient
                                               threadName targets tps LogErrors eraProxy txCount walletScript
  ret <- liftIO $ runExceptT $ case era of
    AnyCardanoEra AlonzoEra  -> coreCall AsAlonzoEra
    AnyCardanoEra MaryEra    -> coreCall AsMaryEra
    AnyCardanoEra AllegraEra -> coreCall AsAllegraEra
    AnyCardanoEra ShelleyEra -> coreCall AsShelleyEra
    AnyCardanoEra ByronEra   -> error "byron not supported"
  case ret of
    Left err -> liftTxGenError err
    Right ctl -> do
      setName (ThreadName threadName) ctl

-- Todo: make it possible to import several funds
-- (Split init and import)
importGenesisFund
   :: KeyName
   -> KeyName
   -> ActionM ()
importGenesisFund genesisKeyName destKey= do
  tracer <- btTxSubmit_ <$> get BenchTracers
  localSubmit <- getLocalSubmitTx
  networkId <- get NetworkId
  genesis  <- get Genesis
  fee      <- getUser TFee
  ttl      <- getUser TTTL
  fundKey  <- getName destKey
  genesisKey  <- getName genesisKeyName
  let
    coreCall :: forall era. IsShelleyBasedEra era => AsType era -> ExceptT TxGenError IO Store.Fund
    coreCall _proxy = do
      let addr = Core.keyAddress @ era networkId fundKey
      f <- GeneratorTx.secureGenesisFund tracer localSubmit networkId genesis fee ttl genesisKey addr
      return (f, fundKey)
  liftCoreWithEra coreCall >>= \case
    Left err -> liftTxGenError err
    Right fund -> initGlobalWallet networkId fundKey fund

-- Todo split init and import of funds
initGlobalWallet :: NetworkId -> SigningKey PaymentKey -> Fund -> ActionM ()
initGlobalWallet networkId key ((txIn, outVal), skey) = do
  wallet <- liftIO $ initWallet networkId key
  liftIO (walletRefInsertFund wallet (FundSet.Fund $ mkFund outVal))
  set GlobalWallet wallet
 where
  mkFund = liftAnyEra $ \value -> FundInEra {
    _fundTxIn = txIn
  , _fundVal = value
  , _fundSigningKey = skey
  , _fundValidity = Confirmed
  }

{-
elif [ "$1" == "" ]; then
 plutusscriptinuse=scripts/plutus/scripts/untyped-always-succeeds-txin.plutus
 # This datum hash is the hash of the untyped 42
 scriptdatumhash="9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
 plutusrequiredspace=70000000
 plutusrequiredtime=70000000
 datumfilepath=scripts/plutus/data/42.datum
 redeemerfilepath=scripts/plutus/data/42.redeemer
 echo "Always succeeds Plutus script in use. Any datum and redeemer combination will succeed."
 echo "Script at: $plutusscriptinuse"
fi
-}

createChangePlutus :: Lovelace -> Int -> ActionM ()
createChangePlutus value count = do
  walletRef <- get GlobalWallet
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout"
  script <- liftIO $ PlutusExample.readScript "scripts/plutus/scripts/untyped-always-succeeds-txin.plutus"
  let scriptData = PlutusExample.toScriptHash "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
  let
    createCoins coins = do
      tx <- liftIO $ modifyWalletRefEither walletRef (walletCreateCoins (PlutusExample.payToScript fundKey (script, scriptData) networkId) coins)
      return $ fmap txInModeCardano tx
  createChangeGeneric createCoins value count

createChange :: Lovelace -> Int -> ActionM ()
createChange value count = do
  networkId <- get NetworkId
  fundKey <- getName $ KeyName "pass-partout"
  walletRef <- get GlobalWallet
  era <- get $ User TEra
  let
    createCoinsGen :: forall era. IsShelleyBasedEra era => AsType era -> [Lovelace] -> ActionM (Either String (TxInMode CardanoMode))
    createCoinsGen _proxy coins = do
      (tx :: Either String (Tx era)) <- liftIO $ modifyWalletRefEither walletRef (walletCreateCoins (genTx fundKey networkId) coins)
      return $ fmap txInModeCardano tx
    createCoins :: ([Lovelace] -> ActionM (Either String (TxInMode CardanoMode)))
    createCoins = case era of
      AnyCardanoEra AlonzoEra  -> createCoinsGen AsAlonzoEra
      AnyCardanoEra MaryEra    -> createCoinsGen AsMaryEra
      AnyCardanoEra AllegraEra -> createCoinsGen AsAllegraEra
      AnyCardanoEra ShelleyEra -> createCoinsGen AsShelleyEra
      AnyCardanoEra ByronEra   -> error "byron not supported"
  createChangeGeneric createCoins value count

createChangeGeneric :: ([Lovelace] -> ActionM (Either String (TxInMode CardanoMode))) -> Lovelace -> Int -> ActionM ()
createChangeGeneric createCoins value count = do
  let
    coinsList = replicate count value
    maxTxSize = 30
    chunks = chunkList maxTxSize coinsList
  forM_ chunks $ \coins -> do
    gen <- createCoins coins
    case gen of
      Left (_err :: String) -> return ()
      Right tx -> void $ localSubmitTx tx
 where
  chunkList :: Int -> [a] -> [[a]]
  chunkList _ [] = []
  chunkList n xs = as : chunkList n bs where (as,bs) = splitAt n xs

{-
This is for dirty hacking and testing and quick-fixes.
Its a function that can be called from the JSON scripts
and for which the JSON encoding is "reserved".
-}
{-
reserved :: [String] -> ActionM ()
reserved _ = do
  throwE $ UserError "no dirty hack is implemented"

-}
reserved :: [String] -> ActionM ()
reserved _ = do
  _networkId <- get NetworkId
  return ()
  -- wallet <- get GlobalWallet >>= liftIO .
