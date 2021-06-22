{-# OPTIONS_GHC -Wwarn #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RankNTypes #-}

module Cardano.Benchmarking.FundSet
where
import           Prelude

import           Data.IxSet.Typed as IxSet
import           Data.Proxy

import           Cardano.Api as Api

-- Outputs that are available for spending.
-- When building a new TX they provide the TxIn parts.

data FundInEra era = FundInEra {
    _fundTxIn :: !TxIn
  , _fundVal  :: !(TxOutValue era)
  , _fundSigningKey :: !(SigningKey PaymentKey)
  , _fundValidity :: !Validity
  } deriving (Show)

data Validity
  = Confirmed
  | InFlight !Target !SeqNumber
  deriving  (Show, Eq, Ord)

newtype Target = Target String
  deriving  (Show, Eq, Ord)

newtype SeqNumber = SeqNumber Int
  deriving  (Show, Eq, Ord, Enum)

newtype Fund = Fund {unFund :: InAnyCardanoEra FundInEra}

getFundTxIn :: Fund -> TxIn
getFundTxIn (Fund (InAnyCardanoEra _ a)) = _fundTxIn a

getFundKey :: Fund -> SigningKey PaymentKey
getFundKey (Fund (InAnyCardanoEra _ a)) = _fundSigningKey a

getFundValidity :: Fund -> Validity
getFundValidity (Fund (InAnyCardanoEra _ a)) = _fundValidity a

getFundLovelace :: Fund -> Lovelace
getFundLovelace (Fund (InAnyCardanoEra _ a)) = case _fundVal a of
  TxOutAdaOnly _era l -> l
  TxOutValue _era v -> selectLovelace v

data IsConfirmed = IsConfirmed | IsNotConfirmed
  deriving  (Show, Eq, Ord)

isConfirmed :: Fund -> IsConfirmed
isConfirmed f = case getFundValidity f of
  Confirmed -> IsConfirmed
  InFlight _ _ -> IsNotConfirmed

instance Show Fund where
  show (Fund (InAnyCardanoEra _ f)) = show f

-- TxIn/fundTxOut is the primary key.
-- There must be no two entries for the same TxIn !.

instance Eq Fund where
  (==) a b = getFundTxIn a == getFundTxIn b

instance Ord Fund where
  compare a b = compare (getFundTxIn a) (getFundTxIn b)

type FundIndices = '[ TxIn, IsConfirmed, Target, SeqNumber, Lovelace ]
type FundSet = IxSet FundIndices Fund

instance Indexable FundIndices Fund where
  indices = ixList
    (ixFun $ \f -> [ getFundTxIn f ])
    (ixFun $ \f -> [ isConfirmed f ])
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> []
      InFlight t _ -> [t]
    )
    (ixFun $ \f -> case getFundValidity f of
      Confirmed -> [SeqNumber (-1) ] -- Confirmed Txs get SeqNumber -1
      InFlight _ n -> [ n ]
    )
    (ixFun $ \f -> [ getFundLovelace f ])

emptyFunds :: FundSet
emptyFunds = IxSet.empty

insertFund :: FundSet -> Fund -> FundSet
insertFund s f = updateIx (getFundTxIn f) f s

deleteFund :: FundSet -> Fund -> FundSet
deleteFund s f = deleteIx (getFundTxIn f) s

liftAnyEra :: ( forall era. IsCardanoEra era => f1 era -> f2 era ) -> InAnyCardanoEra f1 -> InAnyCardanoEra f2
liftAnyEra f x = case x of
  InAnyCardanoEra ByronEra a   ->   InAnyCardanoEra ByronEra $ f a
  InAnyCardanoEra ShelleyEra a ->   InAnyCardanoEra ShelleyEra $ f a
  InAnyCardanoEra AllegraEra a ->   InAnyCardanoEra AllegraEra $ f a
  InAnyCardanoEra MaryEra a    ->   InAnyCardanoEra MaryEra $ f a
  InAnyCardanoEra AlonzoEra a  ->   InAnyCardanoEra AlonzoEra $ f a

type FundSelector = FundSet -> Either String [Fund]

-- Select a number of confirmed Fund that where send to a specific Target node.
-- TODO: dont ignore target.
selectCountTarget :: Int -> Target -> FundSet -> Either String [Fund]
selectCountTarget count _target fs =
  if length funds == count
    then Right funds
    else Left "could not find enough input coins"
  where
    -- Just take confirmed coins.
    -- TODO: extend this to unconfimed coins to the same target node
    funds = take count $ toAscList ( Proxy :: Proxy Lovelace) (fs @= IsConfirmed)

-- Select Funds to cover a minimum value.
-- TODO:
-- This fails unless there is a single fund with the required value
-- Extend this to really return a list of funds.
selectMinValue :: Lovelace -> FundSet -> Either String [Fund]
selectMinValue minValue fs = case coins of
    [] -> Left $ "findSufficientCoin: no single coin with min value >= " ++ show minValue
    (c:_) -> Right [c]
    where coins = toAscList ( Proxy :: Proxy Lovelace) (fs @= IsConfirmed @>= minValue)
