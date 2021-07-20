{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}


module Cardano.Logging.PureFold where

import           Control.Arrow
import           Control.Monad (join)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Tracer as T
import qualified Control.Tracer.Arrow as TA

import           Cardano.Logging.Types

-- TODO: Build a foldTrace which uses something like an ArrowLoop for keeping
-- the state without using an IORef


instance (Monad m, Applicative m) => ArrowLoop (TA.Tracer m) where
  loop tr = undefined
    where
      foldTr :: ((a,acc) -> (a,acc)) -> TA.Tracer m a () -> TA.Tracer m (a, acc) ((),acc)
      foldTr f (TA.Squelching l) = TA.Squelching (arr f >>> first l)
      foldTr f (TA.Emitting p n) = TA.Emitting   (arr f >>> first p) (first n)


foldA :: forall m a x acc. Monad m
  => (a -> acc -> acc)
  -> acc
  -> T.Tracer m a
  -> T.Tracer m a
foldA foldf acc tr =
    let newF = (\(a, acc) -> let res = foldf a acc in (a, res))
        recT = foldTr newF (T.use tr)
    in case recT of
        TA.Squelching f ->
          T.arrow $ TA.Squelching
            (Kleisli (\a -> do
              (a',acc') <- runKleisli f (a,acc)
              pure a'))
        (TA.Emitting
          (p :: Kleisli m (a,acc) x)
          (n :: Kleisli m x ((),acc)) :: TA.Tracer m (a,acc) ((),acc)) ->
          T.arrow $ TA.Emitting
            (Kleisli (\a -> do
              (_,acc') <- runKleisli p (a,acc)
              pure $ foldA foldf tr acc'))
            (Kleisli (\a -> do
              (_a',_acc') <- runKleisli n a
              pure ()))
  where
    foldTr :: ((a,acc) -> (a,acc)) -> TA.Tracer m a () -> TA.Tracer m (a, acc) ((),acc)
    foldTr f (TA.Squelching l) = TA.Squelching (arr f >>> first l)
    foldTr f (TA.Emitting p n) = TA.Emitting   (arr f >>> first p) (first n)
