{-|
Module      : Control.TracerA.Arrow
Copyright   : (c) Alexander Vieth, 2019
Licence     : Apache-2.0
Maintainer  : aovieth@gmail.com
-}

{-# LANGUAGE GADTs        #-}
{-# LANGUAGE Arrows       #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE BangPatterns #-}

module Control.Tracer.Arrow
  ( TracerA (..)
  , runTracerA
  , compute
  , emit
  , effect
  , squelch
  , nat
  ) where

import Prelude hiding ((.), id)
import Control.Arrow
import Control.Category

-- | Formal representation of a tracer arrow as a Kleisli arrow over some
-- monad, but tagged so that we know whether it has any effects which will emit
-- a trace.
data TracerA m a b where
  -- | An emitting part, followed by a non-emitting part.
  -- The non-emitting part is there so that later emitting parts can be
  -- tacked-on later.
  Emitting   :: Kleisli m a x -> Kleisli m x b -> TracerA m a b
  -- | No emitting. There may be side-effects, but they are assumed to be
  -- benign and will be discarded by 'runTracerA'.
  Squelching :: Kleisli m a b                  -> TracerA m a b

-- | The resulting Kleisli arrow includes all of the effects required to do
-- the emitting part.
runTracerA :: Monad m => TracerA m a () -> Kleisli m a ()
runTracerA (Emitting emits _noEmits) = emits >>> arr (const ())
runTracerA (Squelching     _       ) =           arr (const ())

-- | Ignore the input and do not emit. The name is intended to lead to clear
-- and suggestive arrow expressions.
squelch :: Applicative m => TracerA m a ()
squelch = compute (const ())

-- | Do an emitting effect. Contrast with 'effect' which does not make the
-- tracer an emitting tracer.
emit :: Applicative m => (a -> m ()) -> TracerA m a ()
emit f = Emitting (Kleisli f) (Kleisli (const (pure ())))

-- | Do a non-emitting effect. This effect will only be run if some part of
-- the tracer downstream emits (see 'emit').
effect :: (a -> m b) -> TracerA m a b
effect = Squelching . Kleisli

-- | Pure computation in a tracer: no side effects or emits.
compute :: Applicative m => (a -> b) -> TracerA m a b
compute f = effect (pure . f)

instance Monad m => Category (TracerA m) where
  id = compute id
  Squelching l     . Squelching r     = Squelching (l  . r)
  -- Crucial: the squelching parts stay together. Could also have written
  --                                  = Emitting   (rp . re)      l
  -- but that would miss opportunities to skip doing work.
  Squelching l     . Emitting   re rp = Emitting   re             (l . rp)
  -- Contrast with the above clause: here the emitting part comes _after_ the
  -- squelching part, so the squelching part becomes part of the emitting part.
  Emitting   le lp . Squelching r     = Emitting   (le . r)       lp
  Emitting   le lp . Emitting   re rp = Emitting   (le . rp . re) lp

instance Monad m => Arrow (TracerA m) where
  arr = compute
  Squelching l     *** Squelching r     = Squelching (l  *** r )
  Squelching l     *** Emitting   re rp = Emitting   (id *** re) (l  *** rp)
  Emitting   le lp *** Squelching r     = Emitting   (le *** id) (lp *** r )
  Emitting   le lp *** Emitting   re rp = Emitting   (le *** re) (lp *** rp)

instance Monad m => ArrowChoice (TracerA m) where
  Squelching l     +++ Squelching r     = Squelching (l +++ r)
  Squelching l     +++ Emitting   re rp = Emitting   (id +++ re) (l  +++ rp)
  Emitting   le lp +++ Squelching r     = Emitting   (le +++ id) (lp +++ r )
  Emitting   le lp +++ Emitting   re rp = Emitting   (le +++ re) (lp +++ rp)

-- | Use a natural transformation to change the underlying monad.
nat :: (forall x . m x -> n x) -> TracerA m a b -> TracerA n a b
nat h (Squelching (Kleisli k))             = Squelching (Kleisli (h . k))
nat h (Emitting   (Kleisli k) (Kleisli l)) = Emitting   (Kleisli (h . k)) (Kleisli (h . l))
