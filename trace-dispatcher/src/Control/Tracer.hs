{-|
Module      : Control.Tracer
Description : A simple interface for logging, tracing, and monitoring
Copyright   : (c) Alexander Vieth, 2019
Maintainer  : aovieth@gmail.com
License     : Apache-2.0

=== General usage

'Tracer' is a contravariant functor intended to express the pattern in which
values of its parameter type are used to produce effects which are prescribed
by the caller, as in tracing, logging, code instrumentation, etc.

Programs should be written to use as specific a tracer as possible, i.e. to
take as a parameter a @Tracer m domainSpecificType@. To combine these programs
into an executable which does meaningful tracing, an implementation of that
tracing should be used to make a @Tracer probablyIO implementationTracingType@,
which is 'contramap'ped to fit @Tracer m domainSpecificType@ wherever it is
needed, for the various @domainSpecificType@s that appear throughout the
program.

=== An example

This short example shows how a tracer can be deployed, highlighting the use of
'contramap' to fit a general tracer which writes text to a file, where a
specific tracer which takes domain-specific events is expected.

> -- Writes text to some log file.
> traceToLogFile :: FilePath -> Tracer IO Text
>
> -- Domain-specific event type.
> data Event = EventA | EventB Int
>
> -- The log-file format for an Event.
> eventToText :: Event -> Text
>
> -- Some action that can use any tracer on Event, in any monad.
> actionWithTrace :: Monad m => Tracer m Event -> m ()
> actionWithTrace tracer = do
>   traceWith tracer EventA
>   traceWith tracer (EventB 42)
>
> -- Set up a log file tracer, then use it where the Event tracer is expected.
> main :: IO ()
> main = do
>   textTacer <- traceToLogFile "log.txt"
>   let eventTracer :: Tracer IO Event
>       eventTracer = contramap eventToText tracer
>   actionWithTrace eventTracer
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Tracer
    ( Tracer (..)
    , traceWith
    , arrow
    , use
    , Arrow.squelch
    , Arrow.emit
    , Arrow.effect
    -- * Simple tracers
    , nullTracer
    , stdoutTracer
    , debugTracer
    -- * Transforming tracers
    , natTracer
    , Arrow.nat
    , traceMaybe
    , squelchUnless
    -- * Re-export of Contravariant
    , Contravariant(..)
    ) where

import           Control.Arrow ((|||), (&&&), arr, runKleisli)
import           Control.Category ((>>>))
import           Data.Functor.Contravariant (Contravariant (..))
import           Debug.Trace (traceM)

import qualified Control.Tracer.Arrow as Arrow

-- | This type describes some effect in @m@ which depends upon some value of
-- type @a@, for which the /output value/ is not of interest (only the effects).
--
-- The motivating use case is to describe tracing, logging, monitoring, and
-- similar features, in which the programmer wishes to provide some values to
-- some /other/ program which will do some real world side effect, such as
-- writing to a log file or bumping a counter in some monitoring system.
--
-- The actual implementation of such a program will probably work on rather
-- large, domain-agnostic types like @Text@, @ByteString@, JSON values for
-- structured logs, etc.
--
-- But the call sites which ultimately /invoke/ these implementations will deal
-- with smaller, domain-specific types that concisely describe events, metrics,
-- debug information, etc.
--
-- This difference is reconciled by the 'Contravariant' instance for 'Tracer'.
-- 'Data.Functor.Contravariant.contramap' is used to change the input type of
-- a tracer. This allows for a more general tracer to be used where a more
-- specific one is expected.
--
-- Intuitively: if you can map your domain-specific type @Event@ to a @Text@
-- representation, then any @Tracer m Text@ can stand in where a
-- @Tracer m Event@ is required.
--
-- > eventToText :: Event -> Text
-- >
-- > traceTextToLogFile :: Tracer m Text
-- >
-- > traceEventToLogFile :: Tracer m Event
-- > traceEventToLogFile = contramap eventToText traceTextToLogFile
--
-- Effectful tracers that actually do interesting stuff can be defined
-- using 'emit', and composed via 'contramap'.
--
-- The 'nullTracer' can be used as a stand-in for any tracer, doing no
-- side-effects and producing no interesting value.
--
-- To deal with branching, the arrow interface on the underlying
-- 'Control.Tracer.Arrow.Tracer' should be used. Arrow notation can be helpful
-- here.
--
-- For example, a common pattern is to trace only some variants of a sum type.
--
-- > data Event = This Int | That Bool
-- >
-- > traceOnlyThat :: Tracer m Int -> Tracer m Bool
-- > traceOnlyThat tr = Tracer $ proc event -> do
-- >   case event of
-- >     This i -> use tr  -< i
-- >     That _ -> squelch -< ()
--
-- The key point of using the arrow representation we have here is that this
-- tracer will not necessarily need to force @event@: if the input tracer @tr@
-- does not force its value, then @event@ will not be forced. To elaborate,
-- suppose @tr@ is @nullTracer@. Then this expression becomes
--
-- > classify (This i) = Left i
-- > classify (That _) = Right ()
-- >
-- > traceOnlyThat tr
-- > = Tracer $ Pure classify >>> (squelch ||| squelch) >>> Pure (either id id)
-- > = Tracer $ Pure classify >>> Pure (either (const (Left ())) (const (Right ()))) >>> Pure (either id id)
-- > = Tracer $ Pure (classify >>> either (const (Left ())) (const (Right ())) >>> either id id)
--
-- So that when this tracer is run by 'traceWith' we get
--
-- > traceWith (traceOnlyThat tr) x
-- > = traceWith (Pure _)
-- > = pure ()
--
-- It is _essential_ that the computation of the tracing effects cannot itself
-- have side-effects, as this would ruin the ability to short-circuit when
-- it is known that no tracing will be done: the side-effects of a branch
-- could change the outcome of another branch. This would fly in the face of
-- a crucial design goal: you can leave your tracer calls in the program so
-- they do not bitrot, but can also make them zero runtime cost by substituting
-- 'nullTracer' appropriately.
newtype Tracer m a = Tracer { runTracer :: Arrow.TracerA m a () }

instance Monad m => Contravariant (Tracer m) where
  contramap f tracer = Tracer (arr f >>> use tracer)

-- | @tr1 <> tr2@ will run @tr1@ and then @tr2@ with the same input.
instance Monad m => Semigroup (Tracer m s) where
  Tracer a1 <> Tracer a2 = Tracer (a1 &&& a2 >>> arr discard)
    where
    discard :: ((), ()) -> ()
    discard = const ()

instance Monad m => Monoid (Tracer m s) where
    mappend = (<>)
    mempty  = nullTracer

{-# INLINE traceWith #-}
-- | Run a tracer with a given input.
traceWith :: Monad m => Tracer m a -> a -> m ()
traceWith (Tracer tr) a = runKleisli (Arrow.runTracerA tr) a

-- | Inverse of 'use'.
arrow :: Arrow.TracerA m a () -> Tracer m a
arrow = Tracer

-- | Inverse of 'arrow'. Useful when writing arrow tracers which use a
-- contravariant tracer (the newtype in this module).
use :: Tracer m a -> Arrow.TracerA m a ()
use = runTracer

-- | A tracer which does nothing.
nullTracer :: Monad m => Tracer m a
nullTracer = Tracer Arrow.squelch

-- | Create a simple contravariant tracer which runs a given side-effect.
emit :: Applicative m => (a -> m ()) -> Tracer m a
emit f = Tracer (Arrow.emit f)

-- | Run a tracer only for the Just variant of a Maybe. If it's Nothing, the
-- 'nullTracer' is used (no output).
--
-- The arrow representation allows for proper laziness: if the tracer parameter
-- does not produce any tracing effects, then the predicate won't even be
-- evaluated. Contrast with the simple contravariant representation as
-- @a -> m ()@, in which the predicate _must_ be forced no matter what,
-- because it's impossible to know a priori whether that function will not
-- produce any tracing effects.
--
-- It's written out explicitly for demonstration. Could also use arrow
-- notation:
--
-- > traceMaybe p tr = Tracer $ proc a -> do
-- >   case k a of
-- >     Just b  -> use tr        -< b
-- >     Nothing -> Arrow.squelch -< ()
--
traceMaybe :: Monad m => (a -> Maybe b) -> Tracer m b -> Tracer m a
traceMaybe k tr = Tracer $ classify >>> (Arrow.squelch ||| use tr)
  where
  classify = arr (maybe (Left ()) Right . k)

-- | Uses 'traceMaybe' to give a tracer which emits only if a predicate is true.
squelchUnless :: Monad m => (a -> Bool) -> Tracer m a -> Tracer m a
squelchUnless p = traceMaybe (\a -> if p a then Just a else Nothing)

-- | Use a natural transformation to change the @m@ type. This is useful, for
-- instance, to use concrete IO tracers in monad transformer stacks that have
-- IO as their base.
natTracer :: forall m n s . (forall x . m x -> n x) -> Tracer m s -> Tracer n s
natTracer h (Tracer tr) = Tracer (Arrow.nat h tr)

-- | Trace strings to stdout. Output could be jumbled when this is used from
-- multiple threads. Consider 'debugTracer' instead.
stdoutTracer :: Tracer IO String
stdoutTracer = emit putStrLn

-- | Trace strings using 'Debug.Trace.traceM'. This will use stderr. See
-- documentation in "Debug.Trace" for more details.
debugTracer :: Applicative m => Tracer m String
debugTracer = emit traceM
