{-
  @license LITHE
  Signal.purs
-}

-- | This module introduces the `Signal` data type which serves as the building
-- | block of Functional Reactive Programs.
-- |
-- | Every `Event` can be transformed into a `Signal` via the `replicate`
-- | function and once this is done, the stream of signals can be manipulated
-- | through primitive functions such as mapping, filtering and folding or
-- | through more complex operations derived from these.
-- |
-- | Signals can be composed together as monoids or monads, in both cases they
-- | are analogous to lists, where `append` would serve as concatenation and
-- | `join` as flattening.

module Proact.Event.Signal
  ( SIGNAL
  , Signal(..)
  , _signal
  , await
  , debounce
  , filter
  , filterMap
  , foldp
  , receive
  , replicate
  , trigger
  )
where

import Control.MonadZero (guard)
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Now (now)
import Effect.Ref (modify, new)
import Prelude
import Proact.Event (class Event, subscribe)
import Run (FProxy, Run, SProxy(..), lift)

-- | A type synonym for Free programs that emit observable signals.
type SIGNAL = FProxy Signal

-- | Represents asynchronous events that emit observable signals.
newtype Signal a = Signal ((a -> Effect Unit) -> Effect Unit)

-- Signal :: Semigroup, Monoid, Functor, Apply, Applicative, Bind, Monad, Event

derive newtype instance semigroupSignal :: Semigroup (Signal a)
derive newtype instance monoidSignal :: Monoid (Signal a)

instance functorSignal :: Functor Signal
  where
  map fn signal = Signal \observer -> subscribe (observer <<< fn) signal

instance applySignal :: Apply Signal
  where
  apply = ap

instance applicativeSignal :: Applicative Signal
  where
  pure a = Signal (_ $ a)

instance bindSignal :: Bind Signal
  where
  bind signal fn =
    Signal \observer -> subscribe (subscribe observer <<< fn) signal

instance monadSignal :: Monad Signal

instance eventSignal :: Event Signal
  where
  subscribe observer (Signal signal) = signal observer

-- | Gets the proxy symbol of the Signal effect.
_signal :: SProxy "signal"
_signal = SProxy

-- | Emits a signal when an asynchronous operation completes. An error won't
-- | fire the signal.
await :: Aff ~> Signal
await aff = Signal $ flip runAff_ aff <<< either (const $ pure unit)

-- | Limits the rate at which a signal is fired.
debounce :: Milliseconds -> Signal ~> Signal
debounce (Milliseconds interval) =
  filterMap (_ >>= debounce_)
    <<< foldp (map <<< annotate) Nothing
    <<< trigger
    <<< map stamp
  where
  annotate signal last = signal { interval = signal.timestamp - last.timestamp }

  stamp signal =
    do
    Milliseconds timestamp <- map unInstant now
    pure { value : signal, timestamp, interval }

  debounce_ signal =
    do
    guard $ signal.interval >= interval
    pure signal.value

-- | Filters out signals that don't match the given predicate.
filter :: forall a . (a -> Boolean) -> Signal a -> Signal a
filter predicate = filterMap maybeBool
  where
  maybeBool a =
    if predicate a
    then Just a
    else Nothing

-- | Filters out signals that return `Nothing` when a predicate function is
-- | applied to them.
filterMap :: forall a b . (a -> Maybe b) -> Signal a -> Signal b
filterMap predicate signal =
  Signal \observer ->
    subscribe (maybe (pure unit) observer) $ map predicate signal

-- | Fires a past dependent signal whenever another signal is fired.
foldp :: forall a b . (a -> b -> b) -> b -> Signal a -> Signal b
foldp fn seed signal =
  Signal \observer ->
    do
    bRef <- new seed
    flip subscribe signal \a ->
      do
      b <- modify (fn a) bRef
      observer b

-- | Subscribes to a given signal.
receive :: forall e . Signal ~> Run (signal :: SIGNAL | e)
receive = lift _signal

-- | Replicates the signals fired from an `Event`.
replicate :: forall e . Event e => e ~> Signal
replicate event = Signal $ flip subscribe event

-- | Triggers the `Effect` of each fired signal.
trigger :: forall a . Signal (Effect a) -> Signal a
trigger (Signal signal) = Signal $ signal <<< (=<<)
