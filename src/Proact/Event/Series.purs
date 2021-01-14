{-
  @license LITHE
  Series.purs
-}

-- | This module introduces the `Series` data type which generalizes a series of
-- | asynchronous `Aff` operations as a stream of events that may happen in the
-- | future. Series can be composed together as monoids.
-- |
-- | Series always emit the latest event available which depends on the time the
-- | asynchronous operation was started. For example, given the three requests
-- | A, B and C, assume they are dispatched in that same order but arrive A, C
-- | and B. In that scenario, the results of A and C would be notified to the
-- | subscribers of the series but the one from B would be discarded.

module Proact.Event.Series
  ( Series
  , empty
  , kill
  , mute
  , serialize
  )
where

import Data.DateTime.Instant (unInstant)
import Data.Either (Either, hush)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, attempt, joinFiber, killFiber, launchAff)
import Effect.Aff (error) as Aff
import Effect.Now (now)
import Prelude
import Proact.Event (class Event, subscribe)
import Proact.Event.Signal (Signal(..), await, filterMap)

-- | Represents a series of asynchronous operations, ordered by the time they
-- | were dispatched.
data Series a = Nothing | Series Number (Fiber a) (Signal (Either Error a))

-- Series :: Functor, Semigroup, Monoid, Event

instance functorSeries :: Functor Series
  where
  map _ Nothing = Nothing
  map fn (Series timestamp fiber signal) =
    Series timestamp (map fn fiber) (map (map fn) signal)

instance semigroupSeries :: Semigroup (Series a)
  where
  append Nothing series = series
  append series Nothing = series
  append
    (Series timestampA fiberA signalA)
    (Series timestampB fiberB signalB) =
    if timestampB > timestampA
    then append_ timestampB fiberA signalA fiberB signalB
    else append_ timestampA fiberB signalB fiberA signalA
    where
    append_ timestamp fiber1 signal1 fiber2 signal2 =
      Series timestamp fiber2 $ Signal \observer ->
        do
        subscribe (overwrite observer) signal2
        subscribe observer signal1
        where
        overwrite observer response =
          subscribe (const $ observer response)
            <<< await
            $ killFiber (Aff.error "Dismissing older request") fiber1

instance monoidSeries :: Monoid (Series a)
  where
  mempty = Nothing

instance eventSeries :: Event Series
  where
  subscribe _ Nothing = pure unit
  subscribe observer (Series _ _ signal) =
    subscribe observer $ filterMap hush signal

-- | Checks whether there is at least one running request.
empty :: forall a . Series a -> Boolean
empty Nothing = true
empty _ = false

-- | Kills all the running requests of a `Series`.
kill :: forall a . Error -> Series a -> Aff Unit
kill _ Nothing = pure unit
kill error (Series _ fiber _) = killFiber error fiber

-- | Blocks signals to subscribers of the new `Series` without cancelling the
-- | previous asynchronous request. Useful when composing new events from
-- | existing series that may have already been subscribed to.
mute :: Series ~> Series
mute Nothing = Nothing
mute (Series timestamp fiber _) = Series timestamp fiber mempty

-- | Launches an asynchronous operation in a `Series`.
serialize :: forall a . Aff a -> Effect (Series a)
serialize aff =
  do
  Milliseconds timestamp <- map unInstant now
  fiber <- launchAff aff
  let signal = await <<< attempt $ joinFiber fiber
  pure $ Series timestamp fiber signal
