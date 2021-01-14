{-
  @license LITHE
  Single.purs
-}

-- | This module introduces the `Single` data type which generalizes a series of
-- | asynchronous `Aff` operations as a stream of events that may happen in the
-- | future. Singles can be composed together as monoids.
-- |
-- | As the name implies, single requests only allow one asynchronous operation
-- | to be running at once. Whenever a new request is introduced the previous
-- | operation is killed off if it hadn't completed yet.

module Proact.Event.Single
  ( Single
  , empty
  , kill
  , push
  )
where

import Effect (Effect)
import Effect.Aff (Aff, Error, Fiber, joinFiber, killFiber, launchAff)
import Effect.Aff (error) as Aff
import Prelude
import Proact.Event (class Event, subscribe)
import Proact.Event.Signal (Signal(..), await)

-- | Represents an asynchronous operation that replaces previous requests on new
-- | subscriptions.
data Single a = Nothing | Single (Fiber a) (Signal a)

-- Single :: Functor, Semigroup, Monoid, Event

instance functorSingle :: Functor Single
  where
  map _ Nothing = Nothing
  map fn (Single fiber signal) = Single (map fn fiber) (map fn signal)

instance semigroupSingle :: Semigroup (Single a)
  where
  append Nothing single = single
  append single Nothing = single
  append singleA (Single fiberB signalB) =
    Single fiberB $ Signal \observer ->
      subscribe (const $ subscribe observer signalB)
        <<< await
        $ kill (Aff.error "Dismissing older request") singleA

instance monoidSingle :: Monoid (Single a)
  where
  mempty = Nothing

instance eventSingle :: Event Single
  where
  subscribe _ Nothing = pure unit
  subscribe observer (Single _ signal) = subscribe observer signal

-- | Checks whether there is a running request.
empty :: forall a . Single a -> Boolean
empty Nothing = true
empty _ = false

-- | Kills the current request of a `Single`.
kill :: forall a . Error -> Single a -> Aff Unit
kill _ Nothing = pure unit
kill error (Single fiber _) = killFiber error fiber

-- | Launches a `Single` asynchronous operation.
push :: forall a . Aff a -> Effect (Single a)
push aff =
  do
  fiber <- launchAff aff
  let signal = await $ joinFiber fiber
  pure $ Single fiber signal
