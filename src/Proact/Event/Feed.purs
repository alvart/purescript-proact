{-
  @license LITHE
  Feed.purs
-}

-- | This module introduces the `Feed` data type which is useful as a
-- | communication channel between two components in which one sends messages to
-- | the other that acts as a subscriber.

module Proact.Event.Feed
  ( Feed
  , feed
  , notify
  )
where

import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Prelude (Unit)
import Proact.Event (class Event)

-- | Represents a classic implementation of the Observer pattern that notifies
-- | new events to a list of subscribers.
foreign import data Feed :: Type -> Type

-- Feed :: Event

instance eventFeed :: Event Feed
  where
  subscribe = runEffectFn2 _subscribe

-- | Constructs a new `Feed` with no subscribers.
feed :: forall a . Effect (Feed a)
feed = _feed

-- | Notifies all subscribers about a new event.
notify :: forall a . a -> Feed a -> Effect Unit
notify = runEffectFn2 _notify

-- Constructs a new `Feed` with no subscribers.
foreign import _feed :: forall a . Effect (Feed a)

-- Notifies all subscribers about a new event.
foreign import _notify :: forall a . EffectFn2 a (Feed a) Unit

-- Adds a new subscriber to the `Feed`.
foreign import _subscribe
  :: forall a . EffectFn2 (a -> Effect Unit) (Feed a) Unit
