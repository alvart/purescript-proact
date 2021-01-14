{-
  @license LITHE
  Event.purs
-}

module Proact.Event
  ( class Event
  , subscribe
  )
where

import Effect (Effect)
import Prelude (Unit)

-- | The `Event` type class notifies subscribers whenever a new event is
-- | triggered.
class Event e
  where
  -- | Registers a new subscriber.
  subscribe :: forall a . (a -> Effect Unit) -> e a -> Effect Unit
