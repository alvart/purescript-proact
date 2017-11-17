{-
  @license MIT
  Proact.purs
-}

-- | Proact is a set of utilities that leverage interactions with React through
-- | Free programs written with extensible effects. These interactions give
-- | access to a global singular state that is composable via Profunctor lenses.

module Proact
  ( Component
  , EventHandler
  , IndexedComponent
  , PComponent
  , PROACT
  , ProactElement
  , ProactF(..)
  , _proact
  , dispatch
  , dispatcher
  , focus
  , focus'
  , iFocus
  , proact
  )
where

import Control.Apply (lift2)
import Data.Array (head, singleton)
import Data.Either (Either(..))
import Data.Lens
  ( IndexedTraversal'
  , Traversal'
  , Forget(..)
  , Indexed(..)
  , Lens'
  , element
  , preview
  , set
  , view
  )
import Data.Lens.Index (class Index, ix)
import Data.Lens.Indexed (positions)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Prelude
import Proact.Functor.State (STATE, StateF(..), _state)
import React (ReactElement, ReactThis, getState, writeState)
import Run
  ( EFFECT
  , FProxy
  , Run
  , SProxy(..)
  , interpret
  , lift
  , liftEffect
  , on
  , peel
  , runBaseEffect
  , send
  )
import Run.Reader (READER, Reader(..), _reader, runReader)
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for a Proact Component.
type Component s e =
  Run (reader :: READER s, proact :: PROACT s e, state :: STATE s | e)

-- | A type synonym for Free programs that manipulate their component state
-- | through the `State` effect.
type EventHandler s e = Run (state :: STATE s | e)

-- | A type synonym for an Indexed Proact Component.
type IndexedComponent i s e =
  Run (reader :: READER (Tuple i s), proact :: PROACT s e, state :: STATE s | e)

-- A type synonym for Free programs with global access to its component state
-- and to an event dispatcher.
type PComponent s t e =
  Run (reader :: READER s, proact :: PROACT t e, state :: STATE t | e)

-- | A type synonym for the effects of the Proact component.
type PROACT s e = FProxy (ProactF s e)

-- | A type synonym for a React Element in the context of a Proact Component.
type ProactElement s t e = PComponent s t e ReactElement

-- | Represents the Functor container for the external interactions of a Proact
-- | component.
data ProactF s e a = Dispatcher ((EventHandler s e Unit -> Effect Unit) -> a)

-- | Represents applicatives as monoids and semigroups.
newtype Ap m a = Ap (m a)

-- Ap :: Newtype, Functor, Apply, Applicative, Semigroup, Monoid

derive instance newtypeAp :: Newtype (Ap m a) _
derive newtype instance functorAp :: (Functor m) => Functor (Ap m)
derive newtype instance applyAp :: (Apply m) => Apply (Ap m)
derive newtype instance applicativeAp :: (Applicative m) => Applicative (Ap m)

instance semigroupFreeT :: (Applicative m, Semigroup a) => Semigroup (Ap m a)
  where
  append = lift2 append

instance monoidFreeT :: (Apply m, Applicative m, Monoid a) => Monoid (Ap m a)
  where
  mempty = pure mempty

-- ProactF :: Functor

derive instance functorProactF :: Functor (ProactF s e)

-- | Gets the proxy symbol of the App component.
_proact :: SProxy "proact"
_proact = SProxy

-- | Runs the actions of an Event Handler.
dispatch
  :: forall s
   . ReactThis { } { | s }
  -> (EventHandler { | s } (effect :: EFFECT) ~> Effect)
dispatch this event = event # runEvent # runBaseEffect
  where
  runEvent
    :: Run (state :: STATE { | s }, effect :: EFFECT) ~> Run (effect :: EFFECT)
  runEvent = interpret (on _state handleEvent send)
    where
    handleEvent :: StateF { | s } ~> Run (effect :: EFFECT)
    handleEvent (Get next) =
      do
      s <- liftEffect $ getState this
      let a = next s
      pure a
    handleEvent (Modify next) =
      do
      s <- liftEffect $ getState this
      let Tuple s' a = next s
      liftEffect $ writeState this s'
      pure a

-- | Provides an action dispatcher in the context of a Proact `Component`.
dispatcher
  :: forall s t e . PComponent s t e (EventHandler t e Unit -> Effect Unit)
dispatcher = lift _proact $ Dispatcher identity

-- | Changes a `Component`'s state type through the lens of a `Traversal`.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall s1 s2 e a
   . Monoid a
  => Traversal' s1 s2 -> Component s2 e a -> Component s1 e a
focus _traversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< positions _traversal
    <<< Indexed
    <<< Forget
    <<< map Ap $ \s ->
      do
      let index = fst s
      let _get = preview $ element index _traversal
      let _set = set $ element index _traversal
      component
        # focusProact _get _set
        # focusState _get _set
        # withReader snd
        # runReader s
        # unsafeCoerce

-- | Changes a `Component`'s state type through the focus of a `Lens`.
-- | For a more general albeit more restrictive version, consider `focus`.
focus' :: forall s1 s2 e . Lens' s1 s2 -> (Component s2 e ~> Component s1 e)
focus' _lens component =
  join
    <<< lift _reader
    <<< Reader
    <<< unwrap
    <<< _lens
    <<< Forget $ \s ->
      do
      let _get = Just <<< view _lens
      let _set = set _lens
      component
        # map singleton
        # focusProact _get _set
        # focusState _get _set
        # runReader s
        # map (maybe (unsafeCoerce 0) identity <<< head)
        # unsafeCoerce

-- | Changes a `Component`'s state type through the lens of an indexed
-- | traversal.
iFocus
  :: forall s1 s2 i e a
   . Monoid a
  => Index s1 i s2
  => IndexedTraversal' i s1 s2 -> IndexedComponent i s2 e a -> Component s1 e a
iFocus _iTraversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< _iTraversal
    <<< Indexed
    <<< Forget
    <<< map Ap $ \s ->
      do
      let index = fst s
      let _get = preview $ ix index :: s1 -> Maybe s2
      let _set = set $ ix index
      component
        # focusProact _get _set
        # focusState _get _set
        # runReader s
        # unsafeCoerce

-- | Extracts the value inside a Proact component.
proact
  :: forall s
   . ReactThis { } { | s } -> (Component { | s } (effect :: EFFECT) ~> Effect)
proact this component =
  do
  s <- getState this
  component
    # runReader s
    # runProact
    # dispatch this
  where
  runProact
    :: forall r . Run (proact :: PROACT { | s } (effect :: EFFECT) | r) ~> Run r
  runProact = interpret (on _proact handleProact send)
    where
    handleProact :: ProactF { | s } (effect :: EFFECT) ~> Run r
    handleProact (Dispatcher next) = pure $ next (dispatch this)

-- Changes the type of the event dispatcher through getter and setter functions.
focusProact
  :: forall s1 s2 e r a
   . Monoid a
  => (s1 -> Maybe s2)
  -> (s2 -> s1 -> s1)
  -> Run (proact :: PROACT s2 e | r) a
  -> Run (proact :: PROACT s1 e | r) a
focusProact _get _set r =
  case peel r
  of
    Left r' ->
      case on _proact Left Right $ unsafeCoerce r'
      of
        Left f -> handleProact f
        Right r'' -> send r'' >>= focusProact _get _set
    Right a -> pure a
  where
  handleProact (Dispatcher next) =
    join <<< lift _proact <<< Dispatcher $ \d1 ->
      focusProact _get _set $ next (d1 <<< focusState _get _set)

-- Changes the type of the state through getter and setter functions.
focusState
  :: forall s1 s2 e a
   . Monoid a
  => (s1 -> Maybe s2)
  -> (s2 -> s1 -> s1)
  -> Run (state :: STATE s2 | e) a
  -> Run (state :: STATE s1 | e) a
focusState _get _set r =
  case peel r
  of
    Left r' ->
      case on _state Left Right $ unsafeCoerce r'
      of
        Left f -> handleState f
        Right r'' -> send r'' >>= focusState _get _set
    Right a -> pure a
  where
  handleState (Get next) =
    join <<< lift _state <<< Get $ \s1 ->
      maybe (pure mempty) identity $
        do
        r' <- map next $ _get s1
        pure $ focusState _get _set r'
  handleState (Modify next) =
    join <<< lift _state <<< Modify $ \s1 ->
      maybe (Tuple s1 $ pure mempty) identity $
        do
        Tuple s2 r' <- map next $ _get s1
        pure <<< Tuple (_set s2 s1) $ focusState _get _set r'

-- Changes the type of the context in a Reader effect.
withReader
  :: forall r s1 s2
   . (s1 -> s2)
  -> (Run (reader :: READER s2 | r) ~> Run (reader :: READER s1 | r))
withReader _f r =
  case peel r
  of
    Left r' ->
      case on _reader Left Right $ unsafeCoerce r'
      of
        Left f -> handleReader f
        Right r'' -> send r'' >>= withReader _f
    Right a -> pure a
  where
  handleReader (Reader next) =
    join <<< lift _reader <<< Reader $ withReader _f <<< next <<< _f
