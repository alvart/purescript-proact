{-
  @license MIT
  Proact.purs
-}

-- | Proact is a reactive framework that communicates with React through
-- | commands expressed as Free programs and extensible effects. These
-- | interactions provide access to a global single state that is composable via
-- | profunctor lenses.

module Proact
  ( (@=)
  , Component
  , EventHandler
  , IndexedComponent
  , PComponent
  , addListener
  , dispatch
  , focus
  , focus'
  , focusHandler
  , iFocus
  , proact
  , translate
  )
where

import Control.Apply (lift2)
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
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Prelude
import Proact.Event (subscribe)
import Proact.Event.Signal (SIGNAL, Signal, _signal)
import React (ReactThis, getState, writeStateWithCallback)
import Run (EFFECT, Run, lift, liftEffect, on, peel, run, runBaseEffect, send)
import Run.Reader (READER, Reader(..), _reader, runReader)
import Run.State (STATE, State(..), _state)
import Run.Writer (WRITER, Writer(..), _writer, runWriter, tell)
import Unsafe.Coerce (unsafeCoerce)

-- | A type synonym for a Proact Component.
type Component s e1 e2 = PComponent s s e1 e2

-- | A type synonym for Free programs that manipulate their component state
-- | through the `State` effect.
type EventHandler s e = Run (state :: STATE s | e)

-- | A type synonym for an Indexed Proact Component.
type IndexedComponent i s e1 e2 = PComponent (Tuple i s) s e1 e2

-- | A type synonym for Free programs with access to their component state that
-- | produce a continuous signal of Event Handlers.
type PComponent s t e1 e2 =
  Run
    ( reader :: READER s
    , writer :: WRITER (Signal (EventHandler t e2 Unit))
    | e1
    )

-- Represents applicatives as monoids and semigroups.
newtype Ap m a = Ap (m a)

-- Ap :: Newtype, Functor, Apply, Applicative, Semigroup, Monoid

derive instance newtypeAp :: Newtype (Ap m a) _
derive newtype instance functorAp :: (Functor m) => Functor (Ap m)
derive newtype instance applyAp :: (Apply m) => Apply (Ap m)
derive newtype instance applicativeAp :: (Applicative m) => Applicative (Ap m)

instance semigroupAp :: (Applicative m, Semigroup a) => Semigroup (Ap m a)
  where
  append = lift2 append

instance monoidAp :: (Apply m, Applicative m, Monoid a) => Monoid (Ap m a)
  where
  mempty = pure mempty

-- | An alias for `addListener`.
infixr 2 addListener as @=

-- | Adds an event listener in the context of a component.
addListener
  :: forall a s t e1 e2
   . Signal a -> (a -> EventHandler t e2 Unit) -> PComponent s t e1 e2 Unit
addListener signal = tell <<< flip map signal

-- | Runs the actions of an Event Handler asynchronously and returns the fiber.
dispatch
  :: forall s
   . ReactThis { } { | s }
  -> EventHandler { | s } (effect :: EFFECT, signal :: SIGNAL) Unit
  -> Effect Unit
dispatch this = runState this <<< runSignal
  where
  runSignal program =
    case peel program
    of
      Left command ->
        case on _signal Left Right command
        of
          Left signal -> liftEffect $ subscribe (dispatch this) signal
          Right resume -> send resume >>= runSignal
      Right a -> pure a

-- | Changes a `Component`'s state type through the lens of a `Traversal`.
-- | For a less restrictive albeit less general version, consider `focus'`.
focus
  :: forall s1 s2 e1 e2 a
   . Monoid a
  => Traversal' s1 s2 -> Component s2 e1 e2 a -> Component s1 e1 e2 a
focus _traversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< positions _traversal
    <<< Indexed
    <<< Forget
    $ map Ap \s ->
      do
      let index = fst s
      let _get = preview $ element index _traversal
      let _set = set $ element index _traversal
      component
        # focusSignal (focusState _get _set)
        # withReader snd
        # runReader s
        # unsafeCoerce

-- | Changes a `Component`'s state type through the focus of a `Lens`.
-- | For a more general albeit more restrictive version, consider `focus`.
focus'
  :: forall s1 s2 e1 e2
   . Lens' s1 s2 -> (Component s2 e1 e2 ~> Component s1 e1 e2)
focus' _lens component =
  join
    <<< lift _reader
    <<< Reader
    <<< unwrap
    <<< _lens
    $ Forget \s ->
      component
        # focusSignal (focusHandler _lens)
        # runReader s
        # unsafeCoerce

-- | Changes an Event Handler's state type through the focus of a `Lens`.
focusHandler
  :: forall s1 s2 e . Lens' s1 s2 -> (EventHandler s2 e ~> EventHandler s1 e)
focusHandler _lens = run (on _state handleState (unsafeCoerce <<< send))
  where
  _get = view _lens

  _set = set _lens

  handleState (State f2 next) = lift _state $ State f1 \s1 -> next $ _get s1
    where
    f1 s1 = flip _set s1 $ f2 $ _get s1

-- | Changes a `Component`'s state type through the lens of an indexed
-- | traversal.
iFocus
  :: forall s1 s2 i e1 e2 a
   . Monoid a
  => Index s1 i s2
  => IndexedTraversal' i s1 s2
  -> IndexedComponent i s2 e1 e2 a
  -> Component s1 e1 e2 a
iFocus _iTraversal component =
  join
    <<< lift _reader
    <<< Reader
    <<< map unwrap
    <<< unwrap
    <<< _iTraversal
    <<< Indexed
    <<< Forget
    $ map Ap \s ->
      do
      let index = fst s
      let _get = preview $ ix index :: s1 -> Maybe s2
      let _set = set $ ix index
      component
        # focusSignal (focusState _get _set)
        # runReader s
        # unsafeCoerce

-- | Extracts the value inside a Proact component.
proact
  :: forall s
   . ReactThis { } { | s }
  -> Component
     { | s } (effect :: EFFECT) (effect :: EFFECT, signal :: SIGNAL)
  ~> Effect
proact this component =
  do
  s <- getState this
  Tuple handlerSignal a <-
    component
      # runReader s
      # runWriter
      # runBaseEffect
  subscribe (dispatch this) handlerSignal
  pure a

-- | Translates the extensible effects of the Signal of Event Handlers of a
-- | Proact component.
translate
  :: forall s t e e1 e2
   . (EventHandler t e1 Unit -> EventHandler t e2 Unit)
  -> PComponent s t e e1
  ~> PComponent s t e e2
translate translator = withWriter (map translator)

-- Changes the type of the state through getter and setter functions.
focusState
  :: forall s1 s2 e a
   . Monoid a
  => (s1 -> Maybe s2)
  -> (s2 -> s1 -> s1)
  -> Run (state :: STATE s2 | e) a
  -> Run (state :: STATE s1 | e) a
focusState _get _set = run (on _state handleState (unsafeCoerce <<< send))
  where
  handleState (State f2 next) =
    lift _state $ State f1 \s1 ->
      maybe (pure mempty) identity $ map next $ _get s1
    where
    f1 s1 = maybe s1 identity $ map (flip _set s1 <<< f2) $ _get s1

-- Changes the type of a signal of Event Handlers through getter and setter
-- functions.
focusSignal
  :: forall s1 s2 e1 e2
   . (forall a . Monoid a => EventHandler s2 e2 a -> EventHandler s1 e2 a)
  -> Run (writer :: WRITER (Signal (EventHandler s2 e2 Unit)) | e1)
  ~> Run (writer :: WRITER (Signal (EventHandler s1 e2 Unit)) | e1)
focusSignal eventHandlerFocus = withWriter (map eventHandlerFocus)

-- Runs a stateful program inside a React context.
runState
  :: forall s
   . ReactThis { } { | s }
  -> Run (effect :: EFFECT, state :: STATE { | s }) Unit
  -> Effect Unit
runState this program =
  case peel program
  of
    Left command ->
      case on _state Left Right command
      of
        Left state -> handleState state
        Right resume -> runBaseEffect (send resume) >>= runState this
    Right a -> pure a
  where
  handleState (State f next) =
    do
    s <- getState this
    let s' = f s
    writeStateWithCallback this s' (runState this $ next s')

-- Changes the type of the context in a Reader effect.
withReader
  :: forall e s1 s2
   . (s1 -> s2)
  -> Run (reader :: READER s2 | e)
  ~> Run (reader :: READER s1 | e)
withReader f = run (on _reader handleReader (unsafeCoerce <<< send))
  where
  handleReader (Reader next) = lift _reader <<< Reader $ next <<< f

-- Changes the type of the context in a Writer effect.
withWriter
  :: forall e w1 w2
   . (w1 -> w2)
  -> Run (writer :: WRITER w1 | e)
  ~> Run (writer :: WRITER w2 | e)
withWriter f = run (on _writer handleWriter (unsafeCoerce <<< send))
  where
  handleWriter (Writer w1 next) = lift _writer $ Writer (f w1) next
