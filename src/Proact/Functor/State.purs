{-
  @license MIT
  State.purs
-}

module Proact.Functor.State
where

import Data.Either (Either(..))
import Data.Lens.Setter (Setter, Setter', over, set)
import Data.Lens.Getter (Getter, view)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd, uncurry)
import Prelude
import Run (FProxy, Run, SProxy(..), lift, on, peel, send)
import Unsafe.Coerce (unsafeCoerce)

infix 4 assign as .=
infix 4 modifying as %=
infix 4 addModifying as +=
infix 4 mulModifying as *=
infix 4 subModifying as -=
infix 4 divModifying as //=
infix 4 disjModifying as ||=
infix 4 conjModifying as &&=
infix 4 appendModifying as <>=
infix 4 appendModifying as ++=
infix 4 assignJust as ?=

-- | Represents the Functor container for the external interactions of a
-- | stateful program.
data StateF s a =
  Get (s -> a)
  | Modify (s -> Tuple s a)

-- | A type synonym for the effects of a stateful program.
type STATE s = FProxy (StateF s)

-- StateF :: Functor
derive instance functorStateF :: Functor (StateF s)

-- | Gets the proxy symbol of the App component.
_state :: SProxy "state"
_state = SProxy

-- | Adds to the foci of a `Setter`.
addModifying
  :: forall r s a
   . Semiring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
addModifying p = modifying p <<< add

-- | Appends to the foci of a `Setter`.
appendModifying
  :: forall r s a
   . Semigroup a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
appendModifying p = modifying p <<< flip append

-- | Sets the foci of a `Setter` to a constant value.
assign
  :: forall r s a b . Setter s s a b -> b -> Run (state :: STATE s | r) Unit
assign p b = modify (set p b)

-- | Assigns to the nullable foci of a `Setter`.
assignJust
  :: forall r s a b
   . Setter s s a (Maybe b) -> b -> Run (state :: STATE s | r) Unit
assignJust p = assign p <<< Just

-- | `and`s to the foci of a `Setter`.
conjModifying
  :: forall r s a
   . HeytingAlgebra a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
conjModifying p = modifying p <<< flip conj

-- | `or`s to the foci of a `Setter`.
disjModifying
  :: forall r s a
   . HeytingAlgebra a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
disjModifying p = modifying p <<< flip disj

-- | Divides to the foci of a `Setter`.
divModifying
  :: forall r s a
   . EuclideanRing a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
divModifying p = modifying p <<< flip div

-- | Gets the current state.
get :: forall r s . Run (state :: STATE s | r) s
get = lift _state $ Get identity

-- | Gets a value which depends on the current state.
gets :: forall r s a . (s -> a) -> Run (state :: STATE s | r) a
gets f = lift _state $ Get f

-- | Updates the value of a mutable reference by applying a function to the
-- | current value.
modify :: forall r s . (s -> s) -> Run (state :: STATE s | r) Unit
modify f = lift _state $ Modify ((\s -> Tuple s unit) <<< f)

-- | Modifies the foci of a `Setter`.
modifying
  :: forall r s a b
   . Setter s s a b -> (a -> b) -> Run (state :: STATE s | r) Unit
modifying p f = modify (over p f)

-- | Multiplies to the foci of a `Setter`.
mulModifying
  :: forall r s a
   . Semiring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
mulModifying p = modifying p <<< flip mul

-- | Runs a stateful program and returns the result discarding the final state.
evalState :: forall r s a . s -> Run (state :: STATE s | r) a -> Run r a
evalState s = map snd <<< runState s

-- | Runs a stateful program and returns the final state discarding the result.
execState :: forall r s a . s -> Run (state :: STATE s | r) a -> Run r s
execState s = map fst <<< runState s

-- | Runs a stateful program and returns the result and the final state.
runState
  :: forall r s a . s -> Run (state :: STATE s | r) a -> Run r (Tuple s a)
runState s r =
  case peel r
  of
    Left r' ->
      case on _state Left Right $ unsafeCoerce r'
      of
        Left f -> handleState f
        Right r'' -> send r'' >>= runState s
    Right a -> pure (Tuple s a)
  where
  handleState (Get next) = runState s $ next s
  handleState (Modify next) = uncurry runState $ next s

-- | Subtracts to the foci of a `Setter`.
subModifying
  :: forall r s a
   . Ring a => Setter' s a -> a -> Run (state :: STATE s | r) Unit
subModifying p = modifying p <<< flip sub

-- | Views the focus of a `Getter`.
use :: forall r s t a b . Getter s t a b -> Run (state :: STATE s | r) a
use p = gets (view p)
