{-
  @license LITHE
  Versionable.purs
-}

module Proact.Versionable
  ( class GenericVersionable
  , class Versionable
  , class VersionableRecord
  , LWClock(..)
  , _clock
  , genericReconcile
  , genericReconcile'
  , reconcile
  , reconcileRecord
  )
where

import Control.Comonad (class Extend, class Comonad)
import Data.Lens (Lens', lens)
import Data.Generic.Rep
  ( class Generic
  , Argument(..)
  , Constructor(..)
  , NoArguments
  , NoConstructors
  , Product(..)
  , from
  , to
  )
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prelude
import Prim.Row (class Cons) as Row
import Prim.RowList (Cons) as RowList
import Prim.RowList (class RowToList, Nil)
import Record.Unsafe (unsafeGet, unsafeSet)
import Type.Data.RowList (RLProxy(..))

-- | Represents a versionable value abled to be `reconcile`d with another
-- | version of itself.
class Versionable a
  where
  -- | Merges two versions of a value into a reconciled version of it.
  reconcile :: a -> a -> a

-- | A `Generic` representation of a `Versionable` type class.
class GenericVersionable a
  where
  -- | A `Generic` implementation of `reconcile`.
  genericReconcile' :: a -> a -> a

-- | A class for records where all fields have `Versionable` instances, used to
-- | implement the `Versionable` instance for records.
class VersionableRecord rowlist row subrow | rowlist -> subrow where
  -- | A `Generic` implementation of `reconcile` for Records.
  reconcileRecord
    :: RLProxy rowlist -> Record row -> Record row -> Record subrow

-- | Represents a versionable value whose Last Written version is always
-- | preserved.
data LWClock a = LWClock Int a

-- LWClock :: Functor, Extend, Comonad, Versionable

derive instance lwclockFunctor :: Functor LWClock

instance lwclockExtend :: Extend LWClock
  where
  extend f clock@(LWClock version _) = LWClock version (f clock)

instance lwclockComonad :: Comonad LWClock
  where
  extract (LWClock _ a) = a

instance lwclockVersionable :: Versionable (LWClock a)
  where
  reconcile (LWClock v1 a1) (LWClock v2 a2) =
    if v2 < v1
    then LWClock v1 a1
    else LWClock v2 a2

-- Nil :: VersionableRecord

instance nilVersionableRecord :: VersionableRecord Nil row ()
  where
  reconcileRecord _ _ _ = { }

-- Record :: VersionableRecord

instance recordVersionableRecord
  ::
    ( IsSymbol key
    , Row.Cons key focus subrowTail subrow
    , VersionableRecord rowlistTail row subrowTail
    , Versionable focus
    )
  => VersionableRecord (RowList.Cons key focus rowlistTail) row subrow
  where
  reconcileRecord _ ra rb = insert (reconcile (get ra) (get rb)) tail
    where
      key = reflectSymbol (SProxy :: SProxy key)
      get = unsafeGet key :: Record row -> focus
      insert = unsafeSet key :: focus -> Record subrowTail -> Record subrow
      tail = reconcileRecord (RLProxy :: RLProxy rowlistTail) ra rb

-- Record :: Versionable

instance recordVersionable
  :: (RowToList row list, VersionableRecord list row row)
  => Versionable (Record row) where
  reconcile = reconcileRecord (RLProxy :: RLProxy list)

-- NoConstructors :: GenericVersionable

instance noConstructorsGenericVersionable :: GenericVersionable NoConstructors
  where
  genericReconcile' a _ = a

-- NoArguments :: GenericVersionable

instance noArgumentsGenericVersionable :: GenericVersionable NoArguments
  where
  genericReconcile' a _ = a

-- Product :: GenericVersionable

instance productGenericVersionable
  :: (GenericVersionable a, GenericVersionable b)
  => GenericVersionable (Product a b)
  where
  genericReconcile' (Product a1 b1) (Product a2 b2) =
    Product (genericReconcile' a1 a2) (genericReconcile' b1 b2)

-- Constructor :: GenericVersionable

instance constructorGenericVersionable
  :: GenericVersionable a => GenericVersionable (Constructor name a)
  where
  genericReconcile' (Constructor a1) (Constructor a2) =
    Constructor (genericReconcile' a1 a2)

-- Argument :: GenericVersionable

instance argumentGenericVersionable
  :: Versionable a => GenericVersionable (Argument a)
  where
  genericReconcile' (Argument a1) (Argument a2) = Argument (reconcile a1 a2)

-- | Gets or sets a versionable value.
_clock :: forall a . Lens' (LWClock a) a
_clock = lens get set
  where
  get (LWClock _ a) = a

  set (LWClock version _) a = LWClock (version + 1) a

-- | A `Generic` implementation of the `reconcile` member from the
-- | `Versionable` type class.
genericReconcile
  :: forall a rep . Generic a rep => GenericVersionable rep => a -> a -> a
genericReconcile a1 a2 = to (genericReconcile' (from a1) (from a2))
