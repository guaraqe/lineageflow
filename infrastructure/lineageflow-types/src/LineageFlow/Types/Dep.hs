module LineageFlow.Types.Dep
  (
  -- * Dependent indexes
    Dep (..)
  , dep
  -- * Dependent sums
  , DSum
  -- * Maps from dependent sums
  , DSumMap
  -- * Dependent sum transposition
  , DSumT
  ) where

import Control.Lens
import Control.Newtype
import Foreign.Storable

import Data.Functor.Compose

-- | 'Dep' a newtype around the type @j@ representing that it is relative to
-- some type @i@. For example, the global index of a cell is represented by the
-- type @Cell@ and the global index representing time is @Time@. However, the
-- local cell index in a given time step is represented by @'Dep' Time Cell@ and
-- the local index of a time step for a given cell is given by @'Dep' Cell Time@.
-- This reduces the necessity for boilerplate data types that represent the
-- same entity in different situations.
newtype Dep i j = Dep { _dep :: j }
  deriving (Show, Ord, Eq, Storable)

instance Newtype j k => Newtype (Dep i j) k where
  pack = Dep . pack
  {-# INLINE pack #-}
  unpack = unpack . _dep
  {-# INLINE unpack #-}

-- | Lenses for the underlying type in 'Dep'.
$(makeLenses ''Dep)

-- | 'DSum' represents a dependent sum type. It is not used in practice and is
-- defined only as an example, the space being defined as the arrow to the
-- terminal object. The types @f@ and @g@ are supposed to be /list-like/
-- containers.
type DSum f g i j = DSumMap f g i j ()

-- | 'DSumMap' represents a map @'DSum' f g i j -> a@.
type DSumMap f i g j a = Compose (f i) (g (Dep i j)) a

-- | Very often dependent sums are transposable, in the sense that we can
-- change the order of the arguments without changing the semantics. The map
-- that goes from @'DSum' f g i j@ to @'DSum' f' g' j i@ is an element of
-- @'DSumT' f g i j@.
type DSumT f i g j = DSumMap f i g j (j,Dep j i)
