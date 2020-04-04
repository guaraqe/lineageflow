{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Prelude.Containers
  (
  -- * Composition
    type (.:.)
  , composed
  , ixed
  , forceCompose
  -- * List: indexed and unindexed
  , ListU
  , List
  -- * Array: indexed and unindexed
  , ArrayU
  , Array
  -- * BArray: indexed and unindexed
  , BArrayU
  , BArray
  -- * ToBArrayU
  , ToBArrayU (..)
  ) where

import BasePrelude

import Data.Indexed

import qualified LineageFlow.ArrayU as ArrayU
import qualified LineageFlow.BArrayU as BArrayU

import Control.ConstraintClasses

import Control.DeepSeq
import Control.Lens (Lens, over)
import Data.Functor.Compose

--------------------------------------------------------------------------------

infixr 7 .:.
type (.:.) f g = Compose f g

forceCompose :: NFData (f (g a)) => Compose f g a -> Compose f g a
forceCompose = over composed force

composed :: Lens (Compose f g a) (Compose f1 g1 a1) (f (g a)) (f1 (g1 a1))
composed f = \h -> fmap Compose (f . getCompose $ h)

ixed ::
  (Dom f a, Dom g b, HasIndexed f, HasIndexed g) =>
  Lens (Indexed f i a) (Indexed g j b) (f a) (g b)
ixed f = \h -> fmap mkIx (f . unIx $ h)

--------------------------------------------------------------------------------

-- | Type synonym for unindexed lists.
type ListU = []

-- | Type synonym for indexed lists.
type List = Indexed ListU

--------------------------------------------------------------------------------

-- | Type synonym for unindexed arrays (based on 'Data.Vector.Storable.Vector').
type ArrayU = ArrayU.Vector

-- | Type synonym for indexed arrays (based on 'Data.Vector.Storable.Vector').
type Array = Indexed ArrayU

--------------------------------------------------------------------------------

-- | Type synonym for indexed boxed arrays (based on 'Data.Vector.Vector').
type BArrayU = BArrayU.Vector

instance HasIndexed BArrayU where
  newtype (Indexed BArrayU) i a = BArray (BArrayU a)
  unIx (BArray l) = l
  mkIx = BArray

-- | Type synonym for unindexed boxed arrays (based on 'Data.Vector.Vector').
type BArray = Indexed BArrayU

--------------------------------------------------------------------------------

class ToBArrayU f where
  toBArrayU :: Dom f a => f a -> BArrayU a

instance ToBArrayU BArrayU where
  toBArrayU = id
  {-# INLINE toBArrayU #-}

instance ToBArrayU ListU where
  toBArrayU = BArrayU.fromList
  {-# INLINE toBArrayU #-}

instance (HasIndexed f, ToBArrayU f) => ToBArrayU ((Indexed f) i) where
  toBArrayU = toBArrayU . unIx
  {-# INLINE toBArrayU #-}
