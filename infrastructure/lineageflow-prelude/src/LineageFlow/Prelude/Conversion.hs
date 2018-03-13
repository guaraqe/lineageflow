{-# LANGUAGE RankNTypes #-}

module LineageFlow.Prelude.Conversion
  ( t2c
  , c2t
  , tc2ct
  , ct2tc
  ) where

import BasePrelude

import LineageFlow.Prelude.PointsOfView
import LineageFlow.Prelude.Lineage
import LineageFlow.Prelude.Dependent

import Foreign.Storable.Tuple ()
import Control.Lens

--------------------------------------------------------------------------------

t2c :: Storable a => CT -> TCMap a -> CTMap a
t2c = dSumMapTranspose
{-# INLINE t2c #-}

c2t :: Storable a => TC -> CTMap a -> TCMap a
c2t = dSumMapTranspose
{-# INLINE c2t #-}

--------------------------------------------------------------------------------

tc2ct :: Storable a => TC -> CT -> Iso' (TCMap a) (CTMap a)
tc2ct = dSumMapIso
{-# INLINE tc2ct #-}

ct2tc :: Storable a => CT -> TC -> Iso' (CTMap a) (TCMap a)
ct2tc = dSumMapIso
{-# INLINE ct2tc #-}
