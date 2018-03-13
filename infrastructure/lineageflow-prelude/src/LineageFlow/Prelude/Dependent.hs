{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LineageFlow.Prelude.Dependent
  (
  -- * Maps from dependent sums
    DSumMap
  , DSumMapL
  , DSumMapA
  , DArray
  -- * Dependent sum transposition
  , DSumT
  , DSumTL
  , DSumTA
  , dSumMapTranspose
  , dSumMapIso
  -- * Transposition constraints
  , ConTranspose
  ) where

import BasePrelude
import LineageFlow.Types

import Control.Lens
import Control.Newtype
import Data.Indexed
import Control.ConstraintClasses

import Data.Functor.Compose

import LineageFlow.Prelude.Containers
import LATS ()

--------------------------------------------------------------------------------

type DSumMapL i j a = DSumMap List i Array j a
type DSumMapA i j a = DSumMap BArray i Array j a

type DArray i j a = Array (Dep i j) a

type DSumTL i j = DSumT List i Array j
type DSumTA i j = DSumT BArray i Array j

-- | Given an isomorphism between domains, the function transforms a measure in
-- one domain into the equivalent measure on the isomorphic domain. The most
-- common use is to change between the time point of view to the cell one.
dSumMapTranspose
  :: ConTranspose f g f' g' i j a
  => DSumT f' j g' i -- ^ one-way isomorphism
  -> DSumMap f i g j a -- ^ measure
  -> DSumMap f' j g' i a -- ^ converted measure on the isomorphic domain
dSumMapTranspose ji m = _fmap (mBArray !) ji
  where
    mBArray = Compose . mkIx . toBArrayU . getCompose $ m
{-# INLINE dSumMapTranspose #-}

-- | Isomorphism between measure spaces given the two one-way isomorphisms
dSumMapIso
  :: ( ConTranspose f g f' g' i j a
     , ConTranspose f' g' f g j i a )
  => DSumT f i g j -- ^ one-way isomorphism
  -> DSumT f' j g' i -- ^ one-way isomorphism
  -> Iso' (DSumMap f i g j a) (DSumMap f' j g' i a) -- ^ two-way 'Iso'
dSumMapIso ij ji = iso (dSumMapTranspose ji) (dSumMapTranspose ij)
{-# INLINE [2] dSumMapIso #-}
{-# RULES
  "iso/iso" [~2] forall tc ct. dSumMapIso tc ct . dSumMapIso ct tc = id
#-}

-- | Contraint for transposal
type ConTranspose f g f' g' i j a =
  ( Dom (f i .:. g (Dep i j)) a
  , Dom (f' j .:. g' (Dep j i)) a
  , Dom (f' j .:. g' (Dep j i)) (i, Dep i j)
  , CKey (g (Dep i j)) ~ Dep i j
  , CFunctor (f' j .:. g' (Dep j i))
  , ToBArrayU (f i)
  , Newtype i Int
  , CIndexable (g (Dep i j)) )
