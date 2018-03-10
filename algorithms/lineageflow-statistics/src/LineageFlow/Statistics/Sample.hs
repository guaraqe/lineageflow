module LineageFlow.Statistics.Sample
  ( nanApplyWith
  , projectWith
  , meanWith
  , meanDSumWith
  , meanStdWith
  , meanStdDSumWith
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.ArrayU as ArrayU
import Statistics.Sample (meanVarianceUnb, mean)
import Control.Lens (_2)

--------------------------------------------------------------------------------

nanApplyWith ::
  (Dom (Array i) a, Dom (Array i) b, RealFloat b) =>
  (a -> b) -> (ArrayU b -> c) -> Array i a -> c
nanApplyWith f g = \x ->
  g ((ArrayU.filter (not . isNaN) . _fmap f) (unIx x))
{-# INLINE nanApplyWith #-}

--------------------------------------------------------------------------------

projectWith ::
  (Dom (f .:. g) a, CFunctor f, Dom f b)  =>
  (g a -> b) -> (f .:. g) a -> f b
projectWith f = _fmap f . getCompose
{-# INLINE projectWith #-}

--------------------------------------------------------------------------------

meanWith ::
  Storable a =>
  (a -> Scalar) -> Array i a -> Scalar
meanWith f = nanApplyWith f mean
{-# INLINE meanWith #-}

meanDSumWith ::
  (Storable a, CFoldable f, Dom f (Array j a)) =>
  (a -> Scalar) -> (f .:. Array j) a -> Scalar
meanDSumWith f =
  meanWith f . mkIx . ArrayU.concat . fmap unIx . _toList . getCompose
{-# INLINE meanDSumWith #-}

meanStdWith ::
  Storable a =>
  (a -> Scalar) -> Array i a -> (Scalar, Scalar)
meanStdWith f = over _2 sqrt . nanApplyWith f meanVarianceUnb
{-# INLINE meanStdWith #-}

meanStdDSumWith ::
  (Storable a, CFoldable f, Dom f (Array j a)) =>
  (a -> Scalar) -> (f .:. Array j) a -> (Scalar, Scalar)
meanStdDSumWith f =
  meanStdWith f . mkIx . ArrayU.concat . fmap unIx . _toList . getCompose
{-# INLINE meanStdDSumWith #-}
