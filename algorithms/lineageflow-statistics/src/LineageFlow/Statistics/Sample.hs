module LineageFlow.Statistics.Sample
  ( nanApplyWith
  , projectWith
  , mean
  , meanWith
  , meanDSum
  , meanDSumWith
  , std
  , stdWith
  , stdDSum
  , meanStd
  , meanStdWith
  , meanStdDSum
  , meanStdDSumWith
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.ArrayU as ArrayU
import qualified Statistics.Sample as Sample
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

mean ::
  Array i Scalar -> Scalar
mean = meanWith id
{-# INLINE mean #-}

meanWith ::
  Storable a =>
  (a -> Scalar) -> Array i a -> Scalar
meanWith f = nanApplyWith f Sample.mean
{-# INLINE meanWith #-}

meanDSum ::
  (CFoldable f, Dom f (Array j Scalar)) =>
  (f .:. Array j) Scalar -> Scalar
meanDSum = meanDSumWith id
{-# INLINE meanDSum #-}

meanDSumWith ::
  (Storable a, CFoldable f, Dom f (Array j a)) =>
  (a -> Scalar) -> (f .:. Array j) a -> Scalar
meanDSumWith f =
  meanWith f . mkIx . ArrayU.concat . fmap unIx . _toList . getCompose
{-# INLINE meanDSumWith #-}

--------------------------------------------------------------------------------

std ::
  Array i Scalar -> Scalar
std = stdWith id
{-# INLINE std #-}

stdWith ::
  Storable a =>
  (a -> Scalar) -> Array i a -> Scalar
stdWith f = nanApplyWith f Sample.stdDev
{-# INLINE stdWith #-}

stdDSum ::
  (CFoldable f, Dom f (Array j Scalar)) =>
  (f .:. Array j) Scalar -> Scalar
stdDSum = stdDSumWith id
{-# INLINE stdDSum #-}

stdDSumWith ::
  Storable a =>
  (Storable a, CFoldable f, Dom f (Array j a)) =>
  (a -> Scalar) -> (f .:. Array j) a -> Scalar
stdDSumWith f =
  stdWith f . mkIx . ArrayU.concat . fmap unIx . _toList . getCompose
{-# INLINE stdDSumWith #-}

--------------------------------------------------------------------------------

meanStd ::
  Array i Scalar -> (Scalar, Scalar)
meanStd = meanStdWith id
{-# INLINE meanStd #-}

meanStdWith ::
  Storable a =>
  (a -> Scalar) -> Array i a -> (Scalar, Scalar)
meanStdWith f = over _2 sqrt . nanApplyWith f Sample.meanVarianceUnb
{-# INLINE meanStdWith #-}

meanStdDSum ::
  (CFoldable f, Dom f (Array j Scalar)) =>
  (f .:. Array j) Scalar -> (Scalar, Scalar)
meanStdDSum = meanStdDSumWith id
{-# INLINE meanStdDSum #-}

meanStdDSumWith ::
  (Storable a, CFoldable f, Dom f (Array j a)) =>
  (a -> Scalar) -> (f .:. Array j) a -> (Scalar, Scalar)
meanStdDSumWith f =
  meanStdWith f . mkIx . ArrayU.concat . fmap unIx . _toList . getCompose
{-# INLINE meanStdDSumWith #-}
