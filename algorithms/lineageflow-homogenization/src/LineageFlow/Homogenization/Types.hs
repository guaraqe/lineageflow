{-# LANGUAGE RankNTypes #-}

module LineageFlow.Homogenization.Types
  (
    Homogenizer (..)
  , homogenize
  , SpaceHomogenizer
  , GraphHomogenizer
  , SpaceGraphHomogenizer
  , spaceHomogenizeScalar
  , spaceHomogenizeVector
  , spaceHomogenizeTensor
  , timeHomogenizeScalar
  , timeHomogenizeVector
  , timeHomogenizeTensor
  ) where

import LineageFlow.Prelude
import Control.DeepSeq (force)

--------------------------------------------------------------------------------

newtype Homogenizer i = Homogenizer (Array i Scalar -> Array i Scalar)

homogenize :: Homogenizer i -> Array i Scalar -> Array i Scalar
homogenize (Homogenizer f) = mkIx . force . unIx . f

type SpaceHomogenizer =
  DArray Time Cell Vector ->
  Homogenizer (Dep Time Cell)

type GraphHomogenizer =
  DArray Time (DS2 Time Cell) Vector ->
  Homogenizer (Dep Time Cell)

type SpaceGraphHomogenizer =
  DArray Time Cell Vector ->
  DArray Time (DS2 Time Cell) Vector ->
  Homogenizer (Dep Time Cell)

--------------------------------------------------------------------------------

spaceHomogenizeScalar ::
  SpaceHomogenizer ->
  TCMap Vector ->
  TCMap Scalar ->
  TCMap Scalar
spaceHomogenizeScalar h (Compose pos) (Compose tc) =
  let
    hom p = homogenize (h p)
  in
    Compose (_zipWith hom pos tc)
{-# INLINE spaceHomogenizeScalar #-}

spaceHomogenizeVector ::
  SpaceHomogenizer ->
  TCMap Vector ->
  TCMap Vector ->
  TCMap Vector
spaceHomogenizeVector h (Compose pos) (Compose tc) =
  let
    hom p = homogenize (h p)
  in
    Compose (_zipWith (\p -> applyThroughVector (hom p)) pos tc)
{-# INLINE spaceHomogenizeVector #-}

spaceHomogenizeTensor ::
  SpaceHomogenizer ->
  TCMap Vector ->
  TCMap Tensor ->
  TCMap Tensor
spaceHomogenizeTensor h (Compose pos) (Compose tc) =
  let
    hom p = homogenize (h p)
  in
    Compose (_zipWith (\p -> applyThroughTensor (hom p)) pos tc)
{-# INLINE spaceHomogenizeTensor #-}

--------------------------------------------------------------------------------

timeHomogenizeScalar ::
  TC -> CT -> Homogenizer (Dep Cell Time) -> TCMap Scalar -> TCMap Scalar
timeHomogenizeScalar tc ct h meas =
  let
    hom = homogenize h
  in
    over (tc2ct tc ct . composed) (_fmap hom) meas
{-# INLINE timeHomogenizeScalar #-}

timeHomogenizeVector ::
  TC -> CT -> Homogenizer (Dep Cell Time) -> TCMap Vector -> TCMap Vector
timeHomogenizeVector tc ct h meas =
  let
    hom = homogenize h
  in
    over (tc2ct tc ct . composed) (_fmap (applyThroughVector hom)) meas
{-# INLINE timeHomogenizeVector #-}

timeHomogenizeTensor ::
  TC -> CT -> Homogenizer (Dep Cell Time)-> TCMap Tensor -> TCMap Tensor
timeHomogenizeTensor tc ct h meas =
  let
    hom = homogenize h
  in
    over (tc2ct tc ct . composed) (_fmap (applyThroughTensor hom)) meas
{-# INLINE timeHomogenizeTensor #-}
