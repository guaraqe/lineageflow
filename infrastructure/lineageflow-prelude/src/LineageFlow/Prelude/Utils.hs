module LineageFlow.Prelude.Utils
  (
  -- * Generalization
    applyThroughVector
  , applyThroughTensor
  -- * NaN manipulation
  , nanS
  , nanV
  , nanT
  , isNaNS
  , isNaNV
  , isNaNT
  , notNaNS
  , notNaNV
  , notNaNT
  ) where

import BasePrelude
import LineageFlow.Types

import Control.ConstraintClasses

--------------------------------------------------------------------------------

applyThroughVector ::
  (Dom f (V3 a), Dom f a, CZip f) =>
  (f a -> f a) -> f (V3 a) -> f (V3 a)
applyThroughVector f = _seq . fmap f .  __distribute
   where
     _seq (V3 a b c) = _zipWith3 V3 a b c

applyThroughTensor ::
  ( Dom f (V3 (V3 a)) , Dom f (V3 a), Dom f a, CZip f) =>
  (f a -> f a) -> f (V3 (V3 a)) -> f (V3 (V3 a))
applyThroughTensor f = _seq . fmap (applyThroughVector f) . __distribute
   where
     _seq (V3 a b c) = _zipWith3 V3 a b c

--------------------------------------------------------------------------------

nanS :: Scalar
nanS = 0/0

nanV :: Vector
nanV = V3 nanS nanS nanS

nanT :: Tensor
nanT = V3 nanV nanV nanV

isNaNS :: Scalar -> Bool
isNaNS = isNaN

isNaNV :: Vector -> Bool
isNaNV = getAny . foldMap (Any . isNaN)

isNaNT :: Tensor -> Bool
isNaNT = getAny . foldMap (foldMap (Any . isNaN))

notNaNS :: Scalar -> Bool
notNaNS = not . isNaN

notNaNV :: Vector -> Bool
notNaNV = getAll . foldMap (All . not . isNaN)

notNaNT :: Tensor -> Bool
notNaNT = getAll . foldMap (foldMap (All . not . isNaN))
