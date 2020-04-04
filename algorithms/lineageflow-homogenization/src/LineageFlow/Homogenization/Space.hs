{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Homogenization.Space
  (
    module LineageFlow.Homogenization.Types
  -- * Dense homogenization
  , spaceGaussianNormalizedDense
  , spaceGaussianDense
  -- * Sparse homogenization
  , spaceGaussianSparse
  , spaceGaussianNormalizedSparse
  ) where

import LineageFlow.Prelude

import LineageFlow.Homogenization.Types

import LineageFlow.Linear
import qualified LineageFlow.Linear.Raw as Matrix

import qualified Data.Vector.Generic.Mutable as MLinear
import qualified LineageFlow.Linear.Raw as Linear

--------------------------------------------------------------------------------

spaceGaussianDense :: Double -> SpaceHomogenizer
spaceGaussianDense sigma pos = Homogenizer $ \meas ->
  let
    coarse =
      expWithV (\v1 v2 -> coarseFunction sigma (norm (v1 - v2))) pos pos
    weighted = _fmap (\x -> if isNaN x then 0 else x) meas
  in
    coarse #> weighted

spaceGaussianNormalizedDense :: Double -> SpaceHomogenizer
spaceGaussianNormalizedDense sigma pos = Homogenizer $ \meas ->
  let
    weight = weightNaNScalar meas
    density = coarse #> weight
    coarse =
      expWithV (\v1 v2 -> coarseFunction sigma (norm (v1 - v2))) pos pos
    weighted = _zipWith (\x y -> if x == 0 then 0 else y) weight meas
  in
    _zipWith (/) (coarse #> weighted) density

--------------------------------------------------------------------------------

spaceGaussianSparse :: Double -> SpaceHomogenizer
spaceGaussianSparse sigma pos = Homogenizer $ \meas ->
  let
    coarse =
      expWithSparse (\v1 v2 -> coarseFunction sigma (norm (v1 - v2))) pos pos
    weighted = _fmap (\x -> if isNaN x then 0 else x) meas
  in
    sparseAssocTimesVector (_length pos) coarse weighted

spaceGaussianNormalizedSparse :: Double -> SpaceHomogenizer
spaceGaussianNormalizedSparse sigma pos = Homogenizer $ \meas ->
  let
    weight = weightNaNScalar meas
    density = sparseAssocTimesVector (_length pos) coarse weight
    coarse =
      expWithSparse (\v1 v2 -> coarseFunction sigma (norm (v1 - v2))) pos pos
    weighted = _fmap (\x -> if isNaN x then 0 else x) meas
  in
    _zipWith (/) (sparseAssocTimesVector (_length pos) coarse weighted) density

--------------------------------------------------------------------------------

weightNaNScalar :: Array i Scalar -> Array i Scalar
weightNaNScalar = _fmap (boolToDouble . notNaNS)
{-# INLINE weightNaNScalar #-}

boolToDouble :: Bool -> Double
boolToDouble True = 1
boolToDouble False = 0
{-# INLINE boolToDouble #-}

--------------------------------------------------------------------------------

expWithV ::
  (Storable a, Storable b, Matrix.Element c) =>
  (a -> b -> c) -> Array i a -> Array j b -> Matrix i j c
expWithV f v1 v2 =
 mkIx $ Matrix.reshape (_length v2) $ _liftA2 f (unIx v1) (unIx v2)
{-# INLINE expWithV #-}

expWithSparse ::
  (Storable a, Storable b, Ord i, Ord j, Newtype i Int, Newtype j Int) =>
  (a -> b -> Double) -> Array i a -> Array j b -> [((i,j),Double)]
expWithSparse f v1 v2 =
  let
    l1 = indexList . toList $ v1
    l2 = indexList . toList $ v2
  in
    filter (\x -> snd x > 0.001) $
      liftA2 (\(i,p1) (j,p2) -> ((i,j), f p1 p2)) l1 l2
{-# INLINE expWithSparse #-}

{-
expWithGraph ::
  (Storable a, Storable b, Storable i, Ord i, Newtype i Int) =>
  (a -> b -> Double) ->
  DArray Time (S2 i) (S2 i) ->
  Array i a ->
  Array i b ->
  Sparse i i Double
expWithGraph f cs v1 v2 =
  let
    ls = toList cs
    fun (S2 i j) =
      [ ((i,j), f (v1 ! i) (v2 ! j))
      , ((j,i), f (v1 ! j) (v2 ! i))
      ]
  in
    sparseFromList (concatMap fun ls)
{-# INLINE expWithGraph #-}
-}

coarseFunction :: Scalar -> Scalar -> Scalar
coarseFunction sigma = \x ->
  exp(- x ** 2 / sigma ** 2) / (sigma * sqrt(2*pi)) ** 3
{-# INLINE coarseFunction #-}

indexList :: Newtype i Int => [a] -> [(i,a)]
indexList = zip (fmap pack [0..])
{-# INLINE indexList #-}

--------------------------------------------------------------------------------

sparseAssocTimesVector ::
  ( Dom f ((i,j),Double)
  , CFoldable f, Newtype i Int, Newtype j Int) =>
  Int -> f ((i,j), Double) -> Array j Double -> Array i Double
sparseAssocTimesVector n vals v =
  mkIx $ Linear.create $ do
    u <- MLinear.replicate n 0
    _forM_ vals $ \((i,j),va) -> do
      let i' = unpack i; j' = unpack j
      MLinear.modify u (\k -> k + va * unIx v ! j') i'
    return u
{-# INLINE sparseAssocTimesVector #-}

