{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LineageFlow.Deviations.Utils
  ( transposeST
  , increments
  , msd
  , msd0
  , autocorrelation
  , autocorrelation0
  , averageTrajectoryS
  , averageTrajectoryV
  , averageTrajectoryT
  , zeroMean
  , zeroSpeed
  ) where

import LineageFlow.Prelude
import LineageFlow.Derivatives.Simple

import qualified LineageFlow.ArrayU as ArrayU

import qualified Data.List as List

import Data.Align
import Data.These

import Foreign.Storable.Record as Store

import Statistics.Sample (mean)

increments :: Array i Vector -> Array i Vector
increments = over ixed ArrayU.init . deriveV (simpleFw 1)
{-# INLINE increments #-}

--------------------------------------------------------------------------------

shiftWith :: Storable a => (a -> a -> Scalar) -> Array i a -> Array i Scalar
shiftWith f v =
  let
    g k = mean $ _zipWith f (unIx v) $ ArrayU.drop k (unIx v)
  in
    mkIx $ _fmap g $ ArrayU.enumFromTo 0 (_length v - 1)
{-# INLINE shiftWith #-}

shiftWith0 :: Storable a => (a -> a -> Scalar) -> Array i a -> Array i Scalar
shiftWith0 f v = mkIx $ _fmap (f (unIx v ! 0)) (unIx v)
{-# INLINE shiftWith0 #-}

--------------------------------------------------------------------------------

square :: Vector -> Scalar
square v = v !.! v
{-# INLINE square #-}

msd :: Array i Vector -> Array i Scalar
msd = shiftWith (\u v -> square (u - v))
{-# INLINE msd #-}

msd0 :: Array i Vector -> Array i Scalar
msd0 = shiftWith0 (\u v -> square (u - v))
{-# INLINE msd0 #-}

--------------------------------------------------------------------------------

zeroSpeed :: Array i Scalar -> Array i Scalar
zeroSpeed = \v ->
  let
    n = _length v - 1
    x = (unIx v ! n - unIx v ! 0) / fromIntegral n
  in
    if n == 0
      then fromList [0/0]
      else mkIx $ _imap (\k -> subtract (fromIntegral k * x)) (unIx v)
{-# INLINE zeroSpeed #-}

normalizeAuto :: Array i Scalar -> Array i Scalar
normalizeAuto v = mkIx $ _fmap (/ (unIx v ! 0)) (unIx v)
{-# INLINE normalizeAuto #-}

zeroMean :: Array i Scalar -> Array i Scalar
zeroMean = \v -> _fmap (subtract (mean v)) v
{-# INLINE zeroMean #-}

autocorrelation :: Array i Vector -> Array i Scalar
autocorrelation = shiftWith (!.!)
{-# INLINE autocorrelation #-}

autocorrelation0 :: Array i Vector -> Array i Scalar
autocorrelation0 = shiftWith0 (!.!)
{-# INLINE autocorrelation0 #-}

--------------------------------------------------------------------------------

transposeST ::
  Storable a =>
  DSumMap List i Array j a ->
  DSumMap List (Dep i j) Array i a
transposeST =
  Compose .
  mkIx .
  fmap fromList .
  List.transpose .
  fmap toList .
  unIx .
  getCompose
{-# INLINE transposeST #-}

--------------------------------------------------------------------------------

averageTrajectoryS :: [Array i Scalar] -> Array i Scalar
averageTrajectoryS = mergeTrajectoriesWith notNaNS nanS

averageTrajectoryV :: [Array i Vector] -> Array i Vector
averageTrajectoryV = mergeTrajectoriesWith notNaNV nanV

averageTrajectoryT :: [Array i Tensor] -> Array i Tensor
averageTrajectoryT = mergeTrajectoriesWith notNaNT nanT

mergeTrajectoriesWith ::
  (Dom (Array i) a, Fractional a) =>
  (a -> Bool) -> a -> [Array i a] -> Array i a
mergeTrajectoriesWith f x =
  _fmap (getSum . getWeightedWith (Sum x)) .
  foldl' (alignVectorWith (mergeThese (<>))) (fromList []) .
  fmap (_fmap (makeWeightedWith (f . getSum) . Sum))
{-# INLINE mergeTrajectoriesWith #-}

data Weighted a = Weighted !Int !a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

store :: Storable a => Store.Dictionary (Weighted a)
store = Store.run $
    Weighted
       <$> Store.element a
       <*> Store.element b
    where a (Weighted i _) = i
          b (Weighted _ j) = j

instance Storable a => Storable (Weighted a) where
    sizeOf = Store.sizeOf store
    alignment = Store.alignment store
    peek = Store.peek store
    poke = Store.poke store

instance Monoid a => Monoid (Weighted a) where
  mempty = Weighted 0 mempty
  mappend (Weighted w1 a1) (Weighted w2 a2) =
    Weighted (w1 + w2) (mappend a1 a2)

deriving instance Storable a => Storable (Sum a)
deriving instance Fractional a => Fractional (Sum a)

makeWeightedWith :: Monoid a => (a -> Bool) -> a -> Weighted a
makeWeightedWith f a =
  if f a
    then Weighted 1 a
    else mempty

getWeightedWith :: Fractional a => a -> Weighted a -> a
getWeightedWith x (Weighted n a) =
  if n == 0
    then x
    else a / fromIntegral n
