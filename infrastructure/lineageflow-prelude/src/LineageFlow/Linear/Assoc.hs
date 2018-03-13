{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module LineageFlow.Linear.Assoc
  ( Assoc (..)
  , assoc_rows
  , assoc_cols
  , assoc_vals
  , assocEmpty
  , addToAssoc
  , assocFromList
  , addBounds
  , assocTimesVector
  , simplexAssocSym
  , simplexAssocSymDim
  , simplexAssocAsym
  , simplexAssocAsymDim
  ) where

import LineageFlow.Containers
import LineageFlow.Types.Simplex

import BasePrelude hiding (maximum, fold, any)
import Control.Newtype (Newtype (..))
import Data.Indexed
import Control.ConstraintClasses

import qualified LATS.Vector.Mutable as MLinear
import qualified LineageFlow.Linear.Raw as Linear

import Control.Lens hiding (Fold)

import Control.Foldl hiding (head)

--------------------------------------------------------------------------------

data Assoc i j a = Assoc
  { _assoc_rows :: !Int
  , _assoc_cols :: !Int
  , _assoc_vals :: ![((i,j),a)]
  } deriving (Show, Functor, Foldable, Traversable)

$(makeLenses ''Assoc)

assocEmpty :: Int -> Int -> Assoc i j a
assocEmpty i j = Assoc i j []

--------------------------------------------------------------------------------

addToAssoc ::
  i -> j -> a -> Assoc i j a -> Assoc i j a
addToAssoc i j a = over assoc_vals $ addToAssocVals i j a
{-# INLINE addToAssoc #-}

addToAssocVals ::
  i -> j -> a -> [((i,j),a)] -> [((i,j),a)]
addToAssocVals i j a = (((i,j),a):)
{-# INLINE addToAssocVals #-}

--------------------------------------------------------------------------------

foldRow ::
  (Ord i, Newtype i Int) => Fold ((i,j),a) Int
foldRow = dimap (fst . fst) (maybe 0 ((+1) . unpack)) maximum
{-# INLINABLE foldRow #-}

foldCol ::
  (Ord j, Newtype j Int) => Fold ((i,j),a) Int
foldCol = dimap (snd . fst) (maybe 0 ((+1) . unpack)) maximum
{-# INLINABLE foldCol #-}

foldVal ::
  Fold ((i,j),a) [((i,j),a)]
foldVal = Fold (\m ((i,j),a) -> addToAssocVals i j a m) [] id
{-# INLINABLE foldVal #-}

assocFromList ::
  (Newtype i Int, Newtype j Int, Ord i, Ord j) =>
  [((i,j),a)] -> Assoc i j a
assocFromList = fold (Assoc <$> foldRow <*> foldCol <*> foldVal)
{-# INLINE assocFromList #-}

--------------------------------------------------------------------------------

addBounds ::
  (Newtype i Int, Newtype j Int, Eq i, Eq j, Num a) =>
  Int -> Int -> [((i,j),a)] -> [((i,j),a)]
addBounds i j = \vals ->
  vals ++
    if fold ((&&) <$> rows <*> cols) vals
      then []
      else [((pack $ i-1, pack $ j-1),0)]
  where
    rows = lmap (fst . fst) (any (== (pack $ i - 1)))
    cols = lmap (snd . fst) (any (== (pack $ j - 1)))

--------------------------------------------------------------------------------

assocTimesVector ::
  (Newtype i Int, Newtype j Int) =>
  Assoc i j Double -> Array j Double -> Array i Double
assocTimesVector (Assoc n m vals) (Index v) =
  if _length v /= m
    then error "assocTimesVector: bad dimensions"
    else
      Index $ Linear.create $ do
        u <- MLinear.replicate n 0
        _forM_ vals $ \((i,j),va) -> do
          let i' = unpack i; j' = unpack j
          MLinear.modify u (\k -> k + va * v ! j') i'
        return u
{-# INLINE assocTimesVector #-}

--------------------------------------------------------------------------------

simplexAssocSym ::
  ( Dom f (S2 i), Dom f Double, Dom f (S2 i, Double)
  , CFoldable f, CZip f, Ord i, Newtype i Int) =>
  f (S2 i) -> f Double -> Assoc i i Double
simplexAssocSym neigh vals =
  assocFromList .
  concatMap (\(S2 i j, a) -> [((i,j),a), ((j,i),a)]) .
  _toList $ _zip neigh vals
{-# INLINE simplexAssocSym #-}

simplexAssocSymDim ::
  ( Dom f (S2 i), Dom f Double, Dom f (S2 i, Double)
  , CFoldable f, CZip f) =>
  Int -> Int -> f (S2 i) -> f Double -> Assoc i i Double
simplexAssocSymDim n m neigh vals =
  Assoc n m .
  concatMap (\(S2 i j, a) -> [((i,j),a), ((j,i),a)]) .
  _toList $ _zip neigh vals
{-# INLINE simplexAssocSymDim #-}

simplexAssocAsym ::
  ( Dom f (S2 i), Dom f Double, Dom f (S2 i, Double)
  , CFoldable f, CZip f, Ord i, Newtype i Int) =>
  f (S2 i) -> f Double -> Assoc i i Double
simplexAssocAsym neigh vals =
  assocFromList .
  concatMap (\(S2 i j, a) -> [((i,j),a), ((j,i),-a)]) .
  _toList $ _zip neigh vals
{-# INLINE simplexAssocAsym #-}

simplexAssocAsymDim ::
  ( Dom f (S2 i), Dom f Double, Dom f (S2 i, Double)
  , CFoldable f, CZip f) =>
  Int -> Int -> f (S2 i) -> f Double -> Assoc i i Double
simplexAssocAsymDim n m neigh vals =
  Assoc n m .
  concatMap (\(S2 i j, a) -> [((i,j),a), ((j,i),-a)]) .
  _toList $ _zip neigh vals
{-# INLINE simplexAssocAsymDim #-}
