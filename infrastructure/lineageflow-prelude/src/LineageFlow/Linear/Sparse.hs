{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module LineageFlow.Linear.Sparse
  ( Sparse (..)
  , sparse_rows
  , sparse_cols
  , sparse_vals
  , sparseEmpty
  , sparseFromList
  , sparseFromListDim
  , addToSparse
  , toDense
  , squareSparse
  , squareSparseAssoc
  , sparseTimesVector
  , sparseTimesMatrix
  )
  where

import BasePrelude hiding (maximum, fold, any)
import Control.Newtype (Newtype (..))
import Data.Indexed
import Control.ConstraintClasses

import LineageFlow.Prelude.Containers
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

import LATS hiding (Vector, rows, cols)
import qualified LATS.Vector.Mutable as MLinear
import qualified LineageFlow.Linear.Raw as Linear

import Control.Lens hiding (Fold)

import Control.Foldl hiding (head)

--------------------------------------------------------------------------------

data Sparse i j a = Sparse
  { _sparse_rows :: !Int
  , _sparse_cols :: !Int
  , _sparse_vals :: !(IntMap (IntMap a)) }
  deriving (Show, Functor, Foldable, Traversable)

$(makeLenses ''Sparse)

sparseEmpty :: Int -> Int -> Sparse i j a
sparseEmpty i j = Sparse i j (IntMap.empty)

--------------------------------------------------------------------------------

addToSparse ::
  (Newtype i Int, Newtype j Int) =>
  i -> j -> a -> Sparse i j a -> Sparse i j a
addToSparse i j a = over sparse_vals $
  IntMap.insertWith addSingleton (unpack i) (IntMap.singleton (unpack j) a)
{-# INLINE addToSparse #-}

addToSparseVals ::
  (Newtype i Int, Newtype j Int) =>
  i -> j -> a -> IntMap (IntMap a) -> IntMap (IntMap a)
addToSparseVals i j a =
  IntMap.insertWith addSingleton (unpack i) (IntMap.singleton (unpack j) a)
{-# INLINE addToSparseVals #-}

addSingleton :: IntMap a -> IntMap a -> IntMap a
addSingleton = \s ->
  let (k,v) = head (IntMap.toList s)
  in IntMap.insert k v
{-# INLINE addSingleton #-}

--------------------------------------------------------------------------------

foldRow ::
  (Ord i, Newtype i Int) => Fold ((i,j),a) Int
foldRow = dimap (fst . fst) (maybe 0 ((+1) . unpack)) maximum

foldCol ::
  (Ord j, Newtype j Int) => Fold ((i,j),a) Int
foldCol = dimap (snd . fst) (maybe 0 ((+1) . unpack)) maximum

foldVal ::
  (Newtype i Int, Newtype j Int) =>
  Fold ((i,j),a) (IntMap (IntMap a))
foldVal = Fold (\m ((i,j),a) -> addToSparseVals i j a m) (IntMap.empty) id

sparseFromList ::
  (Newtype i Int, Newtype j Int, Ord i, Ord j) =>
  [((i,j),a)] -> Sparse i j a
sparseFromList = fold (Sparse <$> foldRow <*> foldCol <*> foldVal)

sparseFromListDim ::
  (Newtype i Int, Newtype j Int) =>
  Int -> Int -> [((i,j),a)] -> Sparse i j a
sparseFromListDim i j = fold (Sparse i j <$> foldVal)

--------------------------------------------------------------------------------

addBounds :: Num a => Int -> Int -> [((Int,Int),a)] -> [((Int,Int),a)]
addBounds i j = \vals ->
  vals ++
    if fold ((&&) <$> rows <*> cols) vals
      then []
      else [((i-1,j-1),0)]
  where
    rows = lmap (fst . fst) (any (== i - 1))
    cols = lmap (snd . fst) (any (== j - 1))

toAssoc ::
  Num a =>
  Sparse i j a -> [((Int,Int),a)]
toAssoc (Sparse i j a) =
  let
    vals =
      concatMap (\(r,l) -> fmap (\(c,x) -> ((r,c),x)) l) .
      IntMap.toList .
      fmap IntMap.toList $
      a
  in
    addBounds i j vals

toDense ::
  Sparse i j Double-> Matrix i j Double
toDense = mkIx .
  Linear.toDense .
  toAssoc
{-# INLINE toDense #-}

squareSparse ::
  Sparse i j Double -> Matrix i i Double
squareSparse = \(Sparse i _ m) ->
  let
    m' = IntMap.toList m
    f (ni,xn) (mi,xm) =
      ((ni,mi), foldl' (+) 0 $ IntMap.intersectionWith (*) xn xm)
  in
    mkIx . Linear.toDense . addBounds i i $ f <$> m' <*> m'
{-# INLINE squareSparse #-}

squareSparseAssoc ::
  Sparse i j Double -> [((Int,Int),Double)]
squareSparseAssoc = \(Sparse i _ m) ->
  let
    m' = IntMap.toList m
    f (ni,xn) (mi,xm) =
      ((ni,mi), foldl' (+) 0 $ IntMap.intersectionWith (*) xn xm)
  in
    addBounds i i $ f <$> m' <*> m'
{-# INLINE squareSparseAssoc #-}

sparseTimesVector ::
  (Storable a, Num a) =>
  Sparse i j a -> Array j a -> Array i a
sparseTimesVector (Sparse i j s) v =
  if j /= _length v
    then error "sparseTimesVector: bad dimensions"
    else
      let
        sl = IntMap.toList s
        result :: (Storable a, Num a) => IntMap a -> ArrayU a -> a
        result mv u =
          let lv = IntMap.toList mv
          in foldl' (\z (k,x) -> z + x * (u ! k)) 0 lv
      in
        runST $ do
          u <- MLinear.replicate i 0
          forM_ sl $ \(k,mv) -> MLinear.write u k (result mv (unIx v))
          mkIx <$> Linear.freeze u

sparseTimesMatrix ::
  (Num a, Linear.Element a) =>
  Sparse i j a -> Matrix j k a -> Matrix i k a
sparseTimesMatrix s =
  fromColumns .
  fmap (sparseTimesVector s) .
  toColumns


