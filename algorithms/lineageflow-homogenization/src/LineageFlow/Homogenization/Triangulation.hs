{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module LineageFlow.Homogenization.Triangulation
  ( homogenizeTriangulation
  , homogenizeTri
  , triCell
  , triDepCell
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.ArrayU as ArrayU

import Data.Maybe (isJust, fromJust)
import Data.Map.Strict (Map)

import qualified Data.Map.Strict as Map

import Data.Monoid (Sum)
import Data.List (unzip)

import Control.Lens (_2)

toMap :: (Ord a, Storable a) => Array i a -> Map a (Sum Int)
toMap = Map.fromList . fmap (\x -> (x,Sum 1)) . toList

fromMap ::
  Storable a =>
  Int -> Map a (Sum Int) -> Array i (a, Double)
fromMap n = \m ->
    fromList $
    fmap (over _2 (\(Sum k) -> fromIntegral k / fromIntegral n)) $
    Map.toAscList m

combineMap :: Ord a => [Map a (Sum Int)] -> Map a (Sum Int)
combineMap = Map.unionsWith (<>)

windows' :: Int -> Int -> [a] -> [[a]]
windows' _ _ [] = []
windows' n m (l:ls) =
  if n > m
    then []
    else take n (l:ls) : windows' n (m - 1) ls

windows :: Int -> Int -> [a] -> [[a]]
windows n m l =
  replicate (div (n - 1) 2) [] <>
  windows' n m l <>
  replicate (div (n - 1) 2) []

homogenizeTriangulation ::
  (Ord a, Storable a) =>
  Int ->
  (List i .:. Array j) a ->
  (List i .:. Array j) (a, Double)
homogenizeTriangulation n (Compose l) =
  let
    len = length l
    n' = 2 * n + 1
  in
    Compose . mkIx $
    fmap (fromMap n' . combineMap) $
    windows n' len $
    fmap toMap (unIx l)

triCell ::
  DSumTA Time Cell -> -- tc
  Time ->
  Array t (S2 (Dep Time Cell)) ->
  Array t (S2 Cell)
triCell tc t = _fmap toCellS2
  where
    toCell i = fst (tc ! (t,i))
    toCellS2 = fst . s2normalize . fmap toCell

triDepCell ::
  DSumTA Cell Time -> -- ct
  Time ->
  Array t (S2 Cell, Double) ->
  Array t (S2 (Dep Time Cell), Double)
triDepCell ct (Time t) = filterJust . _fmap toDepCellS2
  where
    toDepCell i = fmap snd (c !? (t - t0))
      where
        c = unIx (getCompose ct ! i)
        Time t0 = fst (c ! 0)
    toDepCellS2 (S2 i j, d) =
      set _2 d . s2normalize <$> (S2 <$> toDepCell i <*> toDepCell j)

filterJust :: Storable a => Array t (Maybe a) -> Array t a
filterJust = _fmap fromJust . mkIx . ArrayU.filter isJust . unIx

homogenizeTri ::
  Int ->
  DSumTA Time Cell -> -- tc
  DSumTA Cell Time -> -- ct
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  ( DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
  , DSumMapL Time (DS2 Time Cell) Double
  )
homogenizeTri n tc ct v = vh
  where
    vc =
      Compose . mkIx $
      zipWith (triCell tc) (fmap Time [0..]) $
      unIx . getCompose $
      v
    vch = homogenizeTriangulation n vc
    vh =
      bimap (Compose . mkIx) (Compose. mkIx) $
      unzip $
      fmap _unzip $
      zipWith (triDepCell ct) (fmap Time [0..]) $
      unIx . getCompose $
      vch
