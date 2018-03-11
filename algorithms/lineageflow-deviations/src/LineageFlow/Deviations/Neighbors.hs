{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Deviations.Neighbors
  ( neighbors
  , neighborsList
  ) where

import LineageFlow.Prelude hiding (intersect)
import qualified LineageFlow.ArrayU as ArrayU

import LineageFlow.Deviations.Utils
import LineageFlow.Deviations.Trajectory

import Control.Lens (over, view)

neighbors ::
  DSumTA Time Cell ->
  DSumTA Cell Time ->
  DSumMapA Time Cell Vector -> -- positions
  Double -> -- Limit distance
  Cell -> -- cell
  [Cell]
neighbors tc ct (Compose pos) d n = cells
  where
    (start,cellIndex) = ct ! (n, Dep (Time 0))
    posFrame = pos ! start
    posCell = posFrame ! cellIndex
    indexes = fmap (Dep . Cell) . ArrayU.toList $
      ArrayU.findIndices (\x -> norm (x - posCell) <= d) (unIx posFrame)
    cells = fmap fst . filter (\(k,_) -> k /= n) $ map (\i -> tc ! (start,i)) indexes
{-# INLINE neighbors #-}

neighborsList ::
  DSumTA Time Cell ->
  DSumTA Cell Time ->
  DSumMapA Time Cell Vector -> -- positions
  Double -> -- Limit distance
  [Cell] ->
  [(Cell, Cell)]
neighborsList tc ct pos d =
  concatMap (\c -> fmap (\c' -> (c,c')) (neighbors tc ct pos d c))
{-# INLINE neighborsList #-}
