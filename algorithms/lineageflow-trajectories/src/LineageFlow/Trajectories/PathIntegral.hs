module LineageFlow.Trajectories.PathIntegral
  ( pathPairIntegral
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.BArrayU as BArrayU

import LineageFlow.Trajectories.Types
import LineageFlow.Trajectories.Lineage
import LineageFlow.Trajectories.Expectation

import Control.Lens (view)

pathPairIntegral ::
  (Num a, Storable a) =>
  (a -> Double) ->
  Children ->
  CT ->
  CTMap a ->
  Array (S2 Cell) (S2 Cell) ->
  Array (S2 Cell) (Maybe Double)
pathPairIntegral f children ct measure pairs =
  let
    trajs = mkIx . BArrayU.fromList . unIx $ makeTrajectories ct measure
    cells = fromList (fmap Cell [0 .. _length trajs - 1])
    lineages = fmap (childrenToLineage children) cells
  in
    _fmap (fmap unweight . measureAverageIntegral (sums f) trajs lineages) pairs

unweight :: Weighted (Sum Double) (Sum Double) -> Double
unweight (Weighted (Sum p) (Sum a)) = a / p

sums ::
  Storable a =>
  (a -> Double) ->
  Trajectory (S2 Cell) Time a ->
  Maybe (Weighted (Sum Double) (Sum Double))
sums f =
  toMaybe .
  _foldMap (\x -> let y = f x in
    if isNaN y
      then Weighted (Sum 0) (Sum 0)
      else Weighted (Sum 1) (Sum y)
    ) .
  view trajectory_data
  where
    toMaybe w = if w == Weighted (Sum 0) (Sum 0) then Nothing else Just w
