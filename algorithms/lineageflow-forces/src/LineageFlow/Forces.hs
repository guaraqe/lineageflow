module LineageFlow.Forces
  ( forcesLeastSquares
  , decomposeForces
  , decomposeForcesTC
  ) where

import LineageFlow.Prelude
import LineageFlow.Forces.Eigen

---------------------------------------------------------------------------------

nan :: Double
nan = 0/0

forcesLeastSquares ::
  DArray Time (DS2 Time Cell) (DS2 Time Cell) -> -- contacts
  DArray Time (DS2 Time Cell) Scalar -> -- connexities
  DArray Time Cell Vector -> -- acceleration
  DArray Time (DS2 Time Cell) Vector -- forces
forcesLeastSquares neigh conn accel =
  if _length neigh == 0 || getAny (_foldMap (foldMap (Any . isNaN)) accel)
    then _fmap (const (V3 nan nan nan)) neigh
    else partialForces
  where
    partialForces = _zipWith (\l v -> fmap (l*) v) conn $
      pseudoSolve neigh conn accel
{-# INLINE forcesLeastSquares #-}

---------------------------------------------------------------------------------

decomposeForce :: Vector -> Vector -> (Scalar, Scalar)
decomposeForce f v = second norm (project f v)

decomposeForces::
  DArray Time (DS2 Time Cell) Vector ->
  DArray Time (DS2 Time Cell) Vector ->
  ( DArray Time (DS2 Time Cell) Scalar
  , DArray Time (DS2 Time Cell) Scalar )
decomposeForces f p = _unzip (_zipWith decomposeForce f p)

decomposeForcesTC ::
  DSumMapL Time (DS2 Time Cell) Vector ->
  DSumMapL Time (DS2 Time Cell) Vector ->
  ( DSumMapL Time (DS2 Time Cell) Scalar
  , DSumMapL Time (DS2 Time Cell) Scalar )
decomposeForcesTC (Compose f) (Compose p) =
  bimap Compose Compose $ _unzip (_zipWith decomposeForces f p)
