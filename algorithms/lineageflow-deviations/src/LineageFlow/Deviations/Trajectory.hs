{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Deviations.Trajectory
  ( intersectWith
  , adjustTrajectory
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.ArrayU as ArrayU
import Control.Lens (view, over, _1)

intersectWith ::
  (Dom ArrayU a, Dom ArrayU b, Dom ArrayU i, Newtype i Int) =>
  DSumMap BArray i Array Time (Time, Dep Time i) -> -- ct
  DSumMap BArray i Array Time a -> -- measure
  (a -> a -> b) ->
  i ->
  i ->
  Array (Dep (i,i) Time) b
intersectWith ct m f i j =
  let
    traj_i = (getCompose m) ! i
    traj_j = (getCompose m) ! j
    t_i = view (_1 . time) $ ct ! (i, Dep (Time 0))
    t_j = view (_1 . time) $ ct ! (j, Dep (Time 0))
  in
    if t_i >= t_j
      then mvIx $
        _zipWith f traj_i (over ixed (ArrayU.drop (t_i - t_j)) traj_j)
      else mvIx $
        _zipWith f (over ixed (ArrayU.drop (t_j - t_i)) traj_i) traj_j
{-# INLINE intersectWith #-}

adjustTrajectory ::
  (Storable a, Num a) =>
  Array i a -> Array i a
adjustTrajectory v =
  let v0 = unIx v ! 0
  in _fmap (\u -> u - v0) v
{-# INLINE adjustTrajectory #-}
