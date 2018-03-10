module LineageFlow.Clustering.Mixing
  ( mix
  ) where

import LineageFlow.Prelude

mix :: [Double] -> [Array i Scalar] -> Array i Scalar
mix w l =
  let
    w' = fmap (/ (sum w)) w
    lw = zipWith (\s v -> _fmap (*s) v) w' l
  in
    foldl' (_zipWith (+)) (head lw) (tail lw)
