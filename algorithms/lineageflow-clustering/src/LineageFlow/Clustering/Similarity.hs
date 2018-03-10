module LineageFlow.Clustering.Similarity
  ( toSimilarity
  ) where

import LineageFlow.Prelude
import LineageFlow.Statistics

toSimilarity :: Array c Double -> Array c Double
toSimilarity d =
  let
    std = snd $ meanStdWith id d
  in
    _fmap (simFunction .  (/ std)) d

simFunction :: Double -> Double
simFunction x = if isNaN x then 0.00 else exp (-x ^ (2 :: Int) / 2)
