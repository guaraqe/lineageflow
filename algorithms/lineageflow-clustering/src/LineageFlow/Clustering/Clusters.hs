module LineageFlow.Clustering.Clusters
  ( spectralClustering
  ) where

import LineageFlow.Prelude

import qualified LineageFlow.ArrayU as ArrayU
import qualified LineageFlow.Linear.Raw as L

import qualified AI.Clustering.KMeans as C
import qualified Data.Matrix.Generic as C

--------------------------------------------------------------------------------

spectralClustering ::
  Int ->
  [Array Cell Scalar] ->  -- eigenvector matrix
  Array Cell Int
spectralClustering k = fromList . myKMeans k . normalizeRows . take k

normalizeRows :: [Array Cell Scalar] -> [ArrayU Double]
normalizeRows = fmap normalizeRow . L.toRows . L.fromColumns . fmap unIx
  where
    normalizeRow v = _fmap (\x -> x / normRow v) v
    normRow = sqrt . _sum . _fmap (^ (2 :: Int))
    _sum = _foldl' (+) 0

myKMeans :: Int -> [ArrayU Double] -> [Int]
myKMeans n l =
  let
    opts = C.defaultKMeansOpts { C.kmeansClusters = False }
    matrix = C.fromRows (fmap ArrayU.convert l)
  in
    toList $ C.membership $ C.kmeans n matrix opts
