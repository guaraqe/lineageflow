module LineageFlow.Clustering.Clusters
  ( spectralClustering
  ) where

import LineageFlow.Prelude

--import Math.KMeans
import qualified LineageFlow.ArrayU as ArrayU
import qualified LineageFlow.BArrayU as BArrayU
import qualified LineageFlow.Linear.Raw as L


import qualified AI.Clustering.KMeans as C
import qualified Data.Matrix.Dense.Generic as C

spectralClustering ::
  Int ->
  [Array Cell Scalar] ->  -- eigenvector matrix
  Array Cell Int
spectralClustering k = fromList . myKMeans2 k . normalizeRows . take k

normalizeRows :: [Array Cell Scalar] -> [ArrayU Double]
normalizeRows = fmap normalize . L.toRows . L.fromColumns . fmap unIx
  where
    normalize v = _fmap (\x -> x / norm v) v
    norm = sqrt . _sum . _fmap (^ (2 :: Int))
    _sum = _foldl' (+) 0

{-

myKMeans :: Int -> [ArrayU Double] -> [Int]
myKMeans n v = flip clusterToInts vc $ kmeans id euclidSq n vc
  where vc = map ArrayU.convert v

clusterToInts :: Eq a => Clusters a -> [a] -> [Int]
clusterToInts c = map (findInCluster c)

findInCluster :: Eq a => Clusters a -> a -> Int
findInCluster cs x = fromJust $
                     BArrayU.findIndex (\(Cluster l) -> x `elem` l) cs

-}

--------------------------------------------------------------------------------

myKMeans2 :: Int -> [ArrayU Double] -> [Int]
myKMeans2 n l =
  let
    opts = C.defaultKMeansOpts { C.kmeansClusters = False, C.kmeansSeed = fromList [0,1,2,3,4,5,6,7] }
    matrix = C.fromRows (fmap ArrayU.convert l)
  in
    toList $ C.membership $ C.kmeans n matrix opts
