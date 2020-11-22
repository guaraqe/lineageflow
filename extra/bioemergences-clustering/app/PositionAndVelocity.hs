{-# LANGUAGE FlexibleContexts #-}

module PositionAndVelocity where

import LineageFlow.Prelude hiding (option)
import LineageFlow.Tracking
import LineageFlow.Derivatives.Lanczos
import LineageFlow.Homogenization.Space
import LineageFlow.Triangulations.Delaunay
import LineageFlow.Triangulations.Neighbors
import LineageFlow.Triangulations.GlobalNeighbors
import LineageFlow.Trajectories.PathIntegral
import LineageFlow.Clustering.Similarity
import LineageFlow.Clustering.Mixing
import LineageFlow.Clustering.Eigenvectors
import LineageFlow.Clustering.Clusters

import LineageFlow.Import
import LineageFlow.Export.Format.Selection

import qualified LineageFlow.BArrayU as BArrayU

import qualified Data.Set as Set
import qualified Data.ByteString.Lazy as BL

import Options.Applicative

data Input = Input
  { input_path :: FilePath
  , input_output :: FilePath
  , input_number :: Int
  , input_timeSize :: Int
  , input_spaceSize :: Double
  , input_weightPosition :: Double
  , input_weightVelocity :: Double
  }

input :: Parser Input
input = Input <$>
  strOption (long "path" <> help "Path of the CSV tracking file.") <*>
  strOption (long "output" <> help "Path of the output files.") <*>
  option auto (long "number" <> help "Maximum number of clusters to be calculated.") <*>
  option auto (long "time-size" <> help "Size of the temporal filter to be used for derivatives.") <*>
  option auto (long "space-size" <> help "Size of the spatial filter to be used for homogenizations.") <*>
  option auto (long "position-weight" <> help "Weight given to position.") <*>
  option auto (long "velocity-weight" <> help "Weight given to velocity.")

--------------------------------------------------------------------------------

run :: Input -> IO ()
run (Input path output number timeSize spaceSize weightPosition weightVelocity) = do
  file <- BL.readFile path
  case parseFormat formatCSV file of
    Left e -> error e
    Right (Initial tracking position abstime _) -> do
      let clusters = pipeline tracking position timeSize spaceSize weightPosition weightVelocity number
      forM_ (zip [2..number] clusters) $ \(n,c) -> do
        let bs = encodedSelection abstime tracking position c
        BL.writeFile (output <> show n <> ".csv") bs

pipeline :: TCMap Tracking -> TCMap Vector -> Int -> Double -> Double -> Double -> Int -> [TCMap Int]
pipeline tracking position timeSize spaceSize weightPosition weightVelocity clusterNumber = liftedClusters
  where

    LineageSet mothers children tc ct =
      lineageSetFromTracking tracking

    velocity =
      tcDeriveV tc ct (lanczos 1 timeSize 0 2) position

    velocityH =
      spaceHomogenizeVector (spaceGaussianNormalizedDense spaceSize) position velocity

    makeDelaunay x =
      Compose $ _fmap contacts $ mkIx $ fst $ unzip $ _fmap delaunay $ unIx $ getCompose x

    delaunayPosition =
      makeDelaunay position

    delaunayVelocity =
      makeDelaunay velocityH

    delaunayPositionGlobal =
      globalNeighborsGenealogy mothers tc delaunayPosition

    delaunayVelocityGlobal =
      globalNeighborsGenealogy mothers tc delaunayVelocity

    toSet = Set.fromList . toList
    fromSet = fromList . Set.toList

    global =
      fromSet $ Set.union (toSet delaunayPositionGlobal) (toSet delaunayVelocityGlobal)

    similarity x =
      toSimilarity $ _fmap (fromMaybe (0/0)) $ pathPairIntegral norm children ct (t2c ct x) global

    similarityPosition =
      similarity position

    similarityVelocity =
      similarity velocity

    similarityMixed = mix [weightPosition, weightVelocity] [similarityPosition, similarityVelocity]

    clusters =
      let
        dim = _length mothers
        eig = eigenvectors dim clusterNumber global similarityMixed
      in
        fmap (\k -> spectralClustering k eig) [2 .. clusterNumber]

    liftedClusters =
      let
        liftClusters =
          dSumMapTranspose tc . Compose . mkIx . _zipWith (\v x -> _fmap (const x) v) (BArrayU.fromList $ unIx $ getCompose ct)
      in
        fmap (liftClusters . BArrayU.convert . unIx) clusters
