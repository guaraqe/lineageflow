{-# LANGUAGE FlexibleContexts #-}

module PositionOnly where

import LineageFlow.Prelude hiding (option)
import LineageFlow.Tracking
import LineageFlow.Triangulations.Delaunay
import LineageFlow.Triangulations.Neighbors
import LineageFlow.Triangulations.GlobalNeighbors
import LineageFlow.Trajectories.PathIntegral
import LineageFlow.Clustering.Similarity
import LineageFlow.Clustering.Eigenvectors
import LineageFlow.Clustering.Clusters

import LineageFlow.Import
import LineageFlow.Export.Format.Selection

import qualified LineageFlow.BArrayU as BArrayU

import qualified Data.ByteString.Lazy as BL

import Options.Applicative

data Input = Input
  { input_path :: FilePath
  , input_output :: FilePath
  , input_number :: Int
  }

input :: Parser Input
input = Input <$>
  strOption (long "path" <> help "Path of the CSV tracking file.") <*>
  strOption (long "output" <> help "Path of the output files.") <*>
  option auto (long "number" <> help "Maximum number of clusters to be calculated.")

main :: IO ()
main =
  customExecParser (prefs showHelpOnError) opts >>= run
  where
    opts =
      info
       ( input <**> helper )
       ( fullDesc <>
         progDesc "Complete pipeline for the clustering of cell tractories using cells' positions." <>
         header "bioemergences-clustering" )

--------------------------------------------------------------------------------

run :: Input -> IO ()
run (Input path output number) = do
  file <- BL.readFile path
  case parseFormat formatCSV file of
    Left e -> error e
    Right (Initial tracking position abstime _) -> do
      let clusters = pipeline tracking position number
      forM_ (zip [2..number] clusters) $ \(n,c) -> do
        let bs = encodedSelection abstime tracking position c
        BL.writeFile (output <> show n <> ".csv") bs

pipeline :: TCMap Tracking -> TCMap Vector -> Int -> [TCMap Int]
pipeline tracking position clusterNumber = liftedClusters
  where

    LineageSet mothers children tc ct =
      lineageSetFromTracking tracking

    makeDelaunay x =
      Compose $ _fmap contacts $ mkIx $ fst $ unzip $ _fmap delaunay $ unIx $ getCompose x

    delaunayPosition =
      makeDelaunay position

    delaunayPositionGlobal =
      globalNeighborsGenealogy mothers tc delaunayPosition

    global = delaunayPositionGlobal

    similarity x =
      toSimilarity $ _fmap (fromMaybe (0/0)) $ pathPairIntegral norm children ct (t2c ct x) global

    similarityMixed =
      similarity position

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
