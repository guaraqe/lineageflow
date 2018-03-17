{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Triangulations.Delaunay
 ( delaunay
 ) where

import LineageFlow.Prelude

import qualified Data.List as List
import qualified Numeric.Qhull as Qhull

import Control.DeepSeq (force)

delaunay ::
  (Newtype i Int, Ord i, Storable i) =>
  DArray t i Vector ->
  ( DArray t (DS4 t i) (DS4 t i)
  , DArray t (DS4 t i) Sign )
delaunay points =
  force $
  bimap (mkIx . fromList) (mkIx . fromList) $
  List.unzip $
  fmap (\v -> s4normalize $
         S4 (pack $ v ! 0) (pack $ v ! 1) (pack $ v ! 2) (pack $ v ! 3)) $
  simplices
  where
    points' = _concatMap (\(V3 x y z) -> fromList [x,y,z]) (unIx points)
    simplices = case Qhull.delaunay 3 points' of
      Left (Qhull.Error n msg) ->
        error ("Qhull failed. Code: " <> show n <> ", " <> msg)
      Right x -> x
