module LineageFlow.Triangulations.FullGraph
  ( fullGraph
  ) where

import LineageFlow.Prelude

fullGraph :: Mothers -> Array (S2 Cell) (S2 Cell)
fullGraph m =
  let
    rng = [0 .. _length m - 1]
  in
    fromList [S2 (Cell i) (Cell j) | i <- rng, j <- rng, i > j]
