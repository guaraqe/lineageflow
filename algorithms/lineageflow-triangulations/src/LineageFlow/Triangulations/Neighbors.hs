module LineageFlow.Triangulations.Neighbors
  ( contacts
  , contactsDifferences
  ) where

import LineageFlow.Prelude
import qualified Data.Set as Set

contacts :: (Storable a, Ord a) => Array i (S4 a) -> Array j (S2 a)
contacts =
  fromList .
  Set.toAscList .
  Set.fromList .
  _foldr d2 []
  where
    d2 :: S4 a -> [S2 a] -> [S2 a]
    d2 (S4 i j k l) ls =
      (S2 i j) : (S2 i k) : (S2 i l) : (S2 j k) : (S2 j l) : (S2 k l) : ls

--------------------------------------------------------------------------------

contactsDifferences ::
  DArray Time Cell Vector ->
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time (DS2 Time Cell) Vector
contactsDifferences v = _fmap (\(S2 i j) -> v ! i - v ! j)
