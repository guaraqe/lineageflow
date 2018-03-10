module LineageFlow.Triangulations.Expand
  ( expand
  ) where

import LineageFlow.Prelude

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)

expand ::
  (Storable a, Ord a) =>
  Int -> Array (S2 a) (S2 a) -> Array (S2 a) (S2 a)
expand 0 c = c
expand n c = expandWith c (expand (n - 1) c)

expandWith ::
  (Storable a, Ord a) =>
  Array (S2 a) (S2 a) -> Array (S2 a) (S2 a) -> Array (S2 a) (S2 a)
expandWith step base =
  let
    stepMap = contactMap step
    baseList = toList base
    notSame (S2 i j) = i /= j

    findSet (S2 i j) =
      Set.filter notSame $
      Set.map (s2 i) (fromMaybe Set.empty (Map.lookup j stepMap)) <>
      Set.map (s2 j) (fromMaybe Set.empty (Map.lookup i stepMap)) <>
      Set.singleton (S2 i j)
  in
    fromList . Set.toList . Set.unions $ fmap findSet baseList

contactMap ::
  (Storable a, Ord a) =>
  Array (S2 a) (S2 a) -> Map a (Set a)
contactMap =
  let
    insert (S2 i j) =
      Map.insertWith (<>) i (Set.singleton j) .
      Map.insertWith (<>) j (Set.singleton i)
  in
    _foldl' (flip insert) Map.empty
