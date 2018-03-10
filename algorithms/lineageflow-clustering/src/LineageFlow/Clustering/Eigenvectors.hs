module LineageFlow.Clustering.Eigenvectors
  ( eigenvectors
  ) where

import LineageFlow.Prelude
import Numeric.LinearAlgebra.HArpack

-- TODO: Usar `arpack` em vez de `harpack`.

eigenvectors ::
  Int -> -- dimension
  Int -> -- number of eigens
  Array (S2 Cell) (S2 Cell) ->
  Array (S2 Cell) Scalar ->
  [Array Cell Scalar]
eigenvectors dim n ix vals =
  let
    valsList = toList vals
    ixList = toList $ _fmap (fmap (view cell)) ix
    pairSym (S2 i j) s = [((i,j),s),((j,i),s)]
    sym = concat $ zipWith pairSym ixList valsList
    RReal m' = eigs (SparseMatrix dim sym) (Symmetric SLA) n
    vectors = map snd . sortBy (compare `on` (\x -> - (fst x))) $ m'
  in
    fmap mkIx vectors
