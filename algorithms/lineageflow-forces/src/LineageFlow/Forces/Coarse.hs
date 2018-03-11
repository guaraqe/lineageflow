{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module LineageFlow.Forces.Coarse
  ( stressTensor
  ) where

import LineageFlow.Prelude
import LineageFlow.Homogenization.Space

import Data.Number.Erf

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------

gaussian3Line :: Double -> Vector -> Vector -> Double
gaussian3Line sigma a b =
  exp (- (a !.! a) / sigma ** 2 + (a !.! b) ** 2 / (sigma ** 2 * (b !.! b)) ) *
  ( erf (norm b / sigma + (a !.! b) / (sigma * norm b))
  - erf ((a !.! b) / (sigma * norm b)) ) /
  ( 4 * sqrt 2 * pi * sigma ** 2 * norm b )

coarseTerm :: Double -> Vector -> Vector -> Vector -> Double
coarseTerm sigma rk ri rj = gaussian3Line sigma (rk - ri) (ri - rj)

--------------------------------------------------------------------------------

-- Creates a map connecting each cell to their contacts
contactArray ::
  (t ~ Time, c ~ Dep Time Cell) =>
  DArray t (S2 c) (S2 c) ->
  Map c [c]
contactArray =
  _foldl' (\m (S2 i j) -> insert' i j . insert' j i $ m) Map.empty
  where
    insert' i j = Map.insertWith (<>) i [j]

composeContacts :: Ord a => Map a [a] -> Map a [(a,a)]
composeContacts m =
  Map.map (concatMap f) m
  where
    f i = fmap (\k -> (i,k)) (m Map.! i)

--------------------------------------------------------------------------------

matrixElementPart ::
  (Ord a, Newtype a Int) =>
  Double -> -- sigma
  Map (DS2 t a) (DS2 t a) ->
  DArray t (DS2 t a) Vector -> -- forces
  DArray t a Vector -> -- position
  (Dep t a) ->
  (Dep t a, Dep t a) ->
  Tensor
matrixElementPart sigma m forces pos k (i,j) =
  let
    ij = m Map.! (s2 i j)
    for_ij = forces ! Dep ij
    pos_i = pos ! i
    pos_j = pos ! j
    pos_k = pos ! k
    pos_ij = pos_i - pos_j
    term = coarseTerm sigma pos_k pos_i pos_j
  in
    tensorProductWith (\x y -> x * y * term) for_ij pos_ij

--------------------------------------------------------------------------------

matrixElementSum ::
  (a ~ Cell) =>
  Double -> -- sigma
  Map (DS2 t a) (DS2 t a) ->
  DArray t (DS2 t a) Vector -> -- forces
  DArray t a Vector -> -- position
  (Dep t a) ->
  [(Dep t a, Dep t a)] ->
  Tensor
matrixElementSum sigma m forces pos k l =
  foldl' (+) (pure (pure 0)) $ fmap (matrixElementPart sigma m forces pos k) l

inverseArray ::
  (Ord a, Storable a, Newtype a Int) =>
  DArray t (DS2 t a) (DS2 t a) -> Map (DS2 t a) (DS2 t a)
inverseArray = Map.fromList . _ifoldr (\(Dep i) k l -> (k,i):l) []

firstTerm ::
  (t ~ Time, c_t ~ Dep Time Cell, c ~ Cell) =>
  Double ->
  DArray t (S2 c_t) (S2 c_t) ->
  DArray t (S2 c_t) Vector ->
  DArray t c Vector ->
  DArray t c Tensor
firstTerm sigma neigh forces pos =
  let
    m = inverseArray neigh
    contacts = composeContacts (contactArray neigh)
    indexes = _fmap pack $ fromList [0 .. _length pos - 1]
    f i =
      let
        l = Map.lookup i contacts
      in
        maybe (pure (pure 0)) (matrixElementSum sigma m forces pos i) l
  in
    _fmap f indexes

--------------------------------------------------------------------------------

secondTerm ::
  Double ->
  DArray Time Cell Vector ->
  DArray Time Cell Vector ->
  DArray Time Cell Tensor
secondTerm sigma pos vel =
  let
    ten = _zipWith (tensorProductWith (*)) vel vel
  in
    applyThroughTensor (homogenize (spaceGaussianDense sigma pos)) ten

--------------------------------------------------------------------------------

matrixTerm ::
  Double -> -- sigma
  DArray Time (DS2 Time Cell) (DS2 Time Cell) -> -- contacts
  DArray Time (DS2 Time Cell) Vector -> -- forces
  DArray Time Cell Vector -> -- position
  DArray Time Cell Vector -> -- delta velocity
  DArray Time Cell Tensor
matrixTerm sigma neigh forces pos vel =
  let
    velh =
      applyThroughVector (homogenize (spaceGaussianNormalizedDense sigma pos)) vel
    delvel = _zipWith (-) vel velh
  in
    _zipWith (+)
      (firstTerm sigma neigh forces pos)
      (secondTerm sigma pos delvel)

stressTensor ::
  Double ->
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  DSumMapL Time (DS2 Time Cell) Vector ->
  DSumMapL Time Cell Vector ->
  DSumMapL Time Cell Vector ->
  DSumMapL Time Cell Tensor
stressTensor sigma neigh forces pos vel =
  Compose $
    _zipWith4 (matrixTerm sigma)
      (getCompose neigh)
      (getCompose forces)
      (getCompose pos)
      (getCompose vel)
