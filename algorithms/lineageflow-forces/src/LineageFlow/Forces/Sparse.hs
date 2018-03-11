{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Forces.Sparse
  ( contactsToSparse
  , sparseAssocTimesVector
  , matrixSquare
  ) where

import LineageFlow.Prelude

import qualified LATS.Vector.Mutable as MLinear
import qualified LineageFlow.Linear.Raw as Linear

--------------------------------------------------------------------------------

addSimplex ::
  Num b =>
  (Dep t (S2 a), S2 a, b) -> [((Dep t (S2 a),a),b)]
addSimplex = \(x,S2 i j,c) ->
  [ ((x,i),c)
  , ((x,j),-c)
  ]
{-# INLINE addSimplex #-}

contactsToSparse ::
  (Storable a, Storable b, Newtype a Int, Num b) =>
  DArray t (S2 a) (S2 a) ->
  DArray t (S2 a) b ->
  [((Dep t (S2 a),a), b)]
contactsToSparse c v =
  let
    cv =  _izipWith (,,) c v
  in
    _foldr (\x l -> addSimplex x <> l) [] cv
{-# INLINE contactsToSparse #-}

--------------------------------------------------------------------------------

sparseAssocTimesVector ::
  ( Dom f ((i,j),Double)
  , CFoldable f, Newtype i Int, Newtype j Int) =>
  Int -> f ((i,j), Double) -> Array j Double -> Array i Double
sparseAssocTimesVector n vals v =
  mkIx $ Linear.create $ do
    u <- MLinear.replicate n 0
    _forM_ vals $ \((i,j),va) -> do
      let i' = unpack i; j' = unpack j
      MLinear.modify u (\k -> k + va * unIx v ! j') i'
    return u
{-# INLINE sparseAssocTimesVector #-}

diagonalSquare ::
  Int ->
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time (DS2 Time Cell) Scalar ->
  [(Int,Int,Double)]
diagonalSquare n ind val =
  let vals = _zip ind val in
  _imap (\k v -> (k,k,v)) . toList $
  Linear.create $ do
    u <- MLinear.replicate n 0
    _forM_ vals $ \(S2 i j,va) -> do
      let i' = unpack i; j' = unpack j
      MLinear.modify u (\k -> k + va**2) i'
      MLinear.modify u (\k -> k + va**2) j'
    return u
{-# INLINE diagonalSquare #-}

nonDiagonalSquare ::
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time (DS2 Time Cell) Scalar ->
  [(Int,Int,Double)]
nonDiagonalSquare ind val =
  let
    vals = toList (_zip ind val)
  in
    concatMap (\(S2 i j,v) ->
      let i' = unpack i; j' = unpack j in
      [(i',j',-v**2),(j',i',-v**2)]) vals
{-# INLINE nonDiagonalSquare #-}

matrixSquare ::
  Int ->
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time (DS2 Time Cell) Scalar ->
  [(Int,Int,Double)]
matrixSquare n ind val =
  nonDiagonalSquare ind val ++ diagonalSquare n ind val
{-# INLINE matrixSquare #-}
