{-# LANGUAGE LambdaCase #-}

module LineageFlow.Deviations.Algorithm
  ( sisters
  , bornCells
  , allCellsBy
  , result
  , relativeTrajectories
  , TCCMap
  ) where

import LineageFlow.Prelude

import LineageFlow.Deviations.Utils
import LineageFlow.Deviations.Trajectory

--------------------------------------------------------------------------------

sisters :: Children -> [(Cell, Cell)]
sisters = _foldr (\case Nothing -> id; Just a -> (a:)) []
{-# INLINE sisters #-}

-- List of cells that originate from a division
bornCells :: Children -> [Cell]
bornCells = _foldr addSisters []
  where
    addSisters Nothing = id
    addSisters (Just (i,j)) = (\x -> i:j:x)
{-# INLINE bornCells #-}

-- List of all cells
allCellsBy :: Int -> Children -> [Cell]
allCellsBy k children =
  let n = _length children
  in _fmap Cell [0, k .. n - 1]
{-# INLINE allCellsBy #-}

differences ::
  DSumTA Cell Time ->
  DSumMapA Time Cell Vector ->
  [(Cell, Cell)] -> -- cell
  DSumMapL (Cell,Cell) Time Vector
differences ct pos pairs = Compose $ mkIx $ neighTrajs
  where
    neighTrajs = flip fmap pairs $ \(c,c') ->
      adjustTrajectory $
        intersectWith ct (dSumMapTranspose ct pos) (-) c c'
{-# INLINE differences #-}

type TCCMap a =  DSumMapL (Dep (Cell,Cell) Time) (Cell,Cell) a

result ::
  DSumTA Cell Time ->
  DSumMapA Time Cell Vector ->
  [(Cell, Cell)] -> -- cell
  ( TCCMap Vector -- displacements
  , TCCMap Vector -- increments
  , TCCMap Scalar -- msd
  , TCCMap Scalar -- msd0
  , TCCMap Scalar -- autocorrelation
  , TCCMap Scalar -- autocorrelation0
  )
result ct pos pairs =
  let
    diffs = differences ct pos pairs
    incr = ocomp increments diffs

    diffMsd = ocomp msd diffs
    diffMsd0 = ocomp msd0 diffs
    --diffMsdMean = ocomp (msd . applyThroughVector zeroSpeed) diffs

    incrAuto = ocomp autocorrelation incr
    incrAuto0 = ocomp autocorrelation0 incr
    --incrAutoMean = ocomp (autocorrelation . applyThroughVector zeroMean) incr
  in
    ( transposeST diffs
    , transposeST incr
    , transposeST diffMsd
    , transposeST diffMsd0
    , transposeST incrAuto
    , transposeST incrAuto0
    )
{-# INLINE result #-}

ocomp f = Compose . _fmap f . getCompose

relativeTrajectories ::
  DSumTA Cell Time ->
  DSumMapA Time Cell Vector ->
  [(Cell, Cell)] -> -- cell
  DSumMapL (Cell, Cell) Time Vector
relativeTrajectories ct pos pairs =
  differences ct pos pairs

