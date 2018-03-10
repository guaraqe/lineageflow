module LineageFlow.Triangulations.Radius
  ( neighborhoodRadius
  , neighborhoodProportionalRadius
  ) where

import LineageFlow.Prelude
import LineageFlow.Statistics
import qualified LineageFlow.ArrayU as ArrayU

neighborhoodRadius ::
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  DSumMapL Time Cell Vector ->
  DSumMapL Time Cell Scalar
neighborhoodRadius (Compose c) (Compose p) =
  Compose (_zipWith localRadius c p)

neighborhoodProportionalRadius ::
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  DSumMapL Time Cell Vector ->
  DSumMapL Time Cell Scalar
neighborhoodProportionalRadius (Compose c) (Compose p) =
  Compose (fmap proportional $ _zipWith localRadius c p)

localRadius ::
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time Cell Vector ->
  DArray Time Cell Scalar
localRadius c p =
  let
    initial = _fmap (const 30) (unIx p)
    updateList (S2 i j) =
      let
        d = norm (p ! i - p ! j)
      in
        [(unpack i,d),(unpack j,d)]
  in
    mkIx $
    ArrayU.accum min initial $
    concatMap updateList $
    toList c

proportional :: Array i Scalar -> Array i Scalar
proportional v =
  let
    m = meanWith id v
  in
    _fmap (/m) v
