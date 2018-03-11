module LineageFlow.Import.Utils
  ( splitRec
  , rescale
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.BArrayU as BArrayU

--------------------------------------------------------------------------------

splitRec :: (BArrayU a -> a -> Bool) -> BArrayU a -> [BArrayU a]
splitRec f v =
  let
    (v1,v2) = splitBy (f v) v
  in
    case BArrayU.null v2 of
      True -> [v1]
      False -> v1 : splitRec f v2
{-# INLINE splitRec #-}

splitBy :: (a -> Bool) -> BArrayU a -> (BArrayU a, BArrayU a)
splitBy f v =
  let
    n = BArrayU.length (BArrayU.takeWhile f v)
  in
    BArrayU.splitAt n v
{-# INLINE splitBy #-}

rescale :: Vector -> DSumMapL Time Cell Vector -> DSumMapL Time Cell Vector
rescale scale = _fmap (*scale)
