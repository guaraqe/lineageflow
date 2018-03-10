module LineageFlow.Statistics.Regression
  ( logRegressZero
  , regressLinear
  , regressQuadratic
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.ArrayU as ArrayU

import qualified LineageFlow.Linear.Raw as Raw

import Statistics.Regression

--------------------------------------------------------------------------------

-- Results are coefficient, intersection and validation coefficient.
logRegressZero :: ArrayU Scalar -> (Double, Double, Double)
logRegressZero v =
  let
    y =
      ArrayU.convert $
      _fmap log (ArrayU.drop 1 v)
    x =
      ArrayU.convert $
      _fmap (log . fromIntegral) (ArrayU.fromList [1 .. _length v - 1])
    (as,r) = olsRegress [x] y
    as' = ArrayU.convert as :: ArrayU Scalar
    a = as' ! 0
    b = as' ! 1
  in (a,b,r)
{-# INLINE logRegressZero #-}


regressLinear :: Array i Double -> (Double, Double)
regressLinear v =
  let
    n = _length v
    t = fromList [0 .. fromIntegral n - 1]
    v' = Raw.convert (unIx v)
    (res,_) = olsRegress [t] v'
  in
    (res ! 1, res ! 0)
{-# INLINE regressLinear #-}

regressQuadratic :: Array i Double -> (Double, Double, Double)
regressQuadratic v =
  let
    n = _length v
    t = fromList [0 .. fromIntegral n - 1]
    t2 = _fmap (** 2) t
    v' = Raw.convert (unIx v)
    (res,_) = olsRegress [t, t2] v'
  in
    (res ! 2, res ! 0, res ! 1)
{-# INLINE regressQuadratic #-}
