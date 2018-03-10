module LineageFlow.Statistics.Histogram
  ( histogram
  , histogramPar
  , minMaxL
  ) where

import LineageFlow.Prelude hiding (range)
import qualified LineageFlow.ArrayU as ArrayU

import Statistics.Function
import Statistics.Sample.Histogram (histogram_, range)

minMaxL :: [ArrayU Double] -> (Double, Double)
minMaxL = minMax . ArrayU.concat
{-# INLINE minMaxL #-}

--------------------------------------------------------------------------------

histogram ::
  Int -> -- number of bins
  [ArrayU Double] -> -- data
  ( ArrayU Double -- centers of the bins
  , [ArrayU Double] -- values of bins for each time step
  )
histogram n lv =
  let
    lvClean = fmap (ArrayU.filter (not . isNaN)) lv
    (minrange,maxrange) = range n (ArrayU.concat lvClean)
    centers = binCenters minrange maxrange n
    hists = fmap (histogramT n minrange maxrange) lvClean
  in
    (centers, hists)
{-# INLINE histogram #-}

--------------------------------------------------------------------------------

histogramPar
  :: Int -> [[ArrayU Double]] -> (ArrayU Double, [ArrayU Double])
histogramPar n = histogram n . fmap ArrayU.concat
{-# INLINE histogramPar #-}

--------------------------------------------------------------------------------

histogramT :: Int -> Scalar -> Scalar -> ArrayU Scalar -> ArrayU Scalar
histogramT bins lo hi = \v ->
  let len = fromIntegral (_length v) in
  if len == 0
    then
      ArrayU.replicate bins 0
    else
      _fmap (/len) $ histogram_ bins lo hi v
{-# INLINE histogramT #-}


binCenters :: Double -> Double -> Int -> ArrayU Double
binCenters lo hi n =
  let
    step = (hi - lo) / fromIntegral n
  in
    _fmap (\k -> lo + k * step - step / 2) (ArrayU.enumFromN 1 (fromIntegral n))
{-# INLINE binCenters #-}
