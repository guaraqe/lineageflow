module LineageFlow.Plot.Histogram
  ( Bars
  , toBars
  , plotHistogram
  , bars
  , barsN
  ) where

import LineageFlow.Prelude hiding (range)
import LineageFlow.Plot.Style
import LineageFlow.Plot.Types
import LineageFlow.Plot.Utils

import Graphics.Rendering.Chart
import Data.Colour
import Data.Colour.SRGB
import Control.Lens (_1,_2)

import qualified LineageFlow.ArrayU as ArrayU
import Statistics.Sample.Histogram (range)

type Bars = [(Double,[Double])]

toBars :: ArrayU Double -> ArrayU Double -> Bars
toBars c v = zipWith (\x y -> (x,[y])) (toList c) (toList v)

plotHistogram :: Bars -> Double -> Double -> PlotData Double Double
plotHistogram values lo hi = PlotData $ \config ->
  let
    trickX = fst (head values)
    trickValue = [(trickX,[lo]),(trickX,[hi])]

    trickBar =
        trickHistogramStyle
      & plot_bars_values .~ trickValue

    bars =
        histogramStyle (sRGB 0 0 1) (Just black)
      & plot_bars_values .~ values
  in
    [(-1,plotBars trickBar), (0,plotBars bars)]

barsN :: Int -> [Double] -> PlotData Double Double
barsN n vals = bars bins vals
  where
    (mi,ma) = range n (ArrayU.fromList vals)
    gap = (ma - mi) / fromIntegral n
    bins = fmap (\k -> mi + k * gap) [0 .. fromIntegral n - 1]

bars :: [Double] -> [Double] -> PlotData Double Double
bars bins probs = PlotData $ \config ->
  let
    gap = bins !! 1 - bins !! 0
    binMin = head bins - gap / 2
    binMax = last bins + gap / 2

    xMin = fromMaybe binMin (config ^. plotConfig_xAxis . axisConfig_min)
    xMax = fromMaybe binMax (config ^. plotConfig_xAxis . axisConfig_max)

    vals = zip bins probs

    vals' = removeWith (view _1) (set _1) xMin xMax vals

    (_,yMax) =
      getBounds
        (config ^. plotConfig_yAxis . axisConfig_min)
        (config ^. plotConfig_yAxis . axisConfig_max)
        [fmap (view _2) vals']

    bounds =
        def
      & plot_points_values .~ [(xMin,0), (xMax,yMax)]

    vals'' = replaceWith (view _2) (set _2) 0 yMax vals'

    bars =
        histogramStyle (sRGB 0 0 1) (Just black)
      & plot_bars_values .~ fmap (second pure) vals''
  in
    [(-1,toPlot bounds), (0,plotBars bars)]
