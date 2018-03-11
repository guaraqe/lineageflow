{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Plot
  (
  -- Global Histograms
    globalHistogram
  -- Slice Histograms
  , sliceHistogram
  -- Global graph
  , globalGraph
  -- List of graphs
  , listGraph
  -- Reexports
  , module Export
  ) where

import LineageFlow.Prelude hiding (time, range)
import LineageFlow.Statistics

import qualified LineageFlow.ArrayU as ArrayU

import LineageFlow.Plot.Types as Export
import LineageFlow.Plot.Render as Export
import LineageFlow.Plot.Lines as Export
import LineageFlow.Plot.Points as Export
import LineageFlow.Plot.Histogram as Export
import LineageFlow.Plot.Utils as Export
  (colorGradientRGB, (.=))
import LineageFlow.Plot.Interface as Export

--------------------------------------------------------------------------------

removeIndex :: DSumMapL i j a -> [ArrayU a]
removeIndex = _fmap unIx . unIx . getCompose

--------------------------------------------------------------------------------

globalHistogram ::
  PlotConfig -> LayoutConfig -> Int -> DSumMapL i j Scalar -> PlotLayout
globalHistogram config layout n measure =
  let
    vals = ArrayU.concat $ removeIndex $ _fmap (apply $ config ^. plotConfig_xAxis . axisConfig_unit) measure
    apply (a,b) = \t -> a + b * t
  in
    makePlot layout config $ barsN n (_toList vals)

--------------------------------------------------------------------------------

sliceHistogram :: PlotConfig -> LayoutConfig -> Int -> DSumMapL i j Scalar -> [PlotLayout]
sliceHistogram config layout bins measure =
  let
    apply (a,b) = \t -> a + b * t
    points = removeIndex $ _fmap (apply $ config ^. plotConfig_xAxis . axisConfig_unit) measure
    (centers, vals) = histogram bins points
    (lo, hi) = minMaxL vals
    config' =
      config &
        plotConfig_yAxis . axisConfig_min .~
          (config ^. plotConfig_yAxis . axisConfig_min <|> Just lo) &
        plotConfig_yAxis . axisConfig_max .~
          (config ^. plotConfig_yAxis . axisConfig_max <|> Just hi)
  in
    flip _imap vals $ \n v ->
      makePlot (configMaker layout n) config' $ bars (_toList centers) (_toList v)

--------------------------------------------------------------------------------

globalGraph :: PlotConfig -> LayoutConfig -> DSumMapL i j Scalar -> PlotLayout
globalGraph config layout (Compose lv) =
  let
    grph = zip [0 ..] $ fmap (meanStdWith id) (unIx lv)
    cleanGraph = filter (not . isNaN . fst . snd) grph
  in
    makePlot layout config $ pathStd def cleanGraph

listGraph :: PlotConfig -> ConfigMaker -> [DSumMapL i j Scalar] -> [PlotLayout]
listGraph config mkPlotConfig lv =
  let
    vals = fmap (fmap (meanStdWith id)) (fmap (unIx . getCompose) lv)
    graphs = fmap (zip [1 ..]) vals
    cleanGraphs = fmap(filter (not . isNaN . fst . snd)) graphs
  in
    zipWith
      (\n gs ->
         makePlot (mkPlotConfig n) config $ pathStd def gs
      )
      [0 :: Int  .. ]
      cleanGraphs
