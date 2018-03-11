module LineageFlow.Plot.Lines
  ( lines
  , linesLog
  , linesStd
  , pathLin
  , pathLog
  , pathStd
  ) where


import LineageFlow.Prelude
import LineageFlow.Plot.Style
import LineageFlow.Plot.Types
import LineageFlow.Plot.Utils

import Graphics.Rendering.Chart

import Data.Colour hiding (over)

import Control.Lens (_1,_2)

--------------------------------------------------------------------------------

lines ::
  (CFunctor f, CFoldable f, Dom f Double) =>
  LineConfig -> f Double -> PlotData Double Double
lines line_config =
  pathLin line_config . _imap (\n v -> (fromIntegral n,v)) . _toList

linesLog ::
  (CFunctor f, CFoldable f, Dom f Double) =>
  LineConfig -> f Double -> PlotData LogValue LogValue
linesLog line_config =
  pathLog line_config . _imap (\n v -> (fromIntegral n,v)) . _toList

linesStd ::
  (CFunctor f, CFoldable f, Dom f (Double, Double)) =>
  LineConfig -> f (Double, Double) -> PlotData Double Double
linesStd line_config =
  pathStd line_config . _imap (\n v -> (fromIntegral n,v)) . _toList

--------------------------------------------------------------------------------

hack :: String -> String
hack s =
  let
    parts = words s
    readDouble :: String -> Double
    readDouble = read
  in
    case length parts of
      3 -> removeDot . show $ readDouble (parts !! 0) + readDouble (parts !! 2)
      _ -> s

removeDot :: String -> String
removeDot s =
  let
    (x,dot) = break (== '.') s
  in
    if tail dot == "0" then x else s

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

pathLin ::
  LineConfig -> [(Double,Double)] -> PlotData Double Double
pathLin line_config vals = PlotData $ \plot_config ->
  let
    (bvals, vals') =
        vals
      & rescale
          (plot_config ^. plotConfig_xAxis . axisConfig_unit)
          (plot_config ^. plotConfig_yAxis . axisConfig_unit)
      & bound
          (plot_config ^. plotConfig_xAxis . axisConfig_min)
          (plot_config ^. plotConfig_xAxis . axisConfig_max)
          (plot_config ^. plotConfig_yAxis . axisConfig_min)
          (plot_config ^. plotConfig_yAxis . axisConfig_max)

    bounds =
        def
      & plot_points_values .~ bvals
      & plot_points_style . point_color .~ transparent

    valsPlot =
        lineStyle
      & plot_lines_style . line_color .~ (fromColor $ line_config ^. lineConfig_color)
      & plot_lines_style . line_dashes .~ (line_config ^. lineConfig_pattern)
      & plot_lines_style . line_width .~ (line_config ^. lineConfig_width)
      & plot_lines_title .~ (line_config ^. lineConfig_name)
      & plot_lines_values .~ [vals']

  in
    [0 .= toPlot bounds, 1 .= toPlot valsPlot]

pathLog :: LineConfig -> [(Double,Double)] -> PlotData LogValue LogValue
pathLog line_config vals = PlotData $ \plot_config ->
  let
    (bvals, vals') =
        vals
      & rescale
          (plot_config ^. plotConfig_xAxis . axisConfig_unit)
          (plot_config ^. plotConfig_yAxis . axisConfig_unit)
      & bound
          (plot_config ^. plotConfig_xAxis . axisConfig_min)
          (plot_config ^. plotConfig_xAxis . axisConfig_max)
          (plot_config ^. plotConfig_yAxis . axisConfig_min)
          (plot_config ^. plotConfig_yAxis . axisConfig_max)

    bounds =
        def
      & plot_points_values .~ fmap (bimap LogValue LogValue) bvals
      & plot_points_style . point_color .~ transparent

    valsPlot =
        lineStyle
      & plot_lines_style . line_color .~ (fromColor $ line_config ^. lineConfig_color)
      & plot_lines_style . line_dashes .~ (line_config ^. lineConfig_pattern)
      & plot_lines_style . line_width .~ (line_config ^. lineConfig_width)
      & plot_lines_title .~ (line_config ^. lineConfig_name)
      & plot_lines_values .~ [fmap (bimap LogValue LogValue) vals']

  in
    [0 .= toPlot bounds, 1 .= toPlot valsPlot]

pathStd :: LineConfig -> [(Double, (Double, Double))] -> PlotData Double Double
pathStd line_config vals = PlotData $ \plot_config ->
  let
    apply (a,b) = \t -> a + b * t
    vals' = flip fmap vals $
        bimap
          (apply (plot_config ^. plotConfig_xAxis . axisConfig_unit))
          (bimap
            (apply (plot_config ^. plotConfig_yAxis . axisConfig_unit))
            (apply (plot_config ^. plotConfig_yAxis . axisConfig_unit))
          )

    (xMin,xMax) =
      getBounds
        (plot_config ^. plotConfig_xAxis . axisConfig_min)
        (plot_config ^. plotConfig_xAxis . axisConfig_max)
        [fmap (view _1) vals']

    vals'' = removeWith (view _1) (set _1) xMin xMax vals'

    (yMin,yMax) =
      getBounds
        (plot_config ^. plotConfig_yAxis . axisConfig_min)
        (plot_config ^. plotConfig_yAxis . axisConfig_max)
        [fmap (view (_2 . _1)) vals'']

    bounds =
        def
      & plot_points_values .~ [(xMin,yMin), (xMax,yMax)]
      & plot_points_style . point_color .~ transparent

    vals''' = replaceWith (view (_2 . _1)) (set (_2 . _1)) yMin yMax vals''

    meanPlot =
        lineStyle
      & plot_lines_style . line_color .~ (fromColor $ line_config ^. lineConfig_color)
      & plot_lines_style . line_dashes .~ (line_config ^. lineConfig_pattern)
      & plot_lines_style . line_width .~ (line_config ^. lineConfig_width)
      & plot_lines_title .~ (line_config ^. lineConfig_name)
      & plot_lines_values .~ pure [(n,a) | (n,(a,_)) <- vals''']

    stdPlot =
        stdStyle
      & plot_fillbetween_values .~ [ (n,(a-d,a+d)) | (n,(a,d)) <- vals''']
      & plot_fillbetween_style .~ FillStyleSolid (dissolve 0.5 $ fromColor $ line_config ^. lineConfig_color)

  in
    [0 .= toPlot bounds, 0 .= toPlot stdPlot, 1 .= toPlot meanPlot]
