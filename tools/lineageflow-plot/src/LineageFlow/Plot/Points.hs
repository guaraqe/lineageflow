module LineageFlow.Plot.Points
  ( points
  , scatter
  ) where


import LineageFlow.Prelude
import LineageFlow.Plot.Types
import LineageFlow.Plot.Utils

import Graphics.Rendering.Chart

import Data.Colour hiding (over)
import Data.Colour.SRGB

import Control.Lens (_1,_2)

--------------------------------------------------------------------------------

points ::
  (CFunctor f, CFoldable f, Dom f Double) =>
  PointConfig -> f Double -> PlotData Double Double
points config =
  scatter config . _imap (\n v -> (fromIntegral n,v)) . _toList

--------------------------------------------------------------------------------

scatter ::
  PointConfig -> [(Double,Double)] -> PlotData Double Double
scatter (PointConfig t c bc bw r s) vals = PlotData $ \plot_config ->
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
        def
      & plot_points_title .~ t
      & plot_points_style .~ PointStyle (fromColor c) (fromColor bc) bw r s
      & plot_points_values .~ vals'

  in
    [0 .= toPlot bounds, 1 .= toPlot valsPlot]
