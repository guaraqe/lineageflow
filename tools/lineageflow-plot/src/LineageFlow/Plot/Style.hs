module LineageFlow.Plot.Style
  ( axisStyle
  , lineStyle
  , stdStyle
  , layoutStyle
  , histogramStyle
  , trickHistogramStyle
  ) where

import LineageFlow.Prelude

import Graphics.Rendering.Chart

import Data.Colour hiding (over)
import Data.Colour.SRGB

import Data.Default.Class

--------------------------------------------------------------------------------

axisStyle :: PlotValue x => String -> LayoutAxis x
axisStyle title
  = laxis_title_style . font_size .~ 12
  $ laxis_title .~ title
  $ laxis_style . axis_label_style . font_size .~ 10
  $ def

--------------------------------------------------------------------------------

lineStyle :: PlotLines x y
lineStyle
  = plot_lines_style . line_color .~ opaque (sRGB 0 0 0.4)
  $ def

--------------------------------------------------------------------------------

stdStyle :: PlotFillBetween x y
stdStyle
  = plot_fillbetween_style .~ solidFillStyle (opaque (sRGB 0.2 0.6 1))
  $ def

--------------------------------------------------------------------------------

layoutStyle ::
  (PlotValue x, PlotValue y) =>
  String -> LayoutAxis x -> LayoutAxis y -> Layout x y
layoutStyle title xAxis yAxis
  = layout_title .~ title
  $ layout_grid_last .~ False
  $ layout_x_axis .~ xAxis
  $ layout_y_axis .~ yAxis
  $ def

--------------------------------------------------------------------------------

histogramStyle ::
  BarsPlotValue y =>
  Colour Double -> Maybe (Colour Double) -> PlotBars x y
histogramStyle fill line =
    def
  & plot_bars_style .~ BarsClustered
  & plot_bars_alignment .~ BarsCentered
  & plot_bars_spacing .~ BarsFixGap 0 10
  & plot_bars_item_styles .~
      repeat (solidFillStyle (opaque fill), solidLine 1.0 . opaque <$> line)

trickHistogramStyle :: BarsPlotValue y => PlotBars x y
trickHistogramStyle =
    def
  & plot_bars_item_styles .~
      repeat (solidFillStyle (opaque (sRGB 1 1 1)), Nothing)
  & plot_bars_singleton_width .~ 0
