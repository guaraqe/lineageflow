{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Plot.Types
  (
  -- * Plot Data
    PlotData (PlotData)
  , plotData
  -- * Plot Layout
  , PlotLayout (PlotLayout)
  , plotLayout
  , hcat
  , hcatWeight
  , vcat
  , vcatWeight
  , makePlot
  , ConfigMaker
  , configMaker
  -- * Reexports
  , module Data.Default.Class
  , module LineageFlow.Plot.Interface
  ) where

import LineageFlow.Plot.Interface
import LineageFlow.Plot.Style
import LineageFlow.Prelude hiding ((.|.))
import Data.List (sortBy)

import Control.Lens hiding ((<.>), (&))
import Data.Default.Class
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Grid
import Data.Semigroup hiding ((<>))

--------------------------------------------------------------------------------

newtype PlotData x y = PlotData
  { _plotData :: PlotConfig -> [(Int, Plot x y)]
  } deriving (Semigroup, Monoid)

$(makeLenses ''PlotData)

--------------------------------------------------------------------------------

newtype PlotLayout = PlotLayout
  { _plotLayout :: Grid (Renderable ())
  }

$(makeLenses ''PlotLayout)

hcat :: PlotLayout -> PlotLayout -> PlotLayout
hcat = hcatWeight 1 1

hcatWeight :: Double -> Double -> PlotLayout -> PlotLayout -> PlotLayout
hcatWeight w1 w2 (PlotLayout l1) (PlotLayout l2) =
  PlotLayout $ weights (1,1) $ tval $ gridToRenderable $
    weights (w1,1) l1 .|. weights (w2,1) l2

vcat :: PlotLayout -> PlotLayout -> PlotLayout
vcat = vcatWeight 1 1

vcatWeight :: Double -> Double -> PlotLayout -> PlotLayout -> PlotLayout
vcatWeight w1 w2 (PlotLayout l1) (PlotLayout l2) =
  PlotLayout $ weights (1,1) $ tval $ gridToRenderable $
    weights (1,w1) l1 ./. weights (1,w2) l2

makePlot ::
  (PlotValue x, PlotValue y) =>
  LayoutConfig -> PlotConfig -> PlotData x y -> PlotLayout
makePlot config plotConfig (PlotData l) =
  PlotLayout $ tspan (toRenderable layout) (1,1)
  where
    xLabelStyle = axisStyle (config ^. layoutConfig_xLabel)
    yLabelStyle = axisStyle (config ^. layoutConfig_yLabel)

    layout =
        layoutStyle (config ^. layoutConfig_title) xLabelStyle yLabelStyle
      & layout_plots .~ fmap snd (sortBy (compare `on` fst) (l plotConfig))


--------------------------------------------------------------------------------

type ConfigMaker = Int -> LayoutConfig

configMaker :: LayoutConfig -> ConfigMaker
configMaker config = \n ->
  over layoutConfig_title (<> " Step " <> show n) config
