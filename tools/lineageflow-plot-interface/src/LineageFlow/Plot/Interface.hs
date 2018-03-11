{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LineageFlow.Plot.Interface
  (
    Scale (..)
  , _ScaleLin
  , _ScaleLog
  , PlotType (..)
  , _PlotTypeScalar
  , _PlotTypeVectorNorm
  , _PlotTypeVectorSquare
  , _PlotTypeVectorCoordinates
  , AxisConfig (..)
  , axisConfig_min
  , axisConfig_max
  , axisConfig_unit
  , PlotConfig (..)
  , plotConfig_xAxis
  , plotConfig_yAxis
  , Color (..)
  , color_red
  , color_green
  , color_blue
  , color_opacity
  , fromColor
  , LineConfig (..)
  , lineConfig_name
  , lineConfig_color
  , lineConfig_pattern
  , lineConfig_width
  , PointShape (..)
  , PointConfig (..)
  , pointConfig_title
  , pointConfig_color
  , pointConfig_borderColor
  , pointConfig_borderWidth
  , pointConfig_radius
  , pointConfig_shape
  , LayoutConfig (..)
  , layoutConfig_title
  , layoutConfig_xLabel
  , layoutConfig_yLabel
  , RangeConfig (..)
  , rangeConfig_min
  , rangeConfig_max
  , PlotInput (..)
  , plotInput_type
  , plotInput_plot
  , plotInput_layout
  , PlotCommandInputWith (..)
  , PlotCommandInput
  , PlotCommandPath
  , _GraphGlobal
  , _HistogramGlobal
  , _HistogramSlices
  ) where

import LineageFlow.Query

import Control.Lens
import Data.Aeson.Types
import Data.Default.Class
import GHC.Generics

import Data.Colour
import Data.Colour.SRGB

import Graphics.Rendering.Chart

--------------------------------------------------------------------------------

data Scale = ScaleLin | ScaleLog
  deriving (Show, Eq, Generic)

$(makePrisms ''Scale)

instance ToJSON Scale where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

instance FromJSON Scale where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

--------------------------------------------------------------------------------

data PlotType =
  PlotTypeScalar |
  PlotTypeVectorNorm |
  PlotTypeVectorSquare |
  PlotTypeVectorCoordinates
  deriving (Show, Eq, Generic)

$(makePrisms ''PlotType)

instance ToJSON PlotType where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

instance FromJSON PlotType where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

--------------------------------------------------------------------------------

data AxisConfig = AxisConfig
  { _axisConfig_min :: Maybe Double
  , _axisConfig_max :: Maybe Double
  , _axisConfig_unit :: (Double, Double)
  } deriving (Show, Eq, Generic)

$(makeLenses ''AxisConfig)

instance ToJSON AxisConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON AxisConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default AxisConfig where
  def = AxisConfig Nothing Nothing (0,1)

--------------------------------------------------------------------------------

data PlotConfig = PlotConfig
  { _plotConfig_xAxis :: AxisConfig
  , _plotConfig_yAxis :: AxisConfig
  } deriving (Show, Eq, Generic)

$(makeLenses ''PlotConfig)

instance ToJSON PlotConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON PlotConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default PlotConfig where
  def = PlotConfig def def

--------------------------------------------------------------------------------

data Color = Color
  { _color_red :: Double
  , _color_green :: Double
  , _color_blue :: Double
  , _color_opacity :: Double
  } deriving (Show, Eq, Generic)

$(makeLenses ''Color)

instance ToJSON Color where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON Color where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default Color where
  def = Color 0 0 0 1

fromColor :: Color -> AlphaColour Double
fromColor (Color r g b a) = withOpacity (sRGB r g b) a

--------------------------------------------------------------------------------

data LineConfig = LineConfig
  { _lineConfig_name :: String
  , _lineConfig_color :: Color
  , _lineConfig_pattern :: [Double]
  , _lineConfig_width :: Double
  } deriving (Show, Eq, Generic)

$(makeLenses ''LineConfig)

instance ToJSON LineConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON LineConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default LineConfig where
  def = LineConfig "" def [] 1

--------------------------------------------------------------------------------

data PointConfig = PointConfig
  { _pointConfig_title :: String
  , _pointConfig_color :: Color
  , _pointConfig_borderColor :: Color
  , _pointConfig_borderWidth :: Double
  , _pointConfig_radius :: Double
  , _pointConfig_shape :: PointShape
  } deriving (Show, Eq, Generic)

deriving instance Show PointShape
deriving instance Eq PointShape
deriving instance Generic PointShape
instance ToJSON PointShape
instance FromJSON PointShape

$(makeLenses ''PointConfig)

instance ToJSON PointConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON PointConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default PointConfig where
  def = PointConfig "" def def 0 1 PointShapeCircle

--------------------------------------------------------------------------------

data RangeConfig = RangeConfig
  { _rangeConfig_min :: Maybe Int
  , _rangeConfig_max :: Maybe Int
  } deriving (Show, Eq, Generic)

$(makeLenses ''RangeConfig)

instance ToJSON RangeConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON RangeConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default RangeConfig where
  def = RangeConfig Nothing Nothing

--------------------------------------------------------------------------------

data LayoutConfig = LayoutConfig
  { _layoutConfig_title :: String
  , _layoutConfig_xLabel :: String
  , _layoutConfig_yLabel :: String
  } deriving (Show, Eq, Generic)

$(makeLenses ''LayoutConfig)

instance ToJSON LayoutConfig where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON LayoutConfig where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Default LayoutConfig where
  def = LayoutConfig "" "" ""

--------------------------------------------------------------------------------

data PlotInput = PlotInput
  { _plotInput_type :: PlotType
  , _plotInput_plot :: PlotConfig
  , _plotInput_layout :: LayoutConfig
  } deriving (Show, Eq, Generic)

$(makeLenses ''PlotInput)

instance ToJSON PlotInput where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON PlotInput where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

--------------------------------------------------------------------------------

data PlotCommandInputWith a
  = GraphGlobal PlotInput a Scale
  | HistogramGlobal PlotInput a Int
  | HistogramSlices PlotInput a Int RangeConfig
  deriving (Show, Eq, Generic)

$(makePrisms ''PlotCommandInputWith)

type PlotCommandInput = PlotCommandInputWith MQuery
type PlotCommandPath = PlotCommandInputWith FilePath

instance ToJSON a => ToJSON (PlotCommandInputWith a) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON a => FromJSON (PlotCommandInputWith a) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
