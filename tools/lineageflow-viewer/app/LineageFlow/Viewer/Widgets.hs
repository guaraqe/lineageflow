{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Viewer.Widgets
  ( TimeW (..)
  , TimeS (..)
  , getTime
  , ViewW (..)
  , ViewS (..)
  , getView
  , ColorW (..)
  , ColorS (..)
  , getColor
  , ImageW (..)
  , ImageS (..)
  , getImage
  , VertexW (..)
  , VertexS (..)
  , getVertex
  , ScalarW (..)
  , ScalarS (..)
  , getScalar
  , VectorW (..)
  , VectorS (..)
  , getVector
  , TensorW (..)
  , TensorS (..)
  , getTensor
  , GroupW (..)
  , GroupS (..)
  , getGroup
  , TriW (..)
  , TriS (..)
  , getTri
  , Widgets (..)
  , setupWidgets
  , States (..)
  , getStates
  , measuresFromStates
  ) where

import LineageFlow.Prelude hiding (set, on)

import LineageFlow.Viewer.Input
import LineageFlow.Viewer.Display

import qualified Data.Map as Map

import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import GHC.Float

import Data.Text (Text)

--------------------------------------------------------------------------------

data TimeW = TimeW
  { timeW_time :: HScale
  }

data TimeS = TimeS
  { timeS_time :: Time
  } deriving Eq

getTime :: TimeW -> IO TimeS
getTime (TimeW time) =
  TimeS <$>
    (Time . floor <$> rangeGetValue time)

timeW :: Int -> IO TimeW
timeW n =
  TimeW <$> hscale 0 (fromIntegral n - 1) 1 0

--------------------------------------------------------------------------------

data ViewW = ViewW
  { viewW_zoom :: VScale
  , viewW_phi :: HScale
  , viewW_theta :: HScale
  , viewW_psi :: HScale
  }

data ViewS = ViewS
  { viewS_zoom :: Float
  , viewS_phi :: Float
  , viewS_theta :: Float
  , viewS_psi :: Float
  } deriving Eq

getView :: ViewW -> IO ViewS
getView (ViewW zoom phi theta psi) =
  ViewS <$>
    (double2Float <$> rangeGetValue zoom) <*>
    (double2Float <$> rangeGetValue phi) <*>
    (double2Float <$> rangeGetValue theta) <*>
    (double2Float <$> rangeGetValue psi)

viewW :: IO ViewW
viewW =
  ViewW <$>
    vscale 0.5 10 0.1 3 <*>
    hscale (-180) 180 1 0 <*>
    hscale (-180) 180 1 0 <*>
    hscale (-180) 180 1 0

--------------------------------------------------------------------------------

data ColorW = ColorW
  { colorW_centerScale :: HScale
  , colorW_center :: HScale
  , colorW_widthScale :: HScale
  , colorW_width :: HScale
  }

data ColorS = ColorS
  { colorS_center :: Double
  , colorS_width :: Double
  } deriving Eq

getColor :: ColorW -> IO ColorS
getColor (ColorW centerScale center widthScale width) = do
  cs <- rangeGetValue centerScale
  c <- rangeGetValue center
  ws <- rangeGetValue widthScale
  w <- rangeGetValue width
  return (ColorS (c * 10 ** cs) (w * 10 ** ws))

colorW :: IO ColorW
colorW =
  ColorW <$>
    hscale (-10) 10 1 0 <*>
    hscale (-1) 1 0.01 0 <*>
    hscale (-10) 10 1 0 <*>
    hscale 0.1 2 0.01 0.1

--------------------------------------------------------------------------------

data ImageW = ImageW
  { imageW_image :: ComboBox
  }

data ImageS = ImageS
  { imageS_image :: ImageChoice
  } deriving Eq

getImage :: ImageW -> IO ImageS
getImage (ImageW image) =
  ImageS <$>
    (maybe ImageScalar getImageChoice <$> comboBoxGetActiveText image)

imageW :: IO ImageW
imageW =
  ImageW <$> combobox ["Scalar", "Vector", "Tensor", "Triangulation","Selection"]

--------------------------------------------------------------------------------

data VertexW = VertexW
  { vertexW_choice :: ComboBox
  }

data VertexS = VertexS
  { vertexS_choice :: Text
  } deriving Eq

getVertex :: VertexW -> IO VertexS
getVertex (VertexW vertex) =
  fmap VertexS $
    fromMaybe "all" <$> comboBoxGetValue vertex

vertexW :: [Text] -> IO VertexW
vertexW = fmap VertexW . combobox

--------------------------------------------------------------------------------

data ScalarW = ScalarW
  { scalarW_choice :: ComboBox
  , scalarW_box :: VBox
  }

data ScalarS = ScalarS
  { scalarS_choice :: Maybe Text
  } deriving Eq

getScalar :: ScalarW -> IO ScalarS
getScalar (ScalarW choice _) =
  ScalarS <$>
    comboBoxGetValue choice

scalarW :: [Text] -> IO ScalarW
scalarW choice = do
  scalarChoice <- combobox choice
  scalarLabel <- mkLabel (Just "Scalar")

  scalarBox <- vBoxNew False 4
  set
    scalarBox
    [ containerChild := scalarLabel
    , containerChild := scalarChoice
    ]

  return (ScalarW scalarChoice scalarBox)

--------------------------------------------------------------------------------

data GroupW = GroupW
  { groupW_choice :: ComboBox
  , groupW_box :: VBox
  }

data GroupS = GroupS
  { groupS_choice :: Maybe Text
  } deriving Eq

getGroup :: GroupW -> IO GroupS
getGroup (GroupW choice _) =
  GroupS <$>
    comboBoxGetValue choice

groupW :: [Text] -> IO GroupW
groupW choice = do
  groupChoice <- combobox choice
  groupLabel <- mkLabel (Just "Selection")

  groupBox <- vBoxNew False 4
  set
    groupBox
    [ containerChild := groupLabel
    , containerChild := groupChoice
    ]

  return (GroupW groupChoice groupBox)

--------------------------------------------------------------------------------

data VectorW = VectorW
  { vectorW_scale :: HScale
  , vectorW_choice :: ComboBox
  , vectorW_box :: VBox
  }

data VectorS = VectorS
  { vectorS_scale :: Double
  , vectorS_choice :: Maybe Text
  } deriving Eq

getVector :: VectorW -> IO VectorS
getVector (VectorW scale choice _) =
  VectorS <$>
    rangeGet scale <*>
    comboBoxGetValue choice

vectorW :: [Text] -> IO VectorW
vectorW choice = do
  vectorScale <- hscale 1 20 1 10
  vectorChoice <- combobox choice
  vectorLabel <- mkLabel (Just "Vector")
  vectorScaleLabel <- mkLabel (Just "Vector Scale")

  vectorBox <- vBoxNew False 4
  set
    vectorBox
    [ containerChild := vectorLabel
    , containerChild := vectorChoice
    , containerChild := vectorScaleLabel
    , containerChild := vectorScale
    ]
  return (VectorW vectorScale vectorChoice vectorBox)



--------------------------------------------------------------------------------

data TensorW = TensorW
  { tensorW_scale :: HScale
  , tensorW_choice :: ComboBox
  , tensorW_box :: VBox
  }

data TensorS = TensorS
  { tensorS_scale :: Double
  , tensorS_choice :: Maybe Text
  } deriving Eq

getTensor :: TensorW -> IO TensorS
getTensor (TensorW scale choice _) =
  TensorS <$>
    rangeGet scale <*>
    comboBoxGetValue choice

tensorW :: [Text] -> IO TensorW
tensorW choice = do
  tensorScale <- hscale 1 20 1 10
  tensorChoice <- combobox choice
  tensorLabel <- mkLabel (Just "Tensor")
  tensorScaleLabel <- mkLabel (Just "Tensor Scale")

  tensorBox <- vBoxNew False 4
  set
    tensorBox
    [ containerChild := tensorLabel
    , containerChild := tensorChoice
    , containerChild := tensorScaleLabel
    , containerChild := tensorScale
    ]
  return (TensorW tensorScale tensorChoice tensorBox)


--------------------------------------------------------------------------------

data TriW = TriW
  { triW_choice :: ComboBox
  , triW_scalar :: ComboBox
  , triW_box :: VBox
  }

data TriS = TriS
  { triS_choice :: Maybe Text
  , triS_scalar :: Maybe Text
  } deriving Eq

getTri :: TriW -> IO TriS
getTri (TriW choice scalar _) =
  TriS <$>
    comboBoxGetValue choice <*>
    comboBoxGetValue scalar

triW :: [Text] -> [Text] -> IO TriW
triW choice scalar = do
  triChoice <- combobox choice
  triScalarChoice <- combobox scalar
  triLabel <- mkLabel (Just "Triangulation")
  triScalarLabel <- mkLabel (Just "Triangulation Scalar")

  triBox <- vBoxNew False 4
  set
    triBox
    [ containerChild := triLabel
    , containerChild := triChoice
    , containerChild := triScalarLabel
    , containerChild := triScalarChoice
    ]
  return (TriW triChoice triScalarChoice triBox)


--------------------------------------------------------------------------------

data CanvasW = CanvasW
  { canvasW_canvas :: GLDrawingArea
  }

data CanvasS = CanvasS
  { canvasS_size :: (Int, Int)
  } deriving Eq

getCanvas :: CanvasW -> IO CanvasS
getCanvas (CanvasW canvas) =
  CanvasS <$> widgetGetSize canvas

--------------------------------------------------------------------------------

data Widgets = Widgets
  { widgets_time :: TimeW
  , widgets_view :: ViewW
  , widgets_color :: ColorW
  , widgets_image :: ImageW
  , widgets_vertex :: VertexW
  , widgets_scalar :: ScalarW
  , widgets_vector :: VectorW
  , widgets_tensor :: TensorW
  , widgets_group :: GroupW
  , widgets_tri :: TriW
  , widgets_canvas :: CanvasW
  , widgets_box :: VBox
  }

mkLabel :: Maybe Text -> IO Label
mkLabel = labelNew

setLabel :: Label -> Text -> IO ()
setLabel = labelSetMarkup

setupWidgets ::
  Window ->
  GLDrawingArea ->
  Int ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  [Text] ->
  IO Widgets
setupWidgets window canvas times tc scalar vector tensor group tri triScalar = do
  -- Defines behavior on closing
  onDestroy window mainQuit

  -- Set window general properties
  set
    window
    [ containerBorderWidth := 8
    , windowTitle := ("LineageFlow Viewer" :: Text)
    ]

  -- Time
  timeLabel <- mkLabel Nothing
  setLabel timeLabel "<b><u>Time</u></b>"

  timeWidget <- timeW times

  timePlus <- buttonNewWithLabel ("+" :: Text)
  timeLess <- buttonNewWithLabel ("-" :: Text)

  onClicked timePlus $ do
    t <- rangeGet (timeW_time timeWidget)
    if t < fromIntegral times - 1
      then rangeSetValue (timeW_time timeWidget) (t + 1)
      else return ()

  onClicked timeLess $ do
    t <- rangeGet (timeW_time timeWidget)
    if t > 0
      then rangeSetValue (timeW_time timeWidget) (t - 1)
      else return ()

  timeButtons <- vBoxNew False 4
  set
    timeButtons
    [ containerChild := timePlus
    , containerChild := timeLess
    ]

  timeBox <- hBoxNew False 4
  set
    timeBox
    [ containerChild := timeW_time timeWidget
    , containerChild := timeButtons
    ]

  -- View
  viewLabel <- mkLabel Nothing
  setLabel viewLabel "<b><u>View</u></b>"

  viewWidget <- viewW

  angleBox <- vBoxNew False 4
  set
    angleBox
    [ containerChild := viewW_phi viewWidget
    , containerChild := viewW_theta viewWidget
    , containerChild := viewW_psi viewWidget
    ]

  viewBox <- hBoxNew False 4
  set
    viewBox
    [ containerChild := angleBox
    , containerChild := viewW_zoom viewWidget
    ]

  -- Color
  colorLabel <- mkLabel Nothing
  setLabel colorLabel "<b><u>Color</u></b>"

  colorWidget <- colorW

  colorBox <- vBoxNew False 4
  set
    colorBox
    [ containerChild := colorW_centerScale colorWidget
    , containerChild := colorW_center colorWidget
    , containerChild := colorW_widthScale colorWidget
    , containerChild := colorW_width colorWidget
    ]

  -- Image

  imageLabel <- mkLabel Nothing
  setLabel imageLabel "<b><u>Image</u></b>"

  imageWidget <- imageW

  imageBox <- vBoxNew False 4
  set
    imageBox
    [ containerChild := imageW_image imageWidget
    ]



  -- Measures

  vertexWidget <- vertexW tc

  ---------

  scalarWidget <- scalarW scalar
  vectorWidget <- vectorW vector
  tensorWidget <- tensorW tensor
  groupWidget <- groupW group
  triWidget <- triW tri triScalar

  ---------

  measureBox <- vBoxNew False 4
  set measureBox [ containerChild := scalarW_box scalarWidget ]

  measureLabel <- mkLabel Nothing
  setLabel measureLabel "<b><u>Measures</u></b>"

  void (on measureBox add widgetShowAll)

  ---------

  sidebar <- vBoxNew False 4
  set
    sidebar
    [ containerChild := timeLabel
    , containerChild := timeBox
    , containerChild := viewLabel
    , containerChild := viewBox
    , containerChild := colorLabel
    , containerChild := colorBox
    , containerChild := imageLabel
    , containerChild := imageBox
    , containerChild := measureLabel
    , containerChild := measureBox
    ]

  notebook <- notebookNew
  notebookAppendPage notebook sidebar ("Choices" :: String)

  total <- hBoxNew False 4
  set
    total
    [ containerChild := notebook
    , containerChild := canvas
    ]

  set window [ containerChild := total ]

  return $
    Widgets
      timeWidget
      viewWidget
      colorWidget
      imageWidget
      vertexWidget
      scalarWidget
      vectorWidget
      tensorWidget
      groupWidget
      triWidget
      (CanvasW canvas)
      measureBox

--------------------------------------------------------------------------------

data States = States
  { states_time :: TimeS
  , states_view :: ViewS
  , states_color :: ColorS
  , states_image :: ImageS
  , states_vertex :: VertexS
  , states_scalar :: ScalarS
  , states_vector :: VectorS
  , states_tensor :: TensorS
  , states_group :: GroupS
  , states_tri :: TriS
  , states_canvas :: CanvasS
  } deriving Eq

getStates :: Widgets -> IO States
getStates (Widgets time view color image vertex scalar vector tensor group tri canvas _) =
  States <$>
    getTime time <*>
    getView view <*>
    getColor color <*>
    getImage image <*>
    getVertex vertex <*>
    getScalar scalar <*>
    getVector vector <*>
    getTensor tensor  <*>
    getGroup group  <*>
    getTri tri <*>
    getCanvas canvas

--------------------------------------------------------------------------------

measuresFromStates :: States -> Measures -> Selection
measuresFromStates
  (States _ _ _ _ tc scalar vector tensor group tri _)
  (Measures scalars vectors tensors groups tris triScalars) =
  Selection
    (vectors Map.! "position")
    (do s <- scalarS_choice scalar; Map.lookup s scalars)
    (do v <- vectorS_choice vector; Map.lookup v vectors)
    (do t <- tensorS_choice tensor; Map.lookup t tensors)
    (do g <- groupS_choice group; Map.lookup g groups)
    (do t <- triS_choice tri; ts <- tris; Map.lookup t ts)
    (do t <- triS_scalar tri; ts <- triScalars; Map.lookup t ts)


--------------------------------------------------------------------------------

hscale :: Double -> Double -> Double -> Double -> IO HScale
hscale minVal maxVal stepVal initVal = do
  s <- hScaleNewWithRange minVal maxVal stepVal
  rangeSetValue s initVal
  return s

vscale :: Double -> Double -> Double -> Double -> IO VScale
vscale minVal maxVal stepVal initVal = do
  s <- vScaleNewWithRange minVal maxVal stepVal
  rangeSetValue s initVal
  return s

combobox :: [Text] -> IO ComboBox
combobox l = do
  c <- comboBoxNewText
  mapM_ (comboBoxAppendText c) l
  comboBoxSetActive c 0
  return c

comboBoxGetValue w = do
  v <- widgetGetVisible w
  if v
    then comboBoxGetActiveText w
    else return Nothing

rangeGet w = do
  v <- widgetGetVisible w
  if v
    then rangeGetValue w
    else return 1
