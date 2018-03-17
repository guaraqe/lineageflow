{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import LineageFlow.Prelude hiding (rotate)
import LineageFlow.IO.CBOR
import LineageFlow.Viewer.Input
import LineageFlow.Viewer.Display
import LineageFlow.Viewer.Widgets
import LineageFlow.Statistics

import Graphics.UI.Gtk
import Graphics.UI.Gtk.OpenGL
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.FTGL as FTGL

import Data.Scientific

import Paths_lineageflow_viewer

import Options.Applicative

import qualified Data.Yaml as Yaml

import Data.IORef
import GHC.Float

main :: IO ()
main = join . execParser $
  info (helper <*> parser) $
  fullDesc <>
  header "lineageflow-viewer" <>
  progDesc "Visualisation of measures"
  where
    parser :: Parser (IO ())
    parser = work <$>
      strOption
      ( long "config" <>
        short 'c' <>
        metavar "CONFIG" <>
        help "Configuration file"
      )

work :: FilePath -> IO ()
work cfgPath = do
  input <- Yaml.decodeFileEither cfgPath >>= \case
    Left e -> error (show e)
    Right i -> return i

  measures <- getMeasures cbor input

  let
    time = measuresTime measures
    (scalar, vector, tensor, group, tri, triScalar) = getLabels measures

  initGUI
  window <- windowNew

  initGL
  canvas <-
    glDrawingAreaNew =<<
    glConfigNew [GLModeRGBA, GLModeDepth, GLModeDouble]

  widgets <-
    setupWidgets
      window
      canvas
      time
      ["subdomain"]
      ("nothing":scalar)
      ("nothing":filter (/= "position") vector)
      ("nothing":tensor)
      ("nothing":group)
      ("nothing":tri)
      ("nothing":triScalar)

  states <- getStates widgets
  let selection = measuresFromStates states measures

  setupAnimation canvas widgets states measures selection

  widgetShowAll window
  mainGUI

rescaleCanvas :: Double -> Vector -> Double -> IO ()
rescaleCanvas ratio (V3 x y z) radius' = let radius = radius' in
  ortho
    (ratio * (x - radius))
    (ratio * (x + radius))
    (y - radius)
    (y + radius)
    (z - radius)
    (y + radius)

lightPosition :: Vector -> Scalar -> Vertex4 Float
lightPosition (V3 x y z) r =
  Vertex4
    (double2Float $ (1 + r) * x)
    (double2Float $ (1 + r) * y)
    (double2Float $ (1 + r) * z)
    1

setupAnimation ::
  GLDrawingArea -> Widgets -> States -> Measures -> Selection -> IO ()
setupAnimation canvas widgets states measures selection = do
  widgetSetSizeRequest canvas 663 430

  statesIO <- newIORef states
  selectionIO <- newIORef selection

  let
    (center, radius) = positionCenter (selection_vertex selection)

  onRealize canvas . withGLDrawingArea canvas $ \_ -> do
    clearColor $= Color4 0.0 0.0 0.0 0.0
    matrixMode $= Projection
    loadIdentity
    depthFunc $= Just Less
    light (Light 0) $= Enabled
    lighting $= Enabled
    lightModelAmbient $= Color4 0.5 0.5 0.5 1
    diffuse (Light 0)  $= Color4 1 1 1 1
    blend              $= Enabled
    blendFunc          $= (SrcAlpha, OneMinusSrcAlpha)
    colorMaterial      $= Just (FrontAndBack, AmbientAndDiffuse)
    drawBuffer $= BackBuffers

  onExpose canvas $ \_ -> do
    withGLDrawingArea canvas $ \win -> do
      clear [DepthBuffer, ColorBuffer]
      position (Light 0) $= lightPosition center radius
      (ImageS imageNow) <- states_image <$> readIORef statesIO
      timeNow <- timeS_time . states_time <$> readIORef statesIO
      ColorS c w <- states_color <$> readIORef statesIO
      selectionNow <- readIORef selectionIO
      scaleNow <- chooseScale <$> readIORef statesIO
      displaySelection imageNow scaleNow (linearColor c w) timeNow selectionNow
      (width, height) <- widgetGetSize canvas
      (\s -> drawScale s width height) =<< readIORef statesIO
      glDrawableSwapBuffers win
    return True


  (\a -> timeoutAddFull a priorityDefaultIdle 25) $ do

    statesOld <- readIORef statesIO
    statesNew <- getStates widgets

    if statesOld == statesNew
      then return True
      else do

        (width, height) <- widgetGetSize canvas
        let ratio = fromIntegral width / fromIntegral height
        loadIdentity
        rescaleCanvas ratio center radius

        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))

        updateGeometry (states_view statesOld) (states_view statesNew) center
        widgetQueueDraw canvas

        let selectionNew = measuresFromStates statesNew measures

        replaceImage widgets statesOld statesNew
        replaceColor widgets selectionNew statesOld statesNew

        writeIORef statesIO statesNew
        writeIORef selectionIO selectionNew

        return True

  return ()

toLog :: Double -> (Double, Double)
toLog x =
  let y = fromIntegral (ceiling (logBase 10 x)) in (y, x / (10 ** y))

chooseScale :: States -> Double
chooseScale s =
  let
    image = imageS_image (states_image s)
  in
    case image of
      ImageScalar -> 1
      ImageVector -> vectorS_scale (states_vector s)
      ImageTensor -> tensorS_scale (states_tensor s)
      ImageSelection -> 1
      ImageTri -> 1

replaceImage :: Widgets -> States -> States -> IO ()
replaceImage widgets statesOld statesNew =
  let
    imageOld = imageS_image (states_image statesOld)
    imageNew = imageS_image (states_image statesNew)

    boxSelect ImageScalar = scalarW_box (widgets_scalar widgets)
    boxSelect ImageVector = vectorW_box (widgets_vector widgets)
    boxSelect ImageTensor = tensorW_box (widgets_tensor widgets)
    boxSelect ImageSelection = groupW_box (widgets_group widgets)
    boxSelect ImageTri = triW_box (widgets_tri widgets)

    measureBox = widgets_box widgets
  in
    if imageOld == imageNew
      then return ()
      else do
        containerRemove measureBox (boxSelect imageOld)
        containerAdd measureBox (boxSelect imageNew)

--------------------------------------------------------------------------------

drawScale :: States -> Int -> Int -> IO ()
drawScale states width height =
  let
    n = 100
    textHeight = 14
    colorRange = fmap (linearColor 0.5 1) (gradient n)
    scaleWidth = fromIntegral width / 40
    scaleHeight = fromIntegral height / 2
    leftVertexX = fromIntegral width - 2 * scaleWidth
    leftVertexY = fromIntegral height / 2 - scaleHeight / 2
    positionRange = fmap (\k -> leftVertexY + scaleHeight * k) (gradient n)

    colorCenter = colorS_center . states_color $ states
    colorWidth = colorS_width . states_color $ states

    numberTop = colorCenter + colorWidth / 2
    numberBot = colorCenter - colorWidth / 2

  in do
    font <- setupFont
    preservingMatrix $ do
      matrixMode $= Projection
      loadIdentity
      ortho 0 (fromIntegral width) 0 (fromIntegral height) (-1) 1
      GL.color (Color3 (1 :: Double) 1 1)

      matrixMode $= Modelview 0
      preservingMatrix $ do
        loadIdentity

        lighting $= Disabled
        translate (Vector3 (leftVertexX + scaleWidth / 2) (leftVertexY - textHeight) 0)
        renderText font textHeight (renderNumber numberBot)
        translate (Vector3 0 (scaleHeight + textHeight * 1.5) 0)
        renderText font textHeight (renderNumber numberTop)
        translate (Vector3 (-leftVertexX - scaleWidth / 2) (-leftVertexY - scaleHeight - 0.5 * textHeight) 0)

        forM_ (zip colorRange positionRange) $ \(c,h) ->
          renderPrimitive GL.Quads $ do
            GL.color c
            drawSquare scaleWidth (scaleHeight / fromIntegral n) leftVertexX h
        lighting $= Enabled

gradient :: Int -> [Double]
gradient n =
  fmap (/ (fromIntegral n)) [0 .. fromIntegral n]

drawSquare :: Double -> Double -> Double -> Double -> IO ()
drawSquare dx dy x y = do
  vertex (Vertex3 x y 0)
  vertex (Vertex3 (x + dx) y 0)
  vertex (Vertex3 (x + dx) (y + dy) 0)
  vertex (Vertex3 x (y + dy) 0)

setupFont :: IO FTGL.Font
setupFont = do
  fontPath <- getDataFileName "attach/FreeSans.ttf"
  font <- createBufferFont fontPath
  setFontFaceSize font 14 14
  return font

renderText :: FTGL.Font -> Double -> String -> IO ()
renderText font n txt =
  let
    width = fromIntegral (length txt) * n / 3
  in do
    translate $ Vector3 (-width) 0 0
    renderFont font txt FTGL.All
    translate $ Vector3 width 0 0

renderNumber :: Double -> String
renderNumber = formatScientific Exponent (Just 2) . fromFloatDigits

--------------------------------------------------------------------------------

replaceColor :: Widgets -> Selection -> States -> States -> IO ()
replaceColor widgets selection statesOld statesNew =
  let
    imageOld = imageS_image (states_image statesOld)
    imageNew = imageS_image (states_image statesNew)

    choiceChoose ImageScalar state = scalarS_choice (states_scalar state)
    choiceChoose ImageVector state = vectorS_choice (states_vector state)
    choiceChoose ImageTensor state = tensorS_choice (states_tensor state)
    choiceChoose ImageSelection state = groupS_choice (states_group state)
    choiceChoose ImageTri state = triS_scalar (states_tri state)

    meanStdChoice ImageScalar =
      meanStdDSumWith id <$> selection_scalar selection
    meanStdChoice ImageVector =
      meanStdDSumWith norm <$> selection_vector selection
    meanStdChoice ImageTensor =
      meanStdDSumWith (norm . fmap norm) <$> selection_tensor selection
    meanStdChoice ImageSelection =
      minMaxDSum <$> selection_group selection
    meanStdChoice ImageTri =
      meanStdDSumWith id <$> selection_triScalar selection
  in
    if imageOld == imageNew &&
       choiceChoose imageOld statesOld == choiceChoose imageNew statesNew
      then return ()
      else do
        case meanStdChoice imageNew of
          Nothing -> return ()
          Just (m, std) ->
            let
              (cs,c) = toLog m
              (ws,w) = toLog (sqrt std)
            in do
              rangeSetValue (colorW_centerScale (widgets_color widgets)) cs
              rangeSetValue (colorW_center (widgets_color widgets)) c
              rangeSetValue (colorW_widthScale (widgets_color widgets)) ws
              rangeSetValue (colorW_width (widgets_color widgets)) (2 * w)

minMaxDSum :: DSumMapA Time Cell Int -> (Scalar, Scalar)
minMaxDSum x = (m,v)
  where
    rm = _fmap unIx . unIx . getCompose
    (mi,ma) = minMaxL . toList . rm . _fmap fromIntegral $ x
    m = (mi + ma) / 2
    v = ((ma - mi) / 2) ** 2

updateGeometry :: ViewS -> ViewS -> Vector -> IO ()
updateGeometry
  old@(ViewS oldZoom oldPhi oldTheta oldPsi)
  new@(ViewS newZoom newPhi newTheta newPsi)
  (V3 x y z) = if old == new then return () else do
  preservingMatrix $ do
    matrixMode $= Modelview 0
    translate (Vector3 x y z)
    scale (newZoom / oldZoom) (newZoom / oldZoom) (newZoom / oldZoom)
    rotate (-oldPsi) (Vector3 0 0 1)
    rotate (-oldTheta) (Vector3 1 0 0)
    rotate (newPhi - oldPhi) (Vector3 0 1 0)
    rotate (newTheta) (Vector3 1 0 0)
    rotate (newPsi) (Vector3 0 0 1)
    translate (Vector3 (-x) (-y) (-z))
  return ()
