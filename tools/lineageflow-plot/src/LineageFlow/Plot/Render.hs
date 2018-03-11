{-# LANGUAGE LambdaCase #-}

module LineageFlow.Plot.Render
  (
  -- * Plots
    savePlot
  , savePlotWith
  , showPlot
  , showPlotWith
  -- * Frames
  , saveFrames
  , saveFramesWith
  , showFrames
  , showFramesWith
  -- * Videos
  , saveVideo
  , saveVideoWith
  , showVideo
  , showVideoWith
  -- * Settings
  , foSmall
  -- * Reexports
  , module Export
  ) where

import LineageFlow.Plot.Types

import LineageFlow.Prelude

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Backend.Cairo as Export
  ( FileOptions (..)
  , FileFormat (..)
  , fo_size
  , fo_format )
import Graphics.Rendering.Chart.Grid

import System.Directory
import System.Process
import System.FilePath
import System.IO.Temp

--------------------------------------------------------------------------------

fileExt :: FileFormat -> String
fileExt PNG = "png"
fileExt SVG = "svg"
fileExt PS = "ps"
fileExt PDF = "pdf"

foSmall :: FileOptions
foSmall = FileOptions (400,300) PDF

xdgOpen :: FilePath -> IO ()
xdgOpen path = callCommand $ "xdg-open " <> path

xdgOpenList :: [FilePath] -> IO ()
xdgOpenList paths =
  let
    path = head paths
  in
    callCommand $
      "`xdg-mime query filetype " <> path <>
      " | xargs xdg-mime query default | cut -f1 -d'.'`" <>
      concatMap (' ':) paths

removeIfExists :: FilePath -> IO ()
removeIfExists path =
  doesFileExist path >>= \case
    False -> return ()
    True -> removeFile path

--------------------------------------------------------------------------------

renderPlotLayout :: PlotLayout -> Renderable ()
renderPlotLayout (PlotLayout l) = gridToRenderable l

--------------------------------------------------------------------------------

savePlot :: FilePath -> PlotLayout -> IO ()
savePlot = savePlotWith def

savePlotWith :: FileOptions -> FilePath -> PlotLayout -> IO ()
savePlotWith opt path rend =
  let
    ext = opt ^. fo_format
  in
    void $ renderableToFile opt (path <.> fileExt ext) (renderPlotLayout rend)

showPlot :: PlotLayout -> IO ()
showPlot = showPlotWith def

showPlotWith :: FileOptions -> PlotLayout -> IO ()
showPlotWith opt rend =
  let
    ext = opt ^. fo_format
    tpl = "plot." <> fileExt ext

  in
    withSystemTempFile tpl $ \path _ -> do
      savePlotWith opt (dropExtension path) rend
      xdgOpen path

--------------------------------------------------------------------------------

saveFrames :: FilePath -> [PlotLayout] -> IO ()
saveFrames = saveFramesWith def

saveFramesWith :: FileOptions -> FilePath -> [PlotLayout] -> IO ()
saveFramesWith opt folder rends = do
  createDirectoryIfMissing True folder
  zipWithM_
    (\n rend ->
      savePlotWith opt (folder </> show n) rend
    )
    [0 :: Int ..]
    rends

showFrames :: [PlotLayout] -> IO ()
showFrames = showFramesWith def

showFramesWith :: FileOptions -> [PlotLayout] -> IO ()
showFramesWith opt rends =
  let
    ext = fileExt (opt ^. fo_format)
    files = fmap (\n -> show n <.> ext) [0 .. length rends - 1]
  in
    withSystemTempDirectory "frames" $ \folder -> do
      saveFramesWith opt folder rends
      xdgOpenList $ fmap (folder </>) files

--------------------------------------------------------------------------------

saveVideo :: FilePath -> [PlotLayout] -> IO ()
saveVideo = saveVideoWith def

saveVideoWith :: FileOptions -> FilePath -> [PlotLayout] -> IO ()
saveVideoWith opt path rends =
  let
    ext = fileExt (opt ^. fo_format)
  in
    withSystemTempDirectory "frames" $ \folder -> do
      saveFramesWith opt folder rends
      removeIfExists (path <> ".mp4")
      callCommand $
        "ffmpeg " <>
        "-framerate 30 " <>
        "-i " <> folder <> "/%d." <> ext <> " " <>
        "-c:v libx264 " <>
        "-r 30 " <>
        "-pix_fmt yuv420p " <>
        path <> ".mp4 " <>
        "2> /dev/null"

showVideo :: [PlotLayout] -> IO ()
showVideo = showVideoWith def

showVideoWith :: FileOptions -> [PlotLayout] -> IO ()
showVideoWith opt rends =
  withSystemTempFile "video.mp4" $ \path _ -> do
    saveVideoWith opt (dropExtension path) rends
    xdgOpen path

--------------------------------------------------------------------------------
