{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.Viewer.Elm
  ( viewer
  , viewerGlobal
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Viewer.View
import LineageFlow.Client.Viewer.Model
import LineageFlow.Client.Global.Model

import LineageFlow.Client.Widget.MDL

import Control.Applicative
import Control.Monad

import qualified Data.Map as Map

viewer :: MonadWidget t m => View t (App t m) Global
viewer =
  decorateView
    (viewerRunner <> modelToGlobal <> viewerGlobal) $
    elClass "div" "viewer-container"

viewerGlobal :: MonadWidget t m => View t (App t m) Global
viewerGlobal =
  changeViewUniq global_viewerModel viewerDiv

modelToGlobal :: MonadWidget t m => View t (App t m) Global
modelToGlobal = simpleView $ \global -> do
  fields <- getFields

  model <- holdUniqDyn $ fmap (view global_viewerModel) global

  final <- delay 0.5 $ updated $ liftA2 runVModel model fields

  return $ fmap (set global_viewer) final

viewerRunner :: MonadWidget t m => View t (App t m) Global
viewerRunner = simpleView $ \global ->
  elClass "div" "viewer" $ do

    vquery <- holdUniqDyn $ fmap (view global_viewer) global

    runnerButton <- materialButton "Run Viewer"

    runnerDyn <- (delay 0.5 =<<) $
      attachPromptlyDyn vquery <$>
        may (\query -> runViewer query runnerButton) vquery
        --runViewer (fmap fromJust vquery) runnerButton

    return $
      ffor runnerDyn $
        \(qu,n) ->
          case qu of
            Nothing -> id
            Just q -> over global_viewerRunning (Map.insert n (q, StatusRunning))

may ::
  MonadWidget t m =>
  (Dynamic t a -> m (Event t b)) -> Dynamic t (Maybe a) -> m (Event t b)
may f m =
  fmap switchPromptlyDyn $
  widgetHold (pure never) $
  fmap (f . pure) $
  fmapMaybe id (updated m)
