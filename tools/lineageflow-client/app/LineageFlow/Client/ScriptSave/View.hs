{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Client.ScriptSave.View
  ( scriptSaveView
  ) where

import LineageFlow.Client.ScriptSave.Model

import LineageFlow.Client.Measurement.Model
import LineageFlow.Client.Measurement.View
import LineageFlow.Client.Algorithm.Model
import LineageFlow.Client.Algorithm.View
import LineageFlow.Client.Parameter.Model
import LineageFlow.Client.Parameter.View

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Widget.Common
import LineageFlow.Client.Widget.Dropdown
import LineageFlow.Client.Widget.MDL

import LineageFlow.Client.ScriptRun.View

scriptSaveView :: MonadWidget t m => View t (App t m) ScriptSaveModel
scriptSaveView =
  nameView <> saveView <> manyScriptView

nameView :: MonadWidget t m => View t (App t m) ScriptSaveModel
nameView = simpleView $ \_ -> do
  name <- view textInput_value <$> materialTextInput "name" "" "name"

  return $ fmap (set scriptSave_name) (updated name)


saveView :: MonadWidget t m => View t (App t m) ScriptSaveModel
saveView = simpleView $ \model -> do
  button <- materialButton "SAVE"

  name <- uniqView scriptSave_name model
  algs <- uniqView id model

  _ <- may (\d -> runScriptSave name d button) (fmap makeScript algs)

  return never

may ::
  MonadWidget t m =>
  (Dynamic t a -> m (Event t b)) -> Dynamic t (Maybe a) -> m (Event t b)
may f m =
  fmap switchPromptlyDyn $
  widgetHold (pure never) $
  fmap (f . pure) $
  fmapMaybe id (updated m)

--------------------------------------------------------------------------------

manyScriptView :: MonadWidget t m => View t (App t m) ScriptSaveModel
manyScriptView =
  elmToView
    (const ())
    singleScriptElm
    (makeUpdate (set scriptSave_algs))

--------------------------------------------------------------------------------

data AddScript = AddScript | AddLoop | AddImport

singleScriptElm :: forall t m . MonadWidget t m => Elm t (App t m) () [WidgetStep]
singleScriptElm =
  let
    elm :: MonadWidget t m => Elm t (App t m) AModel AModel
    elm = simpleElm singleScriptView
  in
    rawElm $ \_ -> do
      but1 <- fmap (const AddScript) <$> materialButton "+ Step"
      but2 <- fmap (const AddLoop) <$> materialButton "+ Loop"
      but3 <- fmap (const AddImport) <$> materialButton "+ Import"
      widgetListWith (leftmost [but1, but2, but3]) widgetStep

widgetStep :: MonadWidget t m => AddScript -> App t m (Dynamic t WidgetStep)
widgetStep AddScript = fmap WidgetStep <$> runElm (simpleElm singleScriptView) initState
widgetStep AddLoop = do
  txt <- containerList $ view textInput_value <$>
    materialTextInput "variable" "" "variable"
  ams <- runElm (simpleElm singleScriptView) initState
  return $ WidgetLoop <$> txt <*> ams
widgetStep AddImport =
  let
    v = scriptNameView <> scriptVarsView <> scriptEnvView
  in
    fmap WidgetImport <$> runElm (simpleElm v) initState



singleScriptView ::
  MonadWidget t m =>
  View t (App t m) AModel
singleScriptView = mconcat
  [ decorateView (executableView  <> commandView) $
      elClass "div" "algorithm-choose"
  , declarationView
  , descriptionView
  , insertDeclView
  , choicesView
  ]

choicesView ::
  MonadWidget t m =>
  View t (App t m) AModel
choicesView = mconcat
  [ decorateView (changeViewUniq amodel_parameters parameterView) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Parameters"
        elClass "div" "input-container" p

  , decorateView (changeViewUniq amodel_inputs (measurementView (Flexible True))) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Inputs"
        elClass "div" "input-container" p

  , decorateView (changeViewUniq amodel_outputs (measurementView (Flexible True))) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Outputs"
        elClass "div" "input-container" p

  ]

--------------------------------------------------------------------------------

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)

