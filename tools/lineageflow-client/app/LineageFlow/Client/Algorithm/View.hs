{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Client.Algorithm.View
  ( algorithmView
  , executableView
  , commandView
  , declarationView
  , descriptionView
  , insertDeclView
  ) where

import LineageFlow.Client.Measurement.View
import LineageFlow.Client.Parameter.View
import LineageFlow.Client.Algorithm.Model
import LineageFlow.Client.Parameter.Model
import LineageFlow.Client.Measurement.Model
import LineageFlow.Client.Widget.Dropdown

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

algorithmView ::
  MonadWidget t m =>
  View t (App t m) AModel
algorithmView = mconcat
  [ decorateView (executableView  <> commandView) $
      elClass "div" "algorithm-choose"
  , declarationView
  , descriptionView
  , insertDeclView
  , choicesView
  ]

executableView ::
  MonadWidget t m =>
  View t (App t m) AModel
executableView = simpleView $ \_ -> elClass "div" "algorithm-executables" $ do
  executableList <- getExecutables
  executableChoice <- dropdownType Fixed "" "executable" executableList
  return $ fmap (set amodel_executable) executableChoice

commandView ::
  MonadWidget t m =>
  View t (App t m) AModel
commandView = simpleView $ \model -> elClass "div" "algorithm-commands" $ do
  executable <- fmap (fmap $ fromMaybe "") $ uniqView amodel_executable model
  commandList <- getCommands executable =<< changed executable
   -- may (\c -> getCommands c =<< changed executable) executable

  commands <- holdDyn [] commandList
  commandChoice <-
    dropdownType Fixed "" "command" commands
  return $ fmap (set amodel_command) commandChoice

may ::
  MonadWidget t m =>
  (Dynamic t a -> m (Event t b)) -> Dynamic t (Maybe a) -> m (Event t b)
may f m =
  fmap switchPromptlyDyn $
  widgetHold (pure never) $
  fmap (f . pure) $
  fmapMaybe id (updated m)

declarationView ::
  MonadWidget t m =>
  View t (App t m) AModel
declarationView = simpleView $ \model -> do
  executable <- fmap (fmap $ fromMaybe "") $ uniqView amodel_executable model
  command <- fmap (fmap $ fromMaybe "") $ uniqView amodel_command model
  declaration <- getDeclaration executable command  =<< changed command

  return $ fmap (set amodel_declaration . Just) declaration

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

mayList :: Maybe [a] -> [a]
mayList Nothing = []
mayList (Just l) = l

insertDeclView ::
  MonadWidget t m =>
  View t (App t m) AModel
insertDeclView = simpleView $ \model -> do
  decl <- uniqView amodel_declaration model

  let
    toP = fmap (fmap (\c -> PModel c Nothing))
    toM = fmap (fmap (\m -> MModel m Nothing))

    f g l =
      fmapMaybe id $ updated $ fmap (g . getAssoc . view (decl_type . l)) <$> decl
    h g l =
      fmapMaybe id $ updated $ fmap (g . getAssoc . view (decl_type . l)) <$> decl

    p = h toP atype_parameters
    i = f toM atype_inputs
    o = f toM atype_outputs

  delay 0.5 $ fmap appEndo $
    fmap (Endo . set amodel_parameters) p <>
    fmap (Endo . set amodel_inputs) i <>
    fmap (Endo . set amodel_outputs) o

choicesView ::
  MonadWidget t m =>
  View t (App t m) AModel
choicesView = mconcat
  [ decorateView (changeViewUniq amodel_parameters parameterView) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Parameters"
        elClass "div" "input-container" p

  , decorateView (changeViewUniq amodel_inputs (measurementView Fixed)) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Inputs"
        elClass "div" "input-container" p

  , decorateView (changeViewUniq amodel_outputs (measurementView (Flexible False))) $ \p -> do
      el "hr" $ return ()
      elClass "div" "box" $ do
        el "h3" $ text "Outputs"
        elClass "div" "input-container" p

  ]

descriptionView ::
  MonadWidget t m =>
  View t (App t m) AModel
descriptionView = simpleView $ \model -> do
   decl <- uniqView amodel_declaration model
   let
      name = fmap (fmap (view decl_name)) decl
      description = fmap (fmap (view decl_desc)) decl

   mayVoid (el "h3" . dynText) name
   mayVoid (void . elDynHtmlAttr' "div" ("class" =: "algorithm-description")) description

   return never

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)

mayVoid ::
  MonadWidget t m => (Dynamic t a -> m ()) -> Dynamic t (Maybe a) -> m ()
mayVoid f x =
  void $
  widgetHold (return ()) $
  fmap (f . pure) $
  fmapMaybe id (updated x)
