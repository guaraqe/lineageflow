{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.Selection.View
  ( selectionView
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Selection.Model

import LineageFlow.Client.Widget.Dropdown

import qualified Data.Map.Strict as Map

--------------------------------------------------------------------------------

selectionView :: MonadWidget t m => InputType -> Text -> View t (App t m) SModel
selectionView inputType name =
  makeView selectionUpdate (selectionAction inputType name)

data Action =
  SetFixed Text (Maybe Text) |
  SetFlexible Text (Maybe Text)

selectionUpdate :: Update SModel Action
selectionUpdate = makeUpdate $ \case
  SetFixed _ Nothing -> id
  SetFixed name (Just v) ->
    over smodel_fixed (Map.insert name v) .
    over smodel_flexible (Map.delete name)
  SetFlexible _ Nothing -> id
  SetFlexible name (Just v) ->
    over smodel_flexible (Map.insert name v) .
    over smodel_fixed (Map.delete name)

selectionAction ::
  MonadWidget t m =>
  InputType ->
  Text ->
  Dynamic t SModel ->
  App t m (Event t Action)
selectionAction inputType name model =
  let
    selected = fmap (Assoc . Map.toList . view smodel_fixed) model
    action = case inputType of
      Fixed -> SetFixed name
      Flexible _ -> SetFlexible name
  in
    fmap (fmap action) $ do

      changes <- changed selected
      e <- getPostBuild

      matches <- holdDyn mempty =<<
        getMatching (fmap (removeThis name) selected) (pure name) (e <> changes)

      cur <- fmap (getSelected inputType name) <$> holdUniqDyn model

      --bindE (\(t,c) -> dropdownType t c name matches) cur
      dropdownType inputType "" name matches

bindE ::
  MonadWidget t m =>
  (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
bindE f x =
  join $
  fmap (switchPromptly never) $
  dyn $
  fmap f x

removeThis :: Text -> Assoc Text -> Assoc Text
removeThis name = Assoc .  filter (\(_,x) -> x /= name) . getAssoc

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

getSelected :: InputType -> Text -> SModel -> (InputType, Text)
getSelected Fixed name (SModel m1 _) =
  (Fixed, fromMaybe "" (Map.lookup name m1))
getSelected itype name (SModel m1 m2) =
  case Map.lookup name m1 of
    Just a -> (Flexible False, a)
    Nothing ->
      case Map.lookup name m2 of
        Just a -> (Flexible True, a)
        Nothing -> (itype,"")
