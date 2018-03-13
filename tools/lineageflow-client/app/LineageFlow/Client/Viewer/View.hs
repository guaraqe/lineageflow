{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Client.Viewer.View
  ( viewerDiv
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Widget.Common
import LineageFlow.Client.Widget.MDL
import LineageFlow.Client.Widget.Dropdown

import LineageFlow.Client.Viewer.Model
import LineageFlow.Client.Selection.Model

import LineageFlow.Client.Selection.View

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.Text as T

import Prelude hiding ((.), id)
import Control.Category

viewerDiv :: MonadWidget t m => View t (App t m) VModel
viewerDiv = monadView $ do
  fields <- fmap removeBasic <$> getFields

  let
    selected =
      changeViewUniq vmodel_selection $
        dynView $
          fmap (foldMap (selectionView Fixed)) fields

  return (selected <> choices <> measView)

cons' = (:)

choices :: MonadWidget t m => View t (App t m) VModel
choices = simpleView $ \vmodel -> do

  let
    selected =
      fmap (Map.toList . view (vmodel_selection . smodel_fixed)) vmodel

    triChoice =
      fmap (Map.lookup 1 . view (vmodel_tri . cmodel_selection)) vmodel

    removeDomain = filter ((/= "subdomain") . fst)

    selectedS = ffor selected $
      cons' ("domain","(time,[cell,time])") .
      cons' ("codomain","scalar")

    selectedV = ffor selected $
      cons' ("domain","(time,[cell,time])") .
      cons' ("codomain","vector")

    selectedT = ffor selected $
      cons' ("domain","(time,[cell,time])") .
      cons' ("codomain","tensor")

    selectedG = ffor selected $
      cons' ("domain","(time,[cell,time])") .
      cons' ("codomain","number")

    selectedTri = ffor selected $
      cons' ("domain","(time,[s2 [cell,time],time])") .
      cons' ("codomain","s2 [cell,time]") .
      cons' ("measurement","identity") .
      removeDomain

    selectedTriScalar Nothing = pure [("wrong","wrong")]
    selectedTriScalar (Just s) = ffor selected $
      cons' ("domain","(time,[s2 [cell,time],time])") .
      cons' ("codomain","scalar") .
      cons' ("subdomain",s) .
      removeDomain

  changes <- changed selected
  changesTri <- changed triChoice

  matchesS <- getMatching (fmap Assoc selectedS) (pure "measurement") changes
  matchesV <- getMatching (fmap Assoc selectedV) (pure "measurement") changes
  matchesT <- getMatching (fmap Assoc selectedT) (pure "measurement") changes
  matchesG <- getMatching (fmap Assoc selectedG) (pure "measurement") changes
  matchesTri <- getMatching (fmap Assoc selectedTri) (pure "subdomain") changes
  matchesTriScalar <- do
     let selTri = selectedTriScalar =<< triChoice
     getMatching (fmap Assoc selTri) (pure "measurement") (changes <> changesTri)

  return $ fmap appEndo $
    fmap (Endo . set (vmodel_scalar . cmodel_choices)) matchesS <>
    fmap (Endo . set (vmodel_vector . cmodel_choices)) matchesV <>
    fmap (Endo . set (vmodel_tensor . cmodel_choices)) matchesT <>
    fmap (Endo . set (vmodel_group  . cmodel_choices)) matchesG <>
    fmap (Endo . set (vmodel_tri . cmodel_choices)) matchesTri <>
    fmap (Endo . set (vmodel_triScalar . cmodel_choices)) matchesTriScalar

bindDynD ::
  MonadWidget t m =>
  (a -> m (Dynamic t b)) -> Dynamic t a -> m (Dynamic t b)
bindDynD f x =
  let
    start = do
      init <- sample (current x)
      f init
  in
    fmap join $ widgetHold start (fmap f (updated x))

measView ::
  MonadWidget t m =>
  View t (App t m) VModel
measView = mconcat
  [ decorateView (changeViewUniq vmodel_scalar (textView "scalar")) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Scalars"
        x
  , decorateView (changeViewUniq vmodel_vector (textView "vector")) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Vectors"
        x

  , decorateView (changeViewUniq vmodel_tensor (textView "tensor")) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Tensors"
        x

  , decorateView (changeViewUniq vmodel_group (textView "selection")) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Selections"
        x

  , decorateView (changeViewUniq vmodel_tri triView) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Triangulation"
        x

  , decorateView (changeViewUniq vmodel_triScalar (textView "scalar")) $ \x -> do
      elClass "div" "viewer-measurement" $ do
        el "h4" $ text "Triangulation Scalars"
        x

  ]


textView ::
  MonadWidget t m =>
  Text -> View t (App t m) CModel
textView name = simpleView $ \cmodel -> do

  matches <- holdUniqDyn $ view cmodel_choices <$> cmodel

  vals <- (delay 0.5 =<<) $
    viewList $ dropdownType Fixed "" name $ matches

  return $
    fmap (over cmodel_selection . decomposeMap) vals

triView ::
  MonadWidget t m =>
  View t (App t m) CModel
triView = simpleView $ \cmodel -> do

  matches <- holdUniqDyn $ view cmodel_choices <$> cmodel

  vals <- delay 0.5 =<<
   dropdownType Fixed "" "triangulation" matches

  return $
    fmap (over cmodel_selection . maySet) vals

maySet :: Maybe Text -> Map Int Text -> Map Int Text
maySet Nothing = Map.delete 1
maySet (Just x) = Map.insert 1 x

decomposeMap :: Map Int (Maybe Text) -> Map Int Text -> Map Int Text
decomposeMap f x = Map.union (Map.mapMaybe id f) x

viewList ::
  forall t m a .
  MonadWidget t m =>
  m (Event t a) -> m (Event t (Map Int a))
viewList widget = mdo
  latestId <- count =<< materialButton "Add"

  let modifyChildren :: Event t (Map Int (Maybe ()))
      modifyChildren = fmap (\n -> n =: Just ()) $ updated latestId

  valuesEv <-
    listHoldWithKey mempty (modifyChildren <> evs) $
      \n _ -> elClass "div" "list-choice" $ do
        wid <- widget
        ev <- fmap (\_ -> n =: Nothing) <$> materialButton "Remove"
        return (wid, ev)

  let values = switchPromptlyDyn $ fmap (mergeMap . fmap fst) valuesEv
      evs = switchPromptlyDyn $ fmap (foldMap snd) valuesEv

  return values

changed :: (MonadWidget t m, Eq a) => Dynamic t a -> m (Event t ())
changed x = fmap (const ()) . updated <$> holdUniqDyn x

removeBasic :: [Text] -> [Text]
removeBasic = filter (/= "measurement")
