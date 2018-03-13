{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Client.Widget.Common
  ( someContainer
  , someContainerDyn
  , containerList
  , containerMaybe
  , widgetListWith
  , flexibleInput
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Widget.MDL

import Data.Text (Text)


import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

flexibleInput ::
  MonadWidget t m =>
  Text -> Text -> Dynamic t (Map Text Text) -> m (Dynamic t Text)
flexibleInput tinp name opts =
  let
    dropd = view dropdown_value <$> materialDropdown name "" opts (mdlConf name)
    txt = view textInput_value <$> materialTextInput tinp "" name
  in mdo

  let
    choice = flip fmap (view checkbox_change checkBox) $
      \case
        False -> dropd
        True -> txt
  coisa <- widgetHold dropd choice
  checkBox <- checkbox False def
  return (join coisa)

someContainer ::
  forall t m a b .
  MonadWidget t m =>
  (b ->  m (Dynamic t a)) ->
  (b -> Text) ->
  (b -> Text) ->
  CardF b ->
  m (Dynamic t (CardF a))
someContainer container name desc = \case
  SingleF arg -> fmap SingleF <$> do
    el "h4" $ text (name arg)
    text (desc arg)
    container arg
  ManyF (arg:_) -> fmap ManyF <$> do
    el "h4" $ text (name arg)
    text (desc arg)
    addNew <- el "div" $ materialButton "Add"
    divList addNew (container arg)
  OptionalF (Just arg) -> fmap OptionalF <$> do
    el "h4" $ text (name arg)
    text (desc arg)
    containerMaybe (container arg)

someContainerDyn ::
  forall t m a b .
  MonadWidget t m =>
  (Dynamic t b ->  m (Dynamic t a)) ->
  (b -> Text) ->
  (b -> Text) ->
  Dynamic t (CardF b) ->
  m (Dynamic t (CardF a))
someContainerDyn container name desc x = do
  let arg = fmap extractCardF x
  el "h4" $ dynText $ fmap name arg
  dynText $ fmap desc arg
  liftCardF container x

extractCardF :: CardF a -> a
extractCardF (SingleF a) = a
extractCardF (ManyF (a:_)) = a
extractCardF (OptionalF (Just a)) = a

singleF :: CardF a -> Maybe a
singleF (SingleF a) = Just a
singleF _ = Nothing

manyF :: CardF a -> Maybe a
manyF (ManyF (a:_)) = Just a
manyF _ = Nothing

optionalF :: CardF a -> Maybe a
optionalF (OptionalF (Just a)) = Just a
optionalF _ = Nothing

applyCardF ::
  (Reflex t, Applicative m) =>
  (Dynamic t a -> m (Dynamic t b)) -> CardF a -> m (Dynamic t (CardF b))
applyCardF f (SingleF a) = SingleF <$>: f (pure a)
applyCardF f (ManyF a) = ManyF <$>: (fmap sequence $ traverse (f . pure) a)
applyCardF f (OptionalF a) = OptionalF <$>: (fmap sequence $ traverse (f . pure) a)

liftCardF ::
  MonadWidget t m =>
  (Dynamic t a -> m (Dynamic t b)) ->
  (Dynamic t (CardF a) -> m (Dynamic t (CardF b)))
liftCardF f card = do
  x <- dyn $ fmap (applyCardF f) card
  fmap join $ holdDyn (constDyn (OptionalF Nothing)) x

containerList ::
  MonadWidget t m =>
  m (Dynamic t a) ->
  m (Dynamic t [a])
containerList container = do
  addNew <- el "div" $ materialButton "Add"
  divList addNew container

containerMaybe ::
  MonadWidget t m =>
  m (Dynamic t a) -> m (Dynamic t (Maybe a))
containerMaybe widget = do
  val <-
    el "div" $ materialButton "Toggle" >>=
    toggle False

  let
    f = \case
      True -> fmap Just <$> widget
      False -> pure (pure Nothing)

  bindDyn f Nothing val

divList ::
  forall t m a .
  MonadWidget t m =>
  Event t () -> m (Dynamic t a) -> m (Dynamic t [a])
divList addNew widget = mdo

  latestId <- count addNew

  let modifyChildren :: Event t (Map Int (Maybe ()))
      modifyChildren = fmap (\n -> n =: Just ()) $ updated latestId

  valuesEv <- elAttr "div" ("style" =: "display:table;")  $ listHoldWithKey mempty (modifyChildren <> evs) $ \n _ -> do
    elAttr "div" ("style" =: "display:table-row;") $ do
      wid <- elAttr "div" ("style" =: "display:table-cell;") widget
      ev <- fmap (\_ -> n =: Nothing) <$> (elAttr "div" ("style" =: "display:table-cell; padding-left: 5px;") $ materialButton "Remove")
      return (wid, ev)

  let values = fmap Map.elems . joinDynThroughMap $ fmap (fmap fst) valuesEv
      evs = switchPromptlyDyn $ fmap (foldMap snd) valuesEv

  return values

widgetListWith ::
  forall t m a b.
  MonadWidget t m =>
  Event t a -> (a -> m (Dynamic t b)) -> m (Dynamic t [b])
widgetListWith addNew widget = mdo

  latestId <- count addNew

  let modifyChildren :: Event t (Map Int (Maybe a))
      modifyChildren = fmap (\(n,val) -> n =: Just val) $ attachPromptlyDyn latestId addNew

  valuesEv <- elAttr "div" ("style" =: "display:table;")  $ listHoldWithKey mempty (modifyChildren <> evs) $ \n a -> do
    elAttr "div" ("style" =: "display:table-row;") $ do
      wid <- elAttr "div" ("style" =: "display:table-cell;") (widget a)
      ev <- fmap (\_ -> n =: Nothing) <$> (elAttr "div" ("style" =: "display:table-cell; padding-left: 5px;") $ materialButton "Remove")
      return (wid, ev)

  let values = fmap Map.elems . joinDynThroughMap $ fmap (fmap fst) valuesEv
      evs = switchPromptlyDyn $ fmap (foldMap snd) valuesEv

  return values

{-

  fmap switchPromptlyDyn $
    widgetHold (pure never) $
      fmap widgetRemove (updated latestId)

  where
    widgetRemove :: Int -> m (Event t (Map Int a -> Map Int a))
    widgetRemove n = do
      wid <- widget
      but <- materialButton "Remove"

      return $ fmap appEndo $
        fmap (Endo . flip Map.adjust n) wid <>
        fmap (Endo . const (Map.delete n)) but


  let modifyChildren :: Event t (Map Int (Maybe ()))
      modifyChildren = fmap (\n -> n =: Just ()) $ updated latestId

  valuesEv <-
    listHoldWithKey mempty (modifyChildren <> evs) $ \n _ -> do
      wid <- widget
      ev <- fmap (\_ -> n =: Nothing) <$> materialButton "Remove"
      return (wid, ev)

  let values = fmap Map.elems . joinDynThroughMap $ fmap (fmap fst) valuesEv
      evs = switchPromptlyDyn $ fmap (foldMap snd) valuesEv

  return (updated values)
  -}
