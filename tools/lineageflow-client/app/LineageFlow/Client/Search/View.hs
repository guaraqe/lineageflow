{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.Search.View
  ( searchView
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types

import LineageFlow.Client.Selection.View
import LineageFlow.Client.Selection.Model
import LineageFlow.Client.Search.Model

import LineageFlow.Client.Widget.MDL
import LineageFlow.Client.Widget.Dropdown

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Char (toUpper)

searchView ::
  MonadWidget t m =>
  View t (App t m) SearchModel
searchView = mconcat
  [ decorateView (changeViewUniq search_selection update) $
      elClass "div" "search-selection"
  , makeSearch
  , displaySearch
  ]

update ::
  forall t m .
  MonadWidget t m =>
  View t (App t m) SModel
update = monadView $ do
  fields <- fmap (\x -> "domain" : "codomain" : x) <$> getFields
  return $
    dynView $
      fmap (foldMap (selectionView Fixed)) fields

makeSearch ::
  MonadWidget t m =>
  View t (App t m) SearchModel
makeSearch = simpleView $ \model -> do
  sel <- uniqView search_selection model

  but <- materialButton "Search"

  res <- getSearch (fmap searchQuery sel) but

  return $ fmap (set search_result) res

searchQuery :: SModel -> Assoc Text
searchQuery (SModel m1 m2) = Assoc $ Map.toList (m1 <> m2)


displaySearch ::
  MonadWidget t m =>
  View t (App t m) SearchModel
displaySearch = simpleView $ \model -> do
  runn <- uniqView search_result model

  fields <- getFields

  el "hr" blank

  el "h4" (text "Results")

  elClass "table" "mdl-data-table mdl-js-data-table mdl-data-table--selectable mdl-shadow--2dp" $ do
    el "thead" $ showTopRowDyn fields
    el "tbody" $ traverseDyn_ (showResultDyn fields) runn

  return never

showTopRowDyn ::
  MonadWidget t m =>
  Dynamic t [Text] -> m ()
showTopRowDyn = void . dyn . fmap showTopRow

showTopRow ::
  MonadWidget t m =>
  [Text] -> m ()
showTopRow v =
  el "tr" $ do
    tdMDL "th" $ text "Domain"
    tdMDL "th" $ text "Codomain"
    mapM_ (tdMDL "th" . text . capitalize) v

capitalize :: Text -> Text
capitalize t =
  case Text.uncons t of
    Nothing -> t
    Just (a,b) -> Text.cons (toUpper a) b

showResultDyn ::
  MonadWidget t m =>
  Dynamic t [Text] -> Dynamic t MInfo -> m ()
showResultDyn f v = void . dyn $ liftA2 showResult f v

showResult ::
  MonadWidget t m =>
  [Text] -> MInfo -> m ()
showResult fields vals =
  let
    elems = fmapMaybe (flip lookupAssoc (vals ^. minfo_mquery . query_fields)) fields
    dom = vals ^. minfo_mquery . query_type . mtype_domain
    cod = vals ^. minfo_mquery . query_type . mtype_codomain
  in do
    el "tr" $ do
      tdMDL "td" $ text dom
      tdMDL "td" $ text cod
      mapM_ (tdMDL "td" . text) elems

tdMDL :: MonadWidget t m => Text -> m a -> m a
tdMDL x = elClass x "mdl-data-table__cell--non-numeric"

uniqView ::
  (Eq a, MonadWidget t m) =>
  Lens' b a -> Dynamic t b -> m (Dynamic t a)
uniqView l = holdUniqDyn . fmap (view l)

traverseDyn_ ::
  MonadWidget t m =>
  (Dynamic t a -> m ()) -> Dynamic t [a] -> m ()
traverseDyn_ f x = do
  void $ dyn $ fmap (mapM_ (f . pure)) x


