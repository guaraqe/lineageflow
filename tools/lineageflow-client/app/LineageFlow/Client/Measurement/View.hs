{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Client.Measurement.View
  ( measurementView
  ) where

import LineageFlow.Client.Widget.Common
import LineageFlow.Client.Widget.Dropdown
import LineageFlow.Client.Types

import LineageFlow.Client.Prelude
import LineageFlow.Client.Measurement.Model
import LineageFlow.Client.Selection.Model
import LineageFlow.Client.Selection.View

import qualified Data.Map.Strict as Map

measurementView ::
  MonadWidget t m =>
  InputType -> View t (App t m) [(Text,MModel)]
measurementView itype =
  elmToView
    (fmap $ view mmodel_declaration . view _2)
    (rawElm (fmap sequence . traverse (measurementContainer itype)))
    (makeUpdate (zipWith (set (_2 . mmodel_query))))

measurementContainer ::
  MonadWidget t m =>
  InputType ->
  CardF MDecl ->
  App t m (Dynamic t (Maybe (CardF MQuery)))
measurementContainer mtype someInputDiv =
  fmap (fmap sequence) $
    someContainer
      (runMeasurement mtype)
      (view decl_name)
      (view decl_desc)
      someInputDiv

runMeasurement ::
  MonadWidget t m =>
  InputType -> MDecl -> App t m (Dynamic t (Maybe MQuery))
runMeasurement itype decl = do
  fields <- getFields
  model <- run itype decl
  return $ liftA3 runSModel (pure $ view decl_type decl) fields model


run :: MonadWidget t m => InputType -> MDecl -> App t m (Dynamic t SModel)
run itype decl =
  let
    assoc = Map.fromList
      [ ("domain",   decl ^. decl_type . mtype_domain)
      , ("codomain", decl ^. decl_type . mtype_codomain)
      ]
  in
    elClass "div" "measure" $
      runElm (simpleElm (update itype)) $ SModel assoc Map.empty

update ::
  forall t m .
  MonadWidget t m =>
  InputType ->
  View t (App t m) SModel
update itype = monadView $ do
  fields <- getFields
  return $
    dynView $
      fmap (foldMap (selectionView itype)) fields
