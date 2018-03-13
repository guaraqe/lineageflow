{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Client.Parameter.View
  ( parameterContainer
  , parameterView
  ) where

import Reflex
import Reflex.Dom

import LineageFlow.Client.Types
import LineageFlow.Client.Prelude

import LineageFlow.Client.Widget.MDL
import LineageFlow.Client.Widget.Common

import LineageFlow.Client.Parameter.Model

import Control.Lens

parameterView ::
  MonadWidget t m =>
  View t (App t m) [(Text, PModel)]
parameterView =
  elmToView
    (fmap $ view pmodel_declaration . view _2)
    (rawElm (fmap sequence . traverse parameterContainer))
    (makeUpdate (zipWith (set (_2 . pmodel_query))))

parameterContainer ::
  MonadWidget t m =>
  CardF PDecl -> m (Dynamic t ((Maybe (CardF PQuery))))
parameterContainer parMeta =
  fmap (fmap sequence) $ someContainer
    parameterDiv
    (view decl_name)
    (view decl_desc)
    parMeta

parameterDiv :: MonadWidget t m => PDecl -> m (Dynamic t (Maybe PQuery))
parameterDiv parMeta =
  elClass "div" "parameter" $ do

    parInput <- materialTextInput "parameter" "" (view decl_name parMeta)

    let pType = parMeta ^. decl_type

    return $
      fmap (\v -> Query (Assoc [("value",v)]) pType) . toMaybe <$>
        view textInput_value parInput
  where
    toMaybe a = if a == mempty then Nothing else Just a
