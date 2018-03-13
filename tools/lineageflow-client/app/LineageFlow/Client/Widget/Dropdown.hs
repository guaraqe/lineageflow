{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Widget.Dropdown
  ( InputType (..)
  , DropFlex
  , dropflex_text
  , dropflex_type
  , dropdownFlex
  , dropdownType
  ) where

import Reflex
import Reflex.Dom
import Reflex.Elm

import LineageFlow.Client.Widget.MDL
import LineageFlow.Client.Widget.Common
import LineageFlow.Client.Types

import Data.Monoid

import Control.Lens
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Control.Monad

import Data.Bool

--------------------------------------------------------------------------------

data InputType = Fixed | Flexible Bool
  deriving (Show, Eq)

data DropFlex = DropFlex
  { _dropflex_text :: Maybe Text
  , _dropflex_type :: InputType
  } deriving (Show, Eq)

$(makeLenses ''DropFlex)

--------------------------------------------------------------------------------

dropdownFlex ::
  MonadWidget t m =>
  InputType -> Text -> Event t [Text] -> m (Dynamic t (Maybe Text))
dropdownFlex itype name vals = do
  elClass "div" "choice" $ do
    opts <- holdDyn mempty (fmap fromKeys vals)

    val <- case itype of
      Fixed -> view dropdown_value <$> materialDropdown name "" opts (mdlConf name)
      Flexible _ -> flexibleInput "choice" name opts

    return (fmap toMaybe val)

--------------------------------------------------------------------------------

dropdownType ::
  MonadWidget t m =>
  InputType -> Text -> Text -> Dynamic t [Text] -> m (Event t (Maybe Text))
dropdownType inputType initial inputName options =
  fmap (fmap toMaybe) $
    elClass "div" "choice" $
      case inputType of
        Fixed -> dropdownChoose initial inputName (fmap fromKeys options)
        Flexible val -> dropdownEdit val initial inputName (fmap fromKeys options)

dropdownChoose ::
  MonadWidget t m =>
  Text -> Text -> Dynamic t (Map Text Text) -> m (Event t Text)
dropdownChoose initial inputName options = do
  value <-
    view dropdown_value <$>
      materialDropdown inputName initial options (mdlConf inputName)
  updated <$> holdUniqDyn value

dropdownEdit ::
  MonadWidget t m =>
  Bool -> Text -> Text -> Dynamic t (Map Text Text) -> m (Event t Text)
dropdownEdit val initial inputName options = mdo
  let
    drp =
      view dropdown_value <$>
        materialDropdown inputName initial options (mdlConf inputName)
    txt =
      view textInput_value <$>
        materialTextInput inputName initial inputName

  value <- widgetHold (bool drp txt val) $ fmap (bool drp txt) $ view checkbox_change box
  box <- checkbox val def

  updated <$> holdUniqDyn (join value)

toMaybe :: (Monoid a, Eq a)  => a -> Maybe a
toMaybe a = if a == mempty then Nothing else Just a

toUpdate :: Reflex t  => Event t Text -> Event t (Text -> Text)
toUpdate = fmap const . ffilter (/= "")

fromKeys :: Ord a => [a] -> Map a a
fromKeys = Map.fromList . fmap (\x -> (x,x))
