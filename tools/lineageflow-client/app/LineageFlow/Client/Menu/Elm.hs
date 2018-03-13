{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Client.Menu.Elm
  ( Menu (..)
  , renderMenu
  , contentWrapper
  ) where

import LineageFlow.Client.Prelude hiding ((<>))

import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Semigroup

data Menu =
 HomePage |
 AlgorithmRunner |
 MeasureExplorer |
 Viewer |
 Running |
 ScriptRun |
 ScriptCreate
 deriving (Eq, Enum, Bounded)

instance Show Menu where
  show HomePage = "Home"
  show AlgorithmRunner = "Algorithm Runner"
  show MeasureExplorer = "Measurement Explorer"
  show Viewer = "3D Viewer"
  show Running = "Processes"
  show ScriptRun = "Script Runner"
  show ScriptCreate = "Script Creation"

menuLink :: MonadWidget t m => Menu -> m (Event t Menu)
menuLink menu = do
  l <- linkClass (Text.pack (show menu)) "mdl-navigation__link mdl-color-text--grey-400"
  return $ fmap (const menu) (_link_clicked l)

instance Semigroup Menu where
  a <> _ = a

foldMapM :: (Traversable t, Applicative m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = fmap (foldMap id) . traverse f
{-# INLINE foldMapM #-}

renderMenu :: MonadWidget t m =>  m (Dynamic t Menu)
renderMenu =
  elClass "div" "mdl-layout__drawer mdl-color--indigo-900 mdl-color-text--indigo-50" $ do
    elClass "header" "my-title" $
      elClass "h1" "mdl-layout-title" $ text "LineageFlow"
    elClass "nav" "mdl-navigation mdl-color--indigo-800" $ do

      holdDyn HomePage =<< foldMapM menuLink [minBound .. maxBound]

contentWrapper :: MonadWidget t m => Menu -> Dynamic t Menu -> m a -> m a
contentWrapper this c = elDynAttr "div" (tabStyle this c)

tabStyle :: Reflex t => Menu -> Dynamic t Menu -> Dynamic t (Map Text Text)
tabStyle this = fmap $ \c ->
  if c == this
    then Map.singleton "style" "display:block"
    else Map.singleton "style" "display:none"
