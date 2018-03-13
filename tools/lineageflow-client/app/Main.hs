{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import LineageFlow.Client.Menu.Elm
import LineageFlow.Client.Algorithm.Elm
import LineageFlow.Client.Viewer.Elm
import LineageFlow.Client.Running.Elm
import LineageFlow.Client.Search.Elm

import LineageFlow.Client.ScriptSave.Elm
import LineageFlow.Client.ScriptRun.Elm

import LineageFlow.Client.Types
import LineageFlow.Client.Global.Model

import LineageFlow.Client.Widget.Common

import LineageFlow.Client.Prelude

import Data.FileEmbed
import Data.ByteString as ByteString
import qualified Data.Text as Text

main :: IO ()
main =
  --run 3911 $
  mainWidgetWithCss css (runApp appMain)

appMain :: MonadWidget t m => App t m ()
appMain =
  elClass "div" "mdl-layout__container" $
  elClass "div" "mdl-layout mdl-js-layout mdl-layout--fixed-drawer mdl-layout--fixed-header" $ mdo

  elClass "header" "mdl-layout__header mdl-color--grey-100 mdl-color-text--grey-600" $ do

    elClass "div" "mdl-layout__header-row" $ do
      elClass "div" "ugly-hack" $ return ()
      elClass "span" "mdl-layout-title" $ dynText (fmap (Text.pack . show) currentTab)
      elClass "div" "mdl-layout-spacer" $ return ()

  currentTab <- renderMenu

  elClass "div" "main-container mdl-layout__content mdl-color--grey-100"$ void $ do
    flip runElm initState $ simpleFinalElm $ mconcat

      [ decorateView home $
          contentWrapper HomePage currentTab

      , decorateView algorithm $
          contentWrapper AlgorithmRunner currentTab

      , decorateView viewer $
          contentWrapper Viewer currentTab

      , decorateView running $
          contentWrapper Running currentTab

      , decorateView search $
          contentWrapper MeasureExplorer currentTab

      , decorateView scriptRun $
          contentWrapper ScriptRun currentTab

      , decorateView scriptSave $
          contentWrapper ScriptCreate currentTab
      ]

  return ()

home :: MonadWidget t m => View t (App t m) Global
home = simpleView $ \_ -> do
  _ <-
    elDynHtmlAttr' "div" ("class" =: "info") $
      pure $(embedStringFile "desc/home.md")
  return never

css :: ByteString
css =
  $(embedFile "css/test.css") <>
  $(embedFile "css/material.indigo-pink.min.css") <>
  $(embedFile "css/material.min.css") <>
  $(embedFile "css/mdl-selectfield.min.css")

