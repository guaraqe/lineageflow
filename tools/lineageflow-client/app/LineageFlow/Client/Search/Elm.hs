{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.Search.Elm
  ( search
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Search.Model
import LineageFlow.Client.Global.Model
import LineageFlow.Client.Search.View

search ::
  MonadWidget t m => View t (App t m) Global
search =
  decorateView (changeViewUniq global_search searchView) $
    elClass "div" "search-container"
