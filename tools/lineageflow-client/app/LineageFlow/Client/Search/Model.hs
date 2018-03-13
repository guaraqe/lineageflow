{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Search.Model
  ( SearchModel (SearchModel)
  , search_selection
  , search_result
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Selection.Model

data SearchModel = SearchModel
  { _search_selection :: SModel
  , _search_result :: [MInfo]
  } deriving (Eq, Show)

$(makeLenses ''SearchModel)

instance InitState SearchModel where
 initState = SearchModel initState mempty

