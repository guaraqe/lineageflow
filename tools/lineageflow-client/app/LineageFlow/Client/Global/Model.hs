{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module LineageFlow.Client.Global.Model
  ( Global (Global)
  , global_algorithm
  , global_algorithmModel
  , global_algorithmRunning
  , global_viewer
  , global_viewerModel
  , global_viewerRunning
  , global_search
  , global_scriptSave
  , global_scriptRun
  ) where

import LineageFlow.Client.Prelude hiding (Global)
import LineageFlow.Client.Types
import LineageFlow.Client.Viewer.Model
import LineageFlow.Client.Algorithm.Model
import LineageFlow.Client.Search.Model
import LineageFlow.Client.ScriptRun.Model
import LineageFlow.Client.ScriptSave.Model
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty)

data Global = Global
  { _global_algorithm :: Maybe AQuery
  , _global_algorithmModel :: AModel
  , _global_algorithmRunning :: Map Int (AQuery, Status)
  , _global_viewer :: Maybe VQuery
  , _global_viewerModel :: VModel
  , _global_viewerRunning :: Map Int (VQuery, Status)
  , _global_search :: SearchModel
  , _global_scriptSave :: ScriptSaveModel
  , _global_scriptRun :: ScriptRunModel
  } deriving (Eq, Show)

$(makeLenses ''Global)

instance InitState Global where
  initState =
    Global
      Nothing
      initState
      Map.empty
      Nothing
      initState
      Map.empty
      initState
      initState
      initState
