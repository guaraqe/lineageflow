module LineageFlow.Client.ScriptSave.Elm
  ( scriptSave
  ) where

import LineageFlow.Client.Types
import LineageFlow.Client.Prelude
import LineageFlow.Client.ScriptSave.View

import LineageFlow.Client.Global.Model

scriptSave :: MonadWidget t m => View t (App t m) Global
scriptSave = changeViewUniq global_scriptSave scriptSaveView
