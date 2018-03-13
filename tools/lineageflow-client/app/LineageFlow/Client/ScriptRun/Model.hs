{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.ScriptRun.Model
  ( ScriptRunModel (ScriptRunModel)
  , scriptRun_choice
  , scriptRun_vars
  , scriptRun_env
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Algorithm.Model

import Control.Lens

data ScriptRunModel = ScriptRunModel
  { _scriptRun_choice :: Maybe Text
  , _scriptRun_vars :: ScriptInput
  , _scriptRun_env :: Env
  } deriving (Eq, Show)

$(makeLenses ''ScriptRunModel)

instance InitState ScriptRunModel where
  initState = ScriptRunModel Nothing (ScriptInput [] []) (Env "" (Assoc []) (Assoc []))
