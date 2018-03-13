{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Client.ScriptSave.Model
  ( ScriptSaveModel (ScriptSaveModel)
  , scriptSave_name
  , scriptSave_algs
  , WidgetStep (WidgetStep, WidgetLoop, WidgetImport)
  , _WidgetStep
  , _WidgetLoop
  , _WidgetImport
  , makeScript
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Algorithm.Model

import LineageFlow.Client.ScriptRun.Model

import qualified Data.Map.Strict as Map
import Data.Map.Strict as Map

import Control.Lens

data WidgetStep =
  WidgetStep AModel |
  WidgetLoop [Text] AModel |
  WidgetImport ScriptRunModel
  deriving (Show, Eq)

$(makePrisms ''WidgetStep)

data ScriptSaveModel = ScriptSaveModel
  { _scriptSave_name :: Text
  , _scriptSave_algs :: [WidgetStep]
  } deriving (Eq, Show)

$(makeLenses ''ScriptSaveModel)

instance InitState ScriptSaveModel where
  initState = ScriptSaveModel "" []

toTemplateStep :: WidgetStep -> Maybe TemplateStep
toTemplateStep (WidgetStep m) = Step <$> getAQuery m
toTemplateStep (WidgetLoop t m) = Loop t <$> getAQuery m
toTemplateStep (WidgetImport (ScriptRunModel txt _ env)) = flip Import env <$> txt

makeScript :: ScriptSaveModel -> Maybe ScriptTemplate
makeScript (ScriptSaveModel _ l) = traverse toTemplateStep l
