{-# LANGUAGE FlexibleInstances #-}

module LineageFlow.Script.Step
  ( Step (..)
  , _Step
  , _Loop
  , _Import
  , ScriptStep
  , TemplateStep
  , scriptStepVars
  , scriptStepLoop
  , Script
  , ScriptTemplate
  , fromScriptTemplate
  ) where

import LineageFlow.Declaration

import LineageFlow.Script.Env
import LineageFlow.Script.Var
import LineageFlow.Script.Part
import LineageFlow.Script.Entry

import Data.Maybe

import Control.Lens
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Step a b = Step (AQueryWith a) | Loop [b] (AQueryWith a) | Import Text (EnvWith a)
  deriving (Show, Eq, Generic, Functor)

$(makePrisms ''Step)


instance Bifunctor Step where
  bimap f _ (Step a) = Step (mapAQueryWith f a)
  bimap f g (Loop l a) = Loop (fmap g l) (mapAQueryWith f a)
  bimap f _ (Import c e) = Import c (fmap f e)

type ScriptStep = Step Entry Var
type TemplateStep = Step Text Text

instance FromJSON ScriptStep where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

instance FromJSON TemplateStep where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = convertConstructor }


instance ToJSON ScriptStep where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

instance ToJSON TemplateStep where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = convertConstructor }

scriptStepVars :: ScriptStep -> [Var]
scriptStepVars (Step s) = scriptPartVars s
scriptStepVars (Loop _ s) = scriptPartVars s
scriptStepVars (Import _ e) = getVars e

scriptStepLoop :: ScriptStep -> [Var]
scriptStepLoop (Loop v _) = v
scriptStepLoop (Import _ e) = getLoop e
scriptStepLoop _ = []

instance HasVars (Step Entry Var) where
  getVars = scriptStepVars

instance HasLoop (Step Entry Var) where
  getLoop = scriptStepLoop

--------------------------------------------------------------------------------

type Script = [ScriptStep]

type ScriptTemplate = [TemplateStep]

fromTemplateStep :: TemplateStep -> ScriptStep
fromTemplateStep (Step x) = Step (toScriptPart x)
fromTemplateStep (Loop name x) = Loop (fmap (vars y) name) y
  where
    y = toScriptPart x
    vars script nam = Var nam $ fromJust $
      lookupAssoc nam $ Assoc $ fmap (\(Var n c) -> (n,c)) $ scriptPartVars script

fromTemplateStep (Import c (Env p v l)) =
  Import c $ Env
    (readEntry Single p)
    (fmap (withCard readEntry) v)
    (fmap (fmap (withCard readEntry)) l)

fromScriptTemplate :: ScriptTemplate -> Script
fromScriptTemplate = fmap fromTemplateStep
