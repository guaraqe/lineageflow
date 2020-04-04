module LineageFlow.Script.Input
  ( ScriptInput (..)
  , scriptInput_vars
  , scriptInput_loop
  , scriptInput
  ) where

import LineageFlow.Declaration

import LineageFlow.Script.Var
import LineageFlow.Script.Step

import qualified Data.Set as Set

import Control.Lens
import Data.Aeson.Types
import Data.Semigroup
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data ScriptInput = ScriptInput
  { _scriptInput_vars :: [Var]
  , _scriptInput_loop :: [Var]
  } deriving (Show, Eq, Generic)

$(makeLenses ''ScriptInput)

instance ToJSON ScriptInput where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON ScriptInput where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance Semigroup ScriptInput where
  ScriptInput v1 l1 <> ScriptInput v2 l2 =
    ScriptInput (v1 <> v2) (l1 <> l2)

instance Monoid ScriptInput where
  mempty = ScriptInput [] []
  mappend = (<>)

scriptStepInput :: ScriptStep -> ScriptInput
scriptStepInput s = ScriptInput (scriptStepVars s) (scriptStepLoop s)

scriptInput :: Script -> ScriptInput
scriptInput = normalizeInput . foldMap scriptStepInput

normalizeInput :: ScriptInput -> ScriptInput
normalizeInput (ScriptInput vars loop) =
  let
    varsSet = Set.fromList vars
    loopSet = Set.fromList loop
  in
    ScriptInput
      (Set.toList (Set.difference varsSet loopSet))
      (Set.toList loopSet)


