module LineageFlow.Script.Var
  ( Var (Var)
  , var_name
  , var_card
  , HasVars (..)
  , HasLoop (..)
  ) where

import LineageFlow.Declaration

import Control.Lens
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data Var = Var
  { _var_name :: Text
  , _var_card :: Card
  } deriving (Show, Eq, Ord, Generic)

$(makeLenses ''Var)

instance ToJSON Var where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON Var where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

--------------------------------------------------------------------------------

class HasVars a where
  getVars :: a -> [Var]

class HasLoop a where
  getLoop :: a -> [Var]
