module LineageFlow.Base.PType
  (
  -- * Parameter type
    PType (PType)
  -- * Lenses for @PType@
  , ptype
  ) where

import LineageFlow.Base.Imports
import LineageFlow.Base.Utils

-- | Parameter type, containing the type of its argument.
data PType = PType
  { _ptype :: Text }
  deriving (Show, Eq, Generic)

$(makeLenses ''PType)

instance ToJSON PType where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON PType where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
