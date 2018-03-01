module LineageFlow.Base.MType
  (
  -- * Measurement type
    MType (MType)
  -- * Lenses for @MType@
  , mtype_domain
  , mtype_codomain
  ) where

import LineageFlow.Base.Imports
import LineageFlow.Base.Utils

-- | Measurement type, containing the identifiers for its domain and codomain.
data MType = MType
  { _mtype_domain :: Text
  , _mtype_codomain :: Text
  } deriving (Eq, Show, Generic)

$(makeLenses ''MType)

instance ToJSON MType where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON MType where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
