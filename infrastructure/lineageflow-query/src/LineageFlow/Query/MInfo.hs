module LineageFlow.Query.MInfo
  (
  -- * Information about measurements in a database
    MInfo (MInfo)
  , minfo_mquery
  , minfo_aquery
  , minfo_hash
  , minfo_extra
  ) where

import LineageFlow.Query.Imports
import LineageFlow.Query.Query

import LineageFlow.Base

-- | Information about measurements in a database. It contains its
-- corresponding query, the algorithm query that generated it, that hash of the
-- file containing it and extra fields.
data MInfo = MInfo
  { _minfo_mquery :: MQuery
  , _minfo_aquery :: AQuery
  , _minfo_hash :: Text
  , _minfo_extra :: Value
  } deriving (Show, Eq, Generic)

$(makeLenses ''MInfo)

instance ToJSON MInfo where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON MInfo where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
