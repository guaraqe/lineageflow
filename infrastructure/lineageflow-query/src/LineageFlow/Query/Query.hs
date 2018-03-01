{-# LANGUAGE
  FlexibleInstances,
  StandaloneDeriving,
  TypeSynonymInstances
#-}

module LineageFlow.Query.Query
  (
  -- * Query
    QueryWith (Query)
  -- * Particular queries
  , Query
  , PQuery
  , MQuery
  , AQuery
  -- * Lenses for @QueryWith@
  , query_fields
  , query_type
  ) where

import LineageFlow.Base

import LineageFlow.Query.Imports

--------------------------------------------------------------------------------

-- | Queries contain fields and the type of the element being queried. The
-- fields are supposed to correspond to the columns in a table in a database.
data QueryWith a b = Query
  { _query_fields :: Assoc a
  , _query_type :: b
  } deriving (Eq, Show, Generic, Functor, Foldable, Traversable)

$(makeLenses ''QueryWith)

instance (ToJSON a, ToJSON b) => ToJSON (QueryWith a b) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance (FromJSON a, FromJSON b) => FromJSON (QueryWith a b) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

type Query = QueryWith Text

instance Bifunctor QueryWith where
  bimap f g =
    over query_fields (fmap f) .
    over query_type g

--------------------------------------------------------------------------------

-- | Query for parameters
type PQuery = Query PType

-- | Query for measurements
type MQuery = Query MType

-- | Query for algorithms
type AQuery = Query (AType Query)

--------------------------------------------------------------------------------

deriving instance Show a => Show (AType (QueryWith a))
deriving instance Eq a => Eq (AType (QueryWith a))

instance ToJSON (AType Query) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON (AType Query) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
