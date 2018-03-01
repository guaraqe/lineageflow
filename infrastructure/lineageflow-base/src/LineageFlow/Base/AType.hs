{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module LineageFlow.Base.AType
  (
  -- * Algorithm type
    AType (AType)
  -- * Lenses for @AType@
  , atype_parameters
  , atype_inputs
  , atype_outputs
  -- * APath
  , APath
  ) where

import LineageFlow.Base.Imports
import LineageFlow.Base.Utils

import LineageFlow.Base.MType
import LineageFlow.Base.PType
import LineageFlow.Base.Card

import qualified Rank2

-- | Algorithm type, containing its parameters, inputs and outputs. Each of
-- these elements have both a name and a cardinality. It is polymorphic on @f@
-- in order to allow its use both for definitions and queries.
data AType f = AType
  { _atype_parameters :: Assoc (CardF (f PType))
  , _atype_inputs :: Assoc (CardF (f MType))
  , _atype_outputs :: Assoc (CardF (f MType))
  } deriving Generic

$(makeLenses ''AType)

--------------------------------------------------------------------------------

instance Rank2.Functor AType where
  f <$> (AType p i o) =
    AType (fmap (fmap f) p) (fmap (fmap f) i) (fmap (fmap f) o)

--------------------------------------------------------------------------------

type APath = AType (Const String)

instance ToJSON APath where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON APath where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }
