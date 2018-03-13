{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}


module LineageFlow.Viewer.Interface
  ( VQueryWith (VQuery)
  , VPath
  , VQuery
  , viewer_scalar
  , viewer_vector
  , viewer_tensor
  , viewer_group
  , viewer_tri
  , viewer_triScalar
  ) where

import Control.Lens
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import LineageFlow.Query
import Data.Text (Text)

data VQueryWith a = VQuery
  { _viewer_scalar :: [a]
  , _viewer_vector :: [a]
  , _viewer_tensor :: [a]
  , _viewer_group :: [a]
  , _viewer_tri :: Maybe a
  , _viewer_triScalar :: Maybe [a]
  } deriving (Show, Generic, Eq, Functor, Foldable, Traversable)

$(makeLenses ''VQueryWith)

type VPath = VQueryWith (Text,FilePath)
type VQuery = VQueryWith MQuery

instance FromJSON a => FromJSON (VQueryWith a) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance ToJSON a => ToJSON (VQueryWith a) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }
