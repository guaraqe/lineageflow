{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module LineageFlow.Viewer.Interface
  ( VQuery (VQuery)
  , viewer_scalar
  , viewer_vector
  , viewer_tensor
  , viewer_group
  , viewer_tri
  , viewer_triScalar
  ) where

import Control.Lens
import Data.Aeson
import Data.Text
import GHC.Generics
import LineageFlow.Query
import Data.Text (Text)

data VQuery = VQuery
  { _viewer_scalar :: Assoc FilePath
  , _viewer_vector :: Assoc FilePath
  , _viewer_tensor :: Assoc FilePath
  , _viewer_group :: Assoc FilePath
  , _viewer_tri :: Maybe (Text,FilePath)
  , _viewer_triScalar :: Maybe (Assoc FilePath)
  } deriving (Show, Generic, Eq)

$(makeLenses ''VQuery)

instance FromJSON VQuery
instance ToJSON VQuery
