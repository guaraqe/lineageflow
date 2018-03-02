{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Database.SQLite.Types
  (
    Row (..)
  ) where

import LineageFlow.Database

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

--------------------------------------------------------------------------------

data Row = Row
  { _row_species :: Text
  , _row_specimen :: Text
  , _row_tracking :: Text
  , _row_domain :: Text
  , _row_subdomain :: Text
  , _row_codomain :: Text
  , _row_measurement :: Text
  , _row_hash :: Text
  , _row_aquery :: AQuery
  , _row_extra :: Value
  } deriving (Eq, Show, Generic)
