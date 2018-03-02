{-# LANGUAGE RankNTypes #-}

module LineageFlow.Database
  (
  -- * Database
    Database (..)
  , module Export
  ) where

import LineageFlow.Query as Export
import Data.Text (Text)

-- | Interface to databases. Each database implementation is just a particular
-- record with this type.
data Database = Database
  { -- | Get a measurement from the database
    db_get :: MQuery -> IO FilePath
    -- | Put a measurement in the database
  , db_put :: AQuery -> MQuery -> FilePath -> IO ()
    -- | Delete a measurement from the database
  , db_delete :: MQuery -> IO ()
    -- | Get the fields (columns) defining the database
  , db_fields :: IO [Text]
    -- | Search for fields
  , db_search :: Assoc Text -> IO [MInfo]
    -- | Search for possible values of a field given the value of others
  , db_match :: Assoc Text -> Text -> IO [Text]
  }
