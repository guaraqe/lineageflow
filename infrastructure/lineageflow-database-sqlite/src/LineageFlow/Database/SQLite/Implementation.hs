{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Database.SQLite.Implementation
  ( sqliteDatabase
  ) where

import LineageFlow.Database

import LineageFlow.Database.SQLite.Types
import LineageFlow.Database.SQLite.SQL

import Data.Monoid
import Data.Maybe
import System.FilePath
import qualified Data.Text as Text

import Data.Aeson (Value (..))

import Database.Selda
import Database.Selda.SQLite

import qualified Data.ByteString.Lazy as ByteStringLazy

import System.Directory

import Crypto.Hash

--------------------------------------------------------------------------------

sqliteDatabase :: FilePath -> Database
sqliteDatabase path =
  Database
    (getMeasurementIn path)
    (putMeasurementIn path)
    (deleteMeasurementIn path)
    (return dbFields)
    (search path)
    (match path)

--------------------------------------------------------------------------------

prepareDatabase :: FilePath ->  IO ()
prepareDatabase path = do
  let
    sqlPath = path </> "lf-database" <.> "sqlite"
    storePath = path </> "lf-store"

  createDirectoryIfMissing True storePath
  withSQLite sqlPath (tryCreateTable measurements)

--------------------------------------------------------------------------------

dbFields :: [Text]
dbFields = ["species","specimen","tracking","subdomain","measurement"]

--------------------------------------------------------------------------------

getMeasurementIn :: FilePath -> MQuery -> IO FilePath
getMeasurementIn path q = do
  let
    sqlPath = path </> "lf-database" <.> "sqlite"
    storePath = path </> "lf-store"

  prepareDatabase path

  r <- withSQLite sqlPath (matchRows (toAssoc q) "hash")

  case r of
    [] -> error $ "Measurement not present in database: " <> show q
    (x:[]) -> return (storePath </> Text.unpack x <.> "cbor")
    (_:_) -> error $ "Multiple measurements found in database: " <> show q

toAssoc :: MQuery -> Assoc Text
toAssoc (Query fields (MType dom cod)) =
  Assoc [("domain",dom),("codomain", cod)] <> fields

--------------------------------------------------------------------------------

deleteMeasurementIn :: FilePath -> MQuery -> IO ()
deleteMeasurementIn path mquery = do
  m <- getMeasurementIn path mquery
  removeFile m

--------------------------------------------------------------------------------

putMeasurementIn :: FilePath -> AQuery -> MQuery -> FilePath -> IO ()
putMeasurementIn path aquery mquery m = do
  let
    sqlPath = path </> "lf-database" <.> "sqlite"
    storePath = path </> "lf-store"

  prepareDatabase path

  h <- getHash m
  copyFile m (storePath </> Text.unpack h <.> "cbor")

  withSQLite sqlPath $ upsertRow (toRow mquery aquery h)

getHash :: FilePath -> IO Text
getHash path = do
  b <- ByteStringLazy.readFile path
  return $ Text.pack $ show (hashlazy b :: Digest SHA256)

toRow :: MQuery -> AQuery -> Text -> Row
toRow (Query fields (MType domain codomain)) aquery h =
  let
    species = fromJust $ lookupAssoc "species" fields
    specimen = fromJust $ lookupAssoc "specimen" fields
    tracking = fromJust $ lookupAssoc "tracking" fields
    subdomain = fromJust $ lookupAssoc "subdomain" fields
    measurement = fromJust $ lookupAssoc "measurement" fields
  in
    Row species specimen tracking domain subdomain codomain measurement h aquery Null

--------------------------------------------------------------------------------

search :: FilePath -> Assoc Text -> IO [MInfo]
search path assoc = fmap (fmap rowToInfo) $ do
  let
    sqlPath = path </> "lf-database" <.> "sqlite"

  withSQLite sqlPath (searchRows assoc)

rowToInfo :: Row -> MInfo
rowToInfo
  (Row species specimen tracking domain subdomain codomain measurement hsh aquery extra) =
  MInfo
    (Query
       ( Assoc [
         ("species", species)
       , ("specimen", specimen)
       , ("tracking", tracking)
       , ("subdomain", subdomain)
       , ("measurement", measurement)
       ])
       (MType domain codomain))
    aquery
    hsh
    extra

--------------------------------------------------------------------------------

match :: FilePath -> Assoc Text -> Text -> IO [Text]
match path assoc key = do
  let
    sqlPath = path </> "lf-database" <.> "sqlite"

  withSQLite sqlPath (matchRows assoc key)
