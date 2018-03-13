{-# LANGUAGE OverloadedStrings #-}

module Viewer ( handler ) where

import Imports
import Types.App

import LineageFlow.Viewer.Interface

import System.Process
import System.IO.Temp
import qualified Data.Yaml as Yaml

handler :: VQuery -> App Int
handler input = do
  path <- toPath input
  liftIO $ do
    (file,_) <- openTempFile "/tmp" "viewer.yaml"
    Yaml.encodeFile file path
    _ <- spawnCommand ("lf-viewer -c " <> file)
    return 0

toPath :: VQuery -> App VPath
toPath query = do
  traverse toSinglePath query

toSinglePath :: MQuery -> App (Text, FilePath)
toSinglePath query =
  let
    Just name = lookupAssoc "measurement" (query ^. query_fields)
  in do
    dbpath <- viewM server_dbPath
    path <- liftIO $ db_get (sqliteDatabase dbpath) query
    return (name, path)
