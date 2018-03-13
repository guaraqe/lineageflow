{-# LANGUAGE TemplateHaskell #-}

module Types.ServerState
  ( ServerState (ServerState)
  , server_dbPath
  , server_running
  , server_waiting
  , server_succeded
  , server_failed
  , server_processes
  , server_scripts
  , Processes
  , initState
  ) where

import Imports

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import System.Process

data ServerState = ServerState
  { _server_dbPath :: FilePath
  , _server_running :: Maybe Int
  , _server_waiting :: IntMap AQuery
  , _server_succeded :: Set Int
  , _server_failed :: Set Int
  , _server_processes :: IntMap ProcessHandle
  , _server_scripts :: Map Text Script
  }

$(makeLenses ''ServerState)

type Processes = IORef ServerState

initState :: FilePath -> ServerState
initState path = ServerState path Nothing IntMap.empty Set.empty Set.empty IntMap.empty Map.empty
