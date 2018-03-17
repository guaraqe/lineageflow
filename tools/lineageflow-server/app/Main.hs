{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

import Imports
import Utils

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.Maybe

import System.Process
import System.Exit

import System.FilePath
import System.Directory
import System.Environment

import qualified Data.Yaml as Yaml

import qualified Data.Text as Text

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Control.Concurrent

import Types.ServerState
import Types.App

import qualified Fields
import qualified Match
import qualified Search
import qualified Executables
import qualified Commands
import qualified Declaration
import qualified Algorithm
import qualified Check
import qualified Viewer
import qualified ScriptList
import qualified ScriptSave
import qualified ScriptGet
import qualified ScriptRun

import Launch

--------------------------------------------------------------------------------

main :: IO ()
main = do
  (path':_) <- getArgs
  path <- makeAbsolute path'
  processes <- newIORef (initState path)
  _ <- forkIO $ forever $ do
    threadDelay 1000000
    runApp renewState processes
  run 32323 $ logStdoutDev (app processes)

app :: Processes -> Application
app processes = serve (Proxy :: Proxy API) (server processes)

server :: Processes -> Server API
server processes = makeServer processes $
       Fields.handler
  :<|> Match.handler
  :<|> Search.handler
  :<|> Executables.handler
  :<|> Commands.handler
  :<|> Declaration.handler
  :<|> Algorithm.handler
  :<|> Check.handler
  :<|> Viewer.handler
  :<|> ScriptList.handler
  :<|> ScriptSave.handler
  :<|> ScriptGet.handler
  :<|> ScriptRun.handler

--------------------------------------------------------------------------------

renewState :: App ()
renewState = do
  manageRunning
  manageWaiting
  manageScripts

--------------------------------------------------------------------------------

-- Follows the currently running process, classifying it according to its exit
-- status.
manageRunning :: App ()
manageRunning = do
  processes <- viewM server_processes
  running <- viewM server_running

  case running of
    Nothing -> return ()
    Just n -> do
      let
        h =
          case IntMap.lookup n processes of
            Nothing -> error "Running ID without corresponding process"
            Just x -> x

      exit <- liftIO $ getProcessExitCode h

      case exit of
        Nothing -> return ()
        Just ExitSuccess -> do
          setM server_running Nothing
          overM server_succeded (Set.insert n)
        Just (ExitFailure _) -> do
          setM server_running Nothing
          overM server_failed (Set.insert n)

--------------------------------------------------------------------------------

-- Renew the set of scripts that are in the database.
-- Works, but can be made more elegantly.
manageScripts :: App ()
manageScripts = do
  path <- viewM server_dbPath

  files <- liftIO $ listDirectory (path </> "lf-scripts")

  let
    names = fmap (Text.pack . takeBaseName) files

    readScript name = do
      script <-
        fromMaybe (error "Script could not be read") <$>
          Yaml.decodeFile (scriptPath path name)
      return (name, script)

  list <- liftIO $ traverse readScript names

  setM server_scripts (Map.fromList list)

--------------------------------------------------------------------------------

-- Check for running processes. If there are none, launch the next in line in
-- the waiting list.
manageWaiting :: App ()
manageWaiting = do
  running <- viewM server_running
  waiting <- viewM server_waiting

  case running of
    Just _ -> return ()
    Nothing ->
      case IntMap.toAscList waiting of
        [] -> return ()
        ((n,alg):_) -> do

          handler <- launch alg
          setM server_running (Just n)
          overM server_processes (IntMap.insert n handler)
          overM server_waiting IntMap.deleteMin

--------------------------------------------------------------------------------
