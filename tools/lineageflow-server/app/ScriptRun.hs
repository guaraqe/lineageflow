module ScriptRun ( handler ) where

import Imports
import Types.App
import Utils

import qualified Algorithm

import qualified Data.Yaml as Yaml

handler :: (Text, Env) -> App [(Int, AQuery)]
handler (name, env) = do
  path <- viewM server_dbPath
  processes <- ask
  script <- liftIO $ Yaml.decodeFile (scriptPath path name)
  scripts <- liftIO $ fmap (view server_scripts) $ readIORef processes

  case script of
    Nothing -> return []
    Just s ->
      case runScript scripts env s of
        Nothing -> return []
        Just algs -> do
          nums <- traverse Algorithm.handler algs
          return $ zip nums algs
