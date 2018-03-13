module ScriptGet ( handler ) where

import Imports
import Types.App
import Utils

import qualified Data.Yaml as Yaml

handler :: Text -> App ScriptInput
handler name = do
  path <- viewM server_dbPath
  liftIO $ do
    s <- Yaml.decodeFile (scriptPath path name)

    case s of
      Nothing -> return mempty
      Just s' -> return (scriptInput s')
