module ScriptSave ( handler ) where

import Imports
import Types.App
import Utils

import qualified Data.Yaml as Yaml

handler :: (Text, ScriptTemplate) -> App Bool
handler (name, script) = do
  path <- viewM server_dbPath
  liftIO $ do
    Yaml.encodeFile (scriptPath path name) (fromScriptTemplate script)
    return True
