module ScriptList ( handler ) where

import Imports
import Types.App

import System.Directory
import System.FilePath

import qualified Data.Text as Text

handler :: App [Text]
handler = do
  path <- viewM server_dbPath
  liftIO $ do
    files <- listDirectory (path </> "lf-scripts")
    return $ fmap (Text.pack . takeBaseName) files
