module Search ( handler ) where

import Imports
import Types.App

handler :: Assoc Text-> App [MInfo]
handler fs = do
  path <- viewM server_dbPath
  liftIO $ db_search (sqliteDatabase path) fs
