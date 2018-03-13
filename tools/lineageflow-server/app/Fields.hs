module Fields ( handler ) where

import Imports
import Types.App

handler :: App [Text]
handler = do
  path <- viewM server_dbPath
  liftIO $ db_fields (sqliteDatabase path)
