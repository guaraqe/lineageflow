module Match ( handler ) where

import Imports
import Types.App

handler :: (Assoc Text, Text) -> App [Text]
handler (fs,x) = do
  path <- viewM server_dbPath
  liftIO $ db_match (sqliteDatabase path) fs x
