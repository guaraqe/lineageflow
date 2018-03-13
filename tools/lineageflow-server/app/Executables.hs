module Executables ( handler ) where

import Imports
import Types.App

import qualified Data.Text as Text
import System.Directory
import System.Environment

handler :: App [Text]
handler = liftIO $ do
  execs <- mapM listDirectory =<< pathFolders
  return .
    fmap Text.pack .
    filter (\x -> x /= "lf-viewer" && x /= "lf-plot" && x /= "lf-server") .
    filter (\x -> take 3 x == "lf-") .
    concat $
      execs

pathFolders :: IO [FilePath]
pathFolders = do
  path <- getEnv "PATH"
  let paths = words $ fmap (\x -> if x == ':' then ' ' else x) path
  filterM doesDirectoryExist paths
