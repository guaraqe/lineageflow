module Commands ( handler ) where

import Imports
import Types.App

import qualified Data.Text as Text
import System.Process
import Data.List (nub)

handler :: Text -> App [Text]
handler name = liftIO $ do
  desc <- readCreateProcess (shell (Text.unpack name <> " --help")) ""
  let opts' = tail . dropWhile (/= "Available commands:") . lines $ desc
      opts = fmap Text.pack . nub . concatMap words $ opts'
  return opts
