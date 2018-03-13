module Utils
  ( scriptPath
  ) where

import Imports
import qualified Data.Text as Text
import System.FilePath

scriptPath :: FilePath -> Text -> FilePath
scriptPath path name = path </> "lf-scripts" </> Text.unpack name <.> "yaml"
