{-# LANGUAGE LambdaCase #-}

module Declaration ( handler ) where

import Imports
import Types.App

import qualified Data.Text as Text
import qualified Data.Yaml as Yaml

import Text.Markdown
import Text.Blaze.Html.Renderer.Text

import System.IO.Temp
import System.Process

handler :: (Text, Text) -> App ADecl
handler (exec', opt') = liftIO $ do
  let exec = Text.unpack exec'
      opt = Text.unpack opt'

  decl <- withSystemTempFile "decl.yaml" $ \file _ -> do
    callCommand (exec <> " " <> opt <> " meta -m " <> file)
    Yaml.decodeFile file >>= \case
      Nothing -> error $ "Could not decode algorithm declaration at " <> file
      Just x -> return x

  return (renderDeclaration decl)

renderDeclaration :: ADecl -> ADecl
renderDeclaration =
  over (decl_desc . lazy) (renderHtml . markdown defaultMarkdownSettings)


