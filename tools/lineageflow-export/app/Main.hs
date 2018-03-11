{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import LineageFlow.Export.Format.Emb
import LineageFlow.Export.Format.Selection

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.Database.SQLite
import LineageFlow.IO.CBOR

import qualified Data.Text as Text
import Data.Text (Text)
import Options.Applicative
import qualified Data.ByteString.Lazy as B

data InputType =
  EMB Input |
  ISelection (Input, Text)
  deriving Show

data Input = Input
  { input_species :: Text
  , input_specimen :: Text
  , input_tracking :: Text
  , input_subdomain :: Text
  , input_path :: FilePath
  , input_database :: FilePath
  } deriving Show

input :: Parser Input
input = Input <$>
  strOpt "species" "Species corresponding to the file." <*>
  strOpt "specimen" "Specimen corresponding to the file." <*>
  strOpt "tracking" "Tracking corresponding to the file." <*>
  strOpt "subdomain" "Subdomain corresponding to the file." <*>
  (Text.unpack <$> strOpt "path" "Path to the file.") <*>
  (Text.unpack <$> strOpt "database" "Path to the database.")

selParser :: Parser (Input, Text)
selParser = liftA2 (,) input $
  strOpt "selection" "Selection to be exported"

strOpt :: String -> String -> Parser Text
strOpt s h = fmap Text.pack $
  strOption (
    long s <>
    short (head s) <>
    metavar (fmap toUpper s) <>
    help h )

commands :: Parser InputType
commands = helper <*> subparser
  (command "emb"
    (info
      (helper <*> fmap EMB input)
      (progDesc "Exports .emb files.")
    ) <>
   command "selection"
    (info
      (helper <*> fmap ISelection selParser)
      (progDesc "Exports selection .csv files.")
    )
  )

--------------------------------------------------------------------------------

main :: IO ()
main =
  customExecParser (prefs showHelpOnError) opts >>= run
  where
    opts =
      info
       ( helper <*> commands )
       ( fullDesc <>
         progDesc "Export of files from the LineageFlow database." <>
         header "lf-import" )

run :: InputType -> IO ()
run (EMB inp) = saveEmb inp
run (ISelection inp) = saveSelection inp

--------------------------------------------------------------------------------

saveEmb :: Input -> IO ()
saveEmb (Input species specimen tracking subdomain path database) =
  let
    fields name = [species, specimen, tracking, subdomain, name]
  in do

    emb <- toEmb <$>
      (getFromFields database $ fields "ct") <*>
      (getFromFields database $ fields "mothers") <*>
      (getFromFields database $ fields "tc") <*>
      (getFromFields database $ fields "absolute-time") <*>
      (getFromFields database $ fields "voxel-position")

    let file = embFile emb

    B.writeFile path file

saveSelection :: (Input,Text) -> IO ()
saveSelection (Input species specimen tracking subdomain path database, selection) =
  let
    fields name = [species, specimen, tracking, subdomain, name]
  in do

    sel <- encodedSelection <$>
      (getFromFields database $ fields "absolute-time") <*>
      (getFromFields database $ fields "tracking") <*>
      (getFromFields database $ fields "voxel-position") <*>
      (getFromFields database $ fields selection)

    B.writeFile path sel

--------------------------------------------------------------------------------

getFromFields ::
  forall f a .
  (CBORGet f a, Domain f, Codomain a) =>
  FilePath -> [Text] -> IO (f a)
getFromFields path vals = do
  fields <- db_fields (sqliteDatabase path)

  let
    domain = Proxy :: Proxy f
    codomain = Proxy :: Proxy a

    mquery =
      Query (Assoc $ zip fields vals) $
        MType (domType domain) (codType codomain)

  file <- db_get (sqliteDatabase path) mquery
  io_get cbor file
