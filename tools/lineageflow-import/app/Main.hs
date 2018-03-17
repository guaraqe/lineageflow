{-# LANGUAGE OverloadedStrings #-}

module Main where

import LineageFlow.Database.SQLite
import LineageFlow.IO.CBOR
import LineageFlow.Import
import LineageFlow.Import.Utils

import qualified Data.Text as Text
import Data.Text (Text)
import Data.Char (toUpper)
import Data.Monoid
import Options.Applicative hiding (many)
import Text.ParserCombinators.ReadP hiding (optional)
import Data.Maybe
import System.FilePath
import System.IO.Temp
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Linear.V3

data InputType =
  ILineageTree Input |
  ICSVData Input |
  ISmart Smart
  deriving Show

data Input = Input
  { input_species :: Text
  , input_specimen :: Text
  , input_tracking :: Text
  , input_path :: FilePath
  , input_database :: FilePath
  , input_voxel :: Maybe (Double, Double, Double)
  } deriving Show

input :: Parser Input
input = Input <$>
  strOpt "species" "Species corresponding to the file." <*>
  strOpt "specimen" "Specimen corresponding to the file." <*>
  strOpt "tracking" "Tracking corresponding to the file." <*>
  (Text.unpack <$> strOpt "path" "Path to the file.") <*>
  (Text.unpack <$> strOpt "database" "Path to the database.") <*>
  (optional $ read . Text.unpack <$> strOpt "voxel" "Voxel size.")

data Smart = Smart
  { smart_path :: FilePath
  , smart_database :: FilePath
  , smart_voxel :: Maybe (Double, Double, Double)
  } deriving Show

smart :: Parser Smart
smart = Smart <$>
  (Text.unpack <$> strOpt "path" "Path to the file.") <*>
  (Text.unpack <$> strOpt "database" "Path to the database.") <*>
  (optional $ read . Text.unpack <$> strOpt "voxel" "Voxel size.")

strOpt :: String -> String -> Parser Text
strOpt s h = fmap Text.pack $
  strOption (
    long s <>
    short (head s) <>
    metavar (fmap toUpper s) <>
    help h )

commands :: Parser InputType
commands = helper <*> subparser
  (command "lineagetree"
    (info
      (helper <*> fmap ILineageTree input)
      (progDesc "Imports .LineageTree files.")
    ) <>
   command "csv"
    (info
      (helper <*> fmap ICSVData input)
      (progDesc "Imports .csv files.")
    ) <>
   command "smart"
    (info
      (helper <*> fmap ISmart smart)
      (progDesc "Imports files based on extension.")
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
         progDesc "Import of files into the LineageFlow database." <>
         header "lf-import" )

run :: InputType -> IO ()
run (ILineageTree inp) = do
  putStrLn "Converting tracking data..."
  runInput "lineagetree" formatLineageTree inp
run (ICSVData inp) = do
  putStrLn "Converting tracking data..."
  runInput "csv" formatCSV inp
run (ISmart smr) =
  case fromSmart smr of
    Nothing -> error "Either format not supported or badly formatted filepath"
    Just inpt -> do
      run inpt

fromSmart :: Smart -> Maybe InputType
fromSmart (Smart path dbpath voxel) =
  join $ fmap fst $ listToMaybe $ flip readP_to_S (takeFileName path) $ do
    species <- munch1 (/= '_')
    skipMany1 (char '_')
    specimen <- munch1 (/= '_')
    skipMany1 (char '_')
    tracking <- munch1 (/= '.')
    skipMany1 (char '.')

    let
      inp =
        Input (Text.pack species) (Text.pack specimen) (Text.pack tracking) path dbpath voxel

    extension <- munch1 (const True)
    eof
    case extension of
      "LineageTree" -> return $ Just (ILineageTree inp)
      "csv" -> return $ Just (ICSVData inp)
      _ -> return $ Nothing

runInput :: Text -> Format -> Input -> IO ()
runInput name format inp = do
  file <- B.readFile (input_path inp)

  val <-
    case parseFormat format file of
      Left s -> error s
      Right s -> return s

  putInitial name inp val

--------------------------------------------------------------------------------

putInitial :: Text -> Input -> Initial -> IO ()
putInitial name inp (Initial tra pos tim msel) = do
  let
    makeQuery dom cod nom =
      Query (Assoc
        [ ("species",input_species inp)
        , ("specimen",input_specimen inp)
        , ("tracking",input_tracking inp)
        , ("subdomain","all")
        , ("measurement",nom) ])
        (MType dom cod)

    aQuery =
      Query (Assoc
        [ ("executable","lf-import")
        , ("command",name) ])
        (AType mempty mempty mempty)

    scale = maybe (V3 1 1 1) (\(a,b,c) -> V3 a b c) (input_voxel inp)

    db = sqliteDatabase (input_database inp)


  withSystemTempDirectory "import" $ \path -> do
    putStrLn "Writing tracking..."
    io_put cbor (path </> "tracking")  tra
    db_put db aQuery
      (makeQuery "(time,[cell,time])" "tracking" "tracking")
      (path </> "tracking")

    putStrLn "Writing voxels..."
    io_put cbor (path </> "voxel") pos
    db_put db aQuery
      (makeQuery "(time,[cell,time])" "vector" "voxel")
      (path </> "voxel")

    putStrLn "Writing position..."
    io_put cbor (path </> "position") (rescale scale pos)
    db_put db aQuery
      (makeQuery "(time,[cell,time])" "vector" "position")
      (path </> "position")

    putStrLn "Writing time..."
    io_put cbor (path </> "time") tim
    db_put db aQuery
      (makeQuery "time" "time" "absolute-time")
      (path </> "time")

    case msel of
      Nothing -> return ()
      Just sel -> do
        putStrLn "Writing selection..."
        io_put cbor (path </> "selection") sel
        db_put db aQuery
          (makeQuery "(time,[cell,time])" "number" "selection")
          (path </> "selection")
