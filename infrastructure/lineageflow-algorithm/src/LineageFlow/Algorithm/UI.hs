module LineageFlow.Algorithm.UI
  ( Args
  , mainWith
  , runWith
  , UI (..)
  ) where

import LineageFlow.Algorithm.Imports

import LineageFlow.Algorithm.Algorithm
import LineageFlow.Algorithm.Classes.P
import LineageFlow.Algorithm.Classes.I
import LineageFlow.Algorithm.Classes.O
import LineageFlow.Algorithm.Classes.PD
import LineageFlow.Algorithm.Classes.MD

import LineageFlow.Declaration
import LineageFlow.IO

import Options.Applicative hiding (runParser)

import qualified Data.Yaml as Yaml
import qualified Data.ByteString as ByteString

import Control.Exception
import System.IO

import Data.Monoid

--------------------------------------------------------------------------------

data Args =
  Meta !FilePath | Run !FilePath

metaParser :: Parser Args
metaParser = Meta
     <$> strOption
         ( long "meta-path"
        <> short 'm'
        <> metavar "FILEPATH"
        <> help "Path for the algorithm declaration to be written." )

runParser :: Parser Args
runParser = Run
     <$> strOption
         ( long "query-file"
        <> short 'q'
        <> help "Path to the algorithm query." )

programInputParser :: Parser Args
programInputParser = helper <*> subparser
  ( command "meta" (info (helper <*> metaParser)
      ( progDesc "Generates the algorithm' declaration." ))
 <> command "run" (info (helper <*> runParser)
      ( progDesc "Runs an algorithm query in a given database." ))
  )

--------------------------------------------------------------------------------

runWith ::
  (P p, I kg i, O kp o, PD p, MD i, MD o) =>
  IOMethod kg kp -> Args -> Algorithm p i o -> IO ()
runWith _ (Meta templatePath) (Algorithm meta _) =
  withFile templatePath WriteMode $ \file ->
    ByteString.hPut file (Yaml.encode meta)

runWith iomethod (Run queryPath) algorithm = do
  query <- Yaml.decodeFileEither queryPath >>= \case
    Left s -> error (displayException s)
    Right x -> return x

  runAlgorithm iomethod query algorithm
{-# INLINE runWith #-}

--------------------------------------------------------------------------------

class UI p where
  ui :: Parser p
  default
    ui :: (Generic p, UIG (Rep p)) => Parser p
  ui = to <$> uiG

class UIG f where
  uiG :: Parser (f a)

instance (UIG f, UIG g) => UIG (f :+: g) where
  uiG = (L1 <$> uiG) <|> (R1 <$> uiG)

instance UIG f => UIG (M1 D p f) where
  uiG = M1 <$> uiG

instance (Constructor p, UIS f) => UIG (M1 C p f) where
  uiG = got
    where
      name = convertConstructor (conName (undefined :: M1 C p f a))
      got = M1 <$> uiS name

class UIS f where
  uiS :: String -> Parser (f a)

instance UIS f => UIS (M1 S p f) where
  uiS name = M1 <$> uiS name

instance UIS (K1 i Args) where
  uiS name = fmap K1 $ subparser $
    command name (info programInputParser (progDesc name))

--------------------------------------------------------------------------------

mainWith :: UI a => String -> String -> (a -> IO ()) -> IO ()
mainWith title description work =
  customExecParser (prefs showHelpOnError) opts >>= work
  where
    opts =
      info
       ( helper <*> ui )
       ( fullDesc <>
         progDesc description <>
         header title )
{-# INLINE mainWith #-}
