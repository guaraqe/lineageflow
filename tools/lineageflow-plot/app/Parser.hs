{-# LANGUAGE LambdaCase #-}

module Parser
  ( parseInput
  , readCommandInput
  ) where

import LineageFlow.Prelude
import LineageFlow.Plot

import qualified Data.Yaml as Yaml
import Options.Applicative

--------------------------------------------------------------------------------

readYaml :: Yaml.FromJSON a => FilePath -> IO a
readYaml path = Yaml.decodeFileEither path >>= \case
  Left e -> throw e
  Right v -> return v

--------------------------------------------------------------------------------

data CommandInput
  = CommandGraphGlobal FilePath FilePath Scale
  | CommandHistogramGlobal FilePath FilePath Int
  | CommandHistogramSlices FilePath FilePath Int (Maybe Int) (Maybe Int)
  deriving (Show, Eq)

readCommandInput :: CommandInput -> IO PlotCommandPath
readCommandInput (CommandGraphGlobal input query scale) =
  GraphGlobal <$>
    readYaml input <*>
    pure query <*>
    pure scale
readCommandInput (CommandHistogramGlobal input query bins) =
  HistogramGlobal <$>
    readYaml input <*>
    pure query <*>
    pure bins
readCommandInput (CommandHistogramSlices input query bins min max) =
  HistogramSlices <$>
    readYaml input <*>
    pure query <*>
    pure bins <*>
    pure (RangeConfig min max)

--------------------------------------------------------------------------------

strOpt :: String -> String -> Parser String
strOpt name desc =
  strOption $
    long name <>
    short (head name) <>
    metavar (fmap toUpper name) <>
    help desc

--------------------------------------------------------------------------------

getPlotType :: String -> PlotType
getPlotType = \case
  "scalar" -> PlotTypeScalar
  "vector-norm" -> PlotTypeVectorNorm
  "vector-square" -> PlotTypeVectorSquare
  "vector-coordinates" -> PlotTypeVectorCoordinates
  _ -> error
    "value must be either 'scalar', 'vector-norm', 'vector-square' or 'vector-coordinates'."

parsePlotType :: Parser PlotType
parsePlotType = fmap getPlotType $
  strOpt
    "value"
    "Either 'scalar', 'vector-norm', 'vector-square' or 'vector-coordinates'"

getScale True = ScaleLog
getScale False = ScaleLin

parseScale :: Parser Scale
parseScale = fmap getScale $
  switch $
    long "log" <>
    short 'l' <>
    help "Whether to plot the logarithm"

parseMQuery :: Parser FilePath
parseMQuery =
  strOpt "query" "Path to the measure query"

parsePlotInput :: Parser FilePath
parsePlotInput =
  strOpt "query" "Path to the plot definition"

parseDatabase :: Parser FilePath
parseDatabase =
  strOpt "database" "Path to the database"

parseOut :: Parser FilePath
parseOut =
  strOpt "out" "Path to the output file"

parseBins :: Parser Int
parseBins = read <$>
  strOpt "bins" "Number of bins on the histograms"

parseMin :: Parser (Maybe Int)
parseMin = optional $ fmap read $
  strOpt "bins" "Number of bins on the histograms"

parseMax :: Parser (Maybe Int)
parseMax = optional $ fmap read $
  strOpt "bins" "Number of bins on the histograms"

--------------------------------------------------------------------------------

parseCommandGraphGlobal :: Parser CommandInput
parseCommandGraphGlobal =
  CommandGraphGlobal <$>
    parsePlotInput <*>
    parseMQuery <*>
    parseScale

parseCommandHistogramGlobal :: Parser CommandInput
parseCommandHistogramGlobal =
  CommandHistogramGlobal <$>
    parsePlotInput <*>
    parseMQuery <*>
    parseBins

parseCommandHistogramSlices :: Parser CommandInput
parseCommandHistogramSlices =
  CommandHistogramSlices <$>
    parsePlotInput <*>
    parseMQuery <*>
    parseBins <*>
    parseMin <*>
    parseMax

comm name parser =
  ( command name
    (info
      ( helper <*> parser' )
      ( fullDesc
     <> header "lf-plot : Scalar plot generator" )))
  where
    parser' =
      (,,) <$>
        parseDatabase <*>
        parseOut <*>
        parser

parseInput :: Parser (FilePath, FilePath, CommandInput)
parseInput =
  subparser $
    comm "graph-global" parseCommandGraphGlobal <>
    comm "histogram-global" parseCommandHistogramGlobal <>
    comm "histogram-slices" parseCommandHistogramSlices


