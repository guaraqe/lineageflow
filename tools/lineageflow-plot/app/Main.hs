{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Parser

import LineageFlow.Prelude
import LineageFlow.IO.CBOR
import LineageFlow.Plot

import Options.Applicative

--------------------------------------------------------------------------------

main = execParser (info (helper <*> parseInput) desc) >>= run
  where
    desc = fullDesc <> header "lf-plot : Scalar plot generator"

--------------------------------------------------------------------------------

getScalar ::
  forall i j .
  FilePath -> PlotType -> IO [DSumMapL i j Scalar]
getScalar query PlotTypeScalar =
  pure <$>
    io_get cbor query
getScalar query PlotTypeVectorNorm =
  pure . _fmap norm <$>
    io_get cbor query
getScalar query PlotTypeVectorSquare =
  pure . _fmap (\v -> v !.! v) <$>
    io_get cbor query
getScalar query PlotTypeVectorCoordinates =
  io_get cbor query >>= \(m :: DSumMapL i j Vector) -> return $
    [ _fmap (view _x) m
    , _fmap (view _y) m
    , _fmap (view _z) m
    ]

getPlotConfig :: LayoutConfig -> PlotType -> [LayoutConfig]
getPlotConfig config val =
  case val of
    PlotTypeVectorCoordinates ->
      let
        title = config ^. layoutConfig_title
        titleX = title <> ": X coordinate"
        titleY = title <> ": Y coordinate"
        titleZ = title <> ": Z coordinate"
      in
        [ config & layoutConfig_title .~ titleX
        , config & layoutConfig_title .~ titleY
        , config & layoutConfig_title .~ titleZ
        ]

    _ -> pure config

getPath :: FilePath -> PlotType -> [FilePath]
getPath path val =
  case val of
    PlotTypeVectorCoordinates ->
      [ path <> "-x"
      , path <> "-y"
      , path <> "-z"
      ]
    _ -> pure path

forM3_ :: Monad m => [a] -> [b] -> [c] -> (a -> b -> c -> m d) -> m ()
forM3_ x y z f = foldr (>>) (return ()) $ zipWith3 f x y z

run (database, output, input) = readCommandInput input >>= \case

  GraphGlobal plotInput query scale -> do

    let
      layout = plotInput ^. plotInput_layout
      config = plotInput ^. plotInput_plot
      value  = plotInput ^. plotInput_type

      paths = getPath output value
      layouts = getPlotConfig layout value

    dataset <- getScalar query (plotInput ^. plotInput_type)

    forM3_ dataset layouts paths $ \d l p ->
      savePlot p $ globalGraph config l d

  HistogramGlobal plotInput query bins -> do

    dataset <- getScalar query (plotInput ^. plotInput_type)

    let
      layout = plotInput ^. plotInput_layout
      config = plotInput ^. plotInput_plot
      value  = plotInput ^. plotInput_type

      paths = getPath output value
      layouts = getPlotConfig layout value


    forM3_ dataset layouts paths $ \d l p ->
      savePlot p $ globalHistogram config l bins d

  HistogramSlices plotInput query bins range -> do

    dataset <- getScalar query (plotInput ^. plotInput_type)

    let
      limited = fmap (limitData range) dataset

      layout = plotInput ^. plotInput_layout
      config = plotInput ^. plotInput_plot
      value  = plotInput ^. plotInput_type

      paths = getPath output value
      layouts = getPlotConfig layout value

    forM3_ limited layouts paths $ \d l p ->
      saveVideo p $ sliceHistogram config l bins d

--------------------------------------------------------------------------------

limitData :: RangeConfig -> DSumMapL i j a -> DSumMapL i j a
limitData (RangeConfig lo hi) l =
  let
    m = fromMaybe 0 lo
    n = fromMaybe (length . unIx . getCompose $ l) hi
  in
    over (composed . ixed) (take (n - m) . drop m) l
