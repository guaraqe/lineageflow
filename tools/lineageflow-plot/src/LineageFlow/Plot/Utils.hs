{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Plot.Utils
  ( isRegular
  , replaceWith
  , removeWith
  , getBounds
  , colorGradient
  , colorGradientRGB
  , testLines
  , (.=)
  --
  , keepRegular
  , rescale
  , bound
  ) where

import LineageFlow.Prelude
import LineageFlow.Plot.Interface
import qualified Control.Foldl as Fold
import Data.Colour hiding (over)
import Data.Colour.SRGB
import Control.Lens hiding ((.=))

(.=) :: a -> b -> (a,b)
(.=) = (,)

isRegular :: Double -> Bool
isRegular x = not (isInfinite x) && not (isNaN x)

replaceWith ::
  (a -> Double) -> (Double -> a -> a) -> Double -> Double -> [a] -> [a]
replaceWith _ _ _ _ [] = []
replaceWith getD setD mi ma (x:xs)
  | isNaN (getD x) = replaceWith getD setD mi ma xs
  | getD x > ma = setD ma x : replaceWith getD setD mi ma xs
  | getD x < mi = setD mi x : replaceWith getD setD mi ma xs
  | otherwise = x : replaceWith getD setD mi ma xs

removeWith ::
  (a -> Double) -> (Double -> a -> a) -> Double -> Double -> [a] -> [a]
removeWith _ _ _ _ [] = []
removeWith getD setD mi ma (x:xs)
  | isNaN (getD x) = removeWith getD setD mi ma xs
  | getD x > ma = removeWith getD setD mi ma xs
  | getD x < mi = removeWith getD setD mi ma xs
  | otherwise = x : removeWith getD setD mi ma xs

minMax :: [Double] -> (Double,Double)
minMax l =
  Fold.fold (
    liftA2 (\i j -> (fromMaybe 0 i, fromMaybe 0 j)) Fold.minimum Fold.maximum ) $
    filter isRegular l

getBounds :: Maybe Double -> Maybe Double -> [[Double]] -> (Double, Double)
getBounds mi ma l =
  let
    mima = fmap minMax l
    mi' = minimum (fmap fst mima)
    ma' = maximum (fmap snd mima)
  in
    ( fromMaybe mi' mi
    , fromMaybe ma' ma )

--------------------------------------------------------------------------------

keepRegular :: [(Double, Double)] -> [(Double, Double)]
keepRegular = filter (\(x,y) -> isRegular x && isRegular y)

rescale ::
  (Double, Double) -> (Double, Double) -> [(Double, Double)] -> [(Double, Double)]
rescale tX tY vals =
  let
    apply (a,b) = \t -> a + b * t
  in
    fmap (bimap (apply tX) (apply tY)) vals

bound ::
  Maybe Double -> Maybe Double ->
  Maybe Double -> Maybe Double ->
  [(Double, Double)] -> ([(Double,Double)], [(Double, Double)])
bound miX maX miY maY vals =
  let
    (xMin,xMax) =
      getBounds miX maX [fmap (view _1) vals]

    vals'' = removeWith (view _1) (set _1) xMin xMax vals

    (yMin,yMax) =
      getBounds miY maY [fmap (view _2) vals'']

  in
    ( [(xMin,yMin),(xMax,yMax)]
    , replaceWith (view _2) (set _2) yMin yMax vals'' )

--------------------------------------------------------------------------------

colorGradientElem :: Int -> Int -> Colour Double
colorGradientElem n m
    | s <= 1 = blend s breen blue
    | s <= 2 = blend (s - 1) green breen
    | s <= 3 = blend (s - 2) yellow green
    | s <= 4 = blend (s - 3) red yellow
    | otherwise = red
  where
    blue = sRGB 0 0 1
    breen = sRGB 0 0.7 0.7
    green = sRGB 0 1 0
    yellow = sRGB 0.7 0.7 0
    red = sRGB 1 0 0
    s = 4 * fromIntegral m / fromIntegral n

colorGradient :: Int -> [AlphaColour Double]
colorGradient n =
  fmap (opaque . colorGradientElem (n - 1)) [0 .. n - 1]

colorGradientRGB :: Int -> [Color]
colorGradientRGB n =
  fmap (toT . toSRGB . colorGradientElem (n - 1)) [0 .. n - 1]
  where
    toT (RGB r g b) = Color r g b 1

--------------------------------------------------------------------------------

testLines :: [(String,[Double])]
testLines = fmap (\s -> ("",replicate 10 s)) [0,0.1..100]
