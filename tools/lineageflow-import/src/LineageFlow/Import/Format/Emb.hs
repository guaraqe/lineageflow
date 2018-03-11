{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Convert.Emb
  ( convertEmb
  ) where

import           Data.Bifunctor
import qualified Data.ByteString                as Strict
import           Data.ByteString.Char8          (readInt)
import qualified Data.ByteString.Lazy           as Lazy
import           Data.Char                      (ord)
import           Data.Csv
import           Data.Vector                    (Vector)
import qualified Data.Vector                    as V

-------------------------------------------------------------------------------

type CsvL = [Record]

embOptions :: DecodeOptions
embOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ';')
    }

decodeEmb :: Lazy.ByteString -> Csv
decodeEmb b = case decodeWith embOptions NoHeader b of
  Left s -> error s
  Right v -> v

-------------------------------------------------------------------------------

embToCsv :: Csv -> [(Int,Csv)]
embToCsv = filterMap (null . snd) (second (V.fromList . reverse))
         . V.foldl' (flip addLineEmb) []

embToCsvList :: Csv -> [(Int,Lazy.ByteString)]
embToCsvList = map (second encodeV) . embToCsv

addLineEmb :: Record -> [(Int,CsvL)] -> [(Int,CsvL)]
addLineEmb r = case isNewTime r of
  Nothing -> mapHead (second (r:))
  Just t  -> ((t,[]):)

isNewTime :: Record -> Maybe Int
isNewTime v = if readIntJ (v V.! 0) == 0
                 then Just (readIntJ (v V.! 5))
                 else Nothing

-------------------------------------------------------------------------------

embHeader :: Lazy.ByteString
embHeader = encode [
  V.fromList [ "id_center"
             , "id_relative"
             , "x"
             , "y"
             , "z"
             , "timestep"
             , "something"
             , "validation"
             , "id_mother" :: Strict.ByteString ]
  ]

encodeV :: Vector Record -> Lazy.ByteString
encodeV = mappend embHeader . encode . V.toList

convertEmb :: Lazy.ByteString -> [Lazy.ByteString]
convertEmb = map snd
           . embToCsvList
           . decodeEmb


----------------------------------------------------------------------

filterMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterMap b f = go []
  where go l [] = l
        go l (x:xs) = if b x
                         then go l xs
                         else go (f x:l) xs

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

readIntJ :: Strict.ByteString -> Int
readIntJ n = case readInt n of
  Nothing -> error "Unable to parse Int from ByteString."
  Just (n',_) -> n'
