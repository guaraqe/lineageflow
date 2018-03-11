{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Export.Format.Selection
  ( encodedSelection
  ) where

import LineageFlow.Prelude
import LineageFlow.Tracking

import qualified LineageFlow.BArrayU as BArrayU

import Data.Csv

import qualified Data.ByteString.Lazy as B

--------------------------------------------------------------------------------

data Selection = Selection
  { id_center :: Int
  , selection :: Int
  , timestep :: Int
  , x :: Double
  , y :: Double
  , z :: Double
  , id_mother :: Int
  , validation :: Int
  } deriving (Show, Generic)

instance ToNamedRecord Selection

encodeSelection :: BArrayU Selection -> B.ByteString
encodeSelection = encodeByNameWith opts h . toList
  where
    opts = defaultEncodeOptions {encDelimiter = fromIntegral (ord ';')}
    h = fromList
      ["id_center","selection","timestep","x","y","z","id_mother","validation"]

encodedSelection ::
  Array Time Time ->
  TCMap Tracking ->
  TCMap Vector ->
  TCMap Int ->
  B.ByteString
encodedSelection t tr pos sel = encodeSelection $ toSelection t tr pos sel

--------------------------------------------------------------------------------

makeSelection :: Time -> Tracking -> Vector -> Int -> Selection
makeSelection (Time t) (Tracking c m) (V3 x y z) s =
  Selection c (s+1) t x y z m 0

makeSelectionV ::
  Time ->
  Array i Tracking ->
  Array i Vector ->
  Array i Int ->
  BArrayU Selection
makeSelectionV t tr pos sel =
  BArrayU.zipWith3 (makeSelection t)
    (BArrayU.convert tr)
    (BArrayU.convert pos)
    (BArrayU.convert sel)

toSelection ::
  Array Time Time ->
  TCMap Tracking ->
  TCMap Vector ->
  TCMap Int ->
  BArrayU Selection
toSelection t (Compose tr) (Compose pos) (Compose sel) = BArrayU.concat $
  zipWith4 makeSelectionV (toList t) (toList tr) (toList pos) (toList sel)
