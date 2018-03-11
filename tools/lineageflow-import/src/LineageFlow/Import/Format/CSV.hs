{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Import.Format.CSV
  ( formatCSV
  ) where

import LineageFlow.Prelude hiding (Format)
import qualified LineageFlow.BArrayU as BArrayU
import LineageFlow.Tracking.Types

import LineageFlow.Import.Types
import LineageFlow.Import.Utils

import qualified Data.ByteString.Lazy as B
import Data.Csv

--------------------------------------------------------------------------------

data CSVData = CSVData
  { nsel_id_center :: Int
  , nsel_id_mother :: Int
  , nsel_x         :: Double
  , nsel_y         :: Double
  , nsel_z         :: Double
  , nsel_timestep  :: Int
  } deriving Generic

instance FromNamedRecord CSVData where
  parseNamedRecord m =
    CSVData <$>
      m .: "id_center" <*>
      m .: "id_mother" <*>
      m .: "x" <*>
      m .: "y" <*>
      m .: "z" <*>
      m .: "timestep"

convertCSVData :: BArrayU CSVData -> Initial
convertCSVData v =
  let
    (tr,pos,t) = unzip3 $ fmap splitParts $ splitTime v
  in
    Initial
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ tr)
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ pos)
      (mkIx $ fromList $ fmap Time t)
      Nothing
{-# INLINE convertCSVData #-}

splitParts :: BArrayU CSVData -> (BArrayU Tracking, BArrayU Vector, Int)
splitParts = g . BArrayU.unzip3 . fmap f
  where
    f l =
      ( Tracking (nsel_id_center l) (nsel_id_mother l)
      , V3 (nsel_x l) (nsel_y l) (nsel_z l)
      , nsel_timestep l
      )
    g (a,b,c) = (a,b,BArrayU.head c)
{-# INLINE splitParts #-}

splitTime :: BArrayU CSVData -> [BArrayU CSVData]
splitTime = splitRec f
  where
    f v a = nsel_timestep (BArrayU.head v) == nsel_timestep a
{-# INLINE splitTime #-}

--------------------------------------------------------------------------------

data CSVSelection = CSVSelection
  { sel_id_center :: Int
  , sel_id_mother :: Int
  , sel_x         :: Double
  , sel_y         :: Double
  , sel_z         :: Double
  , sel_timestep  :: Int
  , sel_selection :: Int
  } deriving Generic

instance FromNamedRecord CSVSelection where
  parseNamedRecord m =
    CSVSelection <$>
      m .: "id_center" <*>
      m .: "id_mother" <*>
      m .: "x" <*>
      m .: "y" <*>
      m .: "z" <*>
      m .: "timestep" <*>
      m .: "selection"

convertCSVSelection :: BArrayU CSVSelection -> Initial
convertCSVSelection v =
  let
    (tr,pos,t,sel) = unzip4 $ fmap splitParts' $ splitTime' v
  in
    Initial
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ tr)
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ pos)
      (mkIx $ fromList $ fmap Time t)
      (Just $ Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ sel)
{-# INLINE convertCSVSelection #-}

splitParts' :: BArrayU CSVSelection -> (BArrayU Tracking, BArrayU Vector, Int, BArrayU Int)
splitParts' = g . BArrayU.unzip4 . fmap f
  where
    f l =
      ( Tracking (sel_id_center l) (sel_id_mother l)
      , V3 (sel_x l) (sel_y l) (sel_z l)
      , sel_timestep l
      , sel_selection l
      )
    g (a,b,c,d) = (a,b,BArrayU.head c,d)
{-# INLINE splitParts' #-}

splitTime' :: BArrayU CSVSelection -> [BArrayU CSVSelection]
splitTime' = splitRec f
  where
    f v a = sel_timestep (BArrayU.head v) == sel_timestep a
{-# INLINE splitTime' #-}

--------------------------------------------------------------------------------

data CSV =
  NoSelection (BArrayU CSVData) |
  Selection (BArrayU CSVSelection)

csvOptions :: DecodeOptions
csvOptions = defaultDecodeOptions {
      decDelimiter = fromIntegral (ord ';')
    }

parseCSV :: B.ByteString -> Either String CSV
parseCSV file =
  let
    nocsv = fmap snd $ decodeByNameWith csvOptions file
  in
    case nocsv of
      Right csv -> Right (Selection csv)
      Left _ -> fmap (NoSelection . snd) (decodeByNameWith csvOptions file)

convertCSV :: CSV -> Initial
convertCSV (NoSelection a) = convertCSVData a
convertCSV (Selection a) = convertCSVSelection a

formatCSV :: Format
formatCSV = Format parseCSV convertCSV
