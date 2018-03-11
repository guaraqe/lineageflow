{-# LANGUAGE DeriveGeneric #-}

module LineageFlow.Import.Format.LineageTree
  ( formatLineageTree
  ) where

import LineageFlow.Prelude hiding (Format)
import qualified LineageFlow.BArrayU as BArrayU
import LineageFlow.Tracking.Types

import LineageFlow.Import.Types
import LineageFlow.Import.Utils

import qualified Data.ByteString.Lazy as B
import Data.Csv

import qualified Data.Vector.Algorithms.Merge as V

--------------------------------------------------------------------------------

data LineageTree = LineageTree
  { id_center :: Int
  , id_mother :: Int
  , x         :: Double
  , y         :: Double
  , z         :: Double
  , timestep  :: Int
  } deriving (Show, Generic)

instance FromRecord LineageTree where
  parseRecord v =
    LineageTree <$>
      v .! 0 <*>
      v .! 1 <*>
      v .! 2 <*>
      v .! 3 <*>
      v .! 4 <*>
      v .! 5

--------------------------------------------------------------------------------

convertLineageTree :: BArrayU LineageTree -> Initial
convertLineageTree v =
  let
    (tr,pos,t) = unzip3 $ fmap splitParts $ splitTime $ sortArray v
  in
    Initial
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ tr)
      (Compose $ mkIx $ fmap (mkIx . BArrayU.convert) $ pos)
      (mkIx $ fromList $ fmap Time t)
      Nothing
{-# INLINE convertLineageTree #-}

splitParts :: BArrayU LineageTree -> (BArrayU Tracking, BArrayU Vector, Int)
splitParts = g . BArrayU.unzip3 . fmap f
  where
    f l =
      ( Tracking (id_center l) (id_mother l)
      , V3 (x l) (y l) (z l)
      , timestep l
      )
    g (a,b,c) = (a,b,BArrayU.head c)
{-# INLINE splitParts #-}

splitTime :: BArrayU LineageTree -> [BArrayU LineageTree]
splitTime = splitRec f
  where
    f v a = timestep (BArrayU.head v) == timestep a
{-# INLINE splitTime #-}

sortArray :: BArrayU LineageTree -> BArrayU LineageTree
sortArray = BArrayU.modify $ V.sortBy (compare `on` timestep)

--------------------------------------------------------------------------------

parseLineageTree :: B.ByteString -> Either String (BArrayU LineageTree)
parseLineageTree = decode NoHeader
{-# INLINE parseLineageTree #-}

formatLineageTree :: Format
formatLineageTree = Format parseLineageTree convertLineageTree
