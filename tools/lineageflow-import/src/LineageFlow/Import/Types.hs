{-# LANGUAGE ExistentialQuantification #-}

module LineageFlow.Import.Types
 ( Initial (..)
 , Format (..)
 , parseFormat
 ) where

import LineageFlow.Prelude hiding (Format)
import LineageFlow.Tracking.Types

import qualified Data.ByteString.Lazy as BL

data Initial = Initial
  { initial_tracking :: DSumMapL Time Cell Tracking
  , initial_position :: DSumMapL Time Cell Vector
  , initial_time :: Array Time Time
  , initial_selection :: Maybe (DSumMapL Time Cell Int)
  }

data Format = forall a . Format
  { format_parse :: BL.ByteString -> Either String a
  , format_convert :: a -> Initial
  }

parseFormat :: Format -> BL.ByteString -> Either String Initial
parseFormat (Format parse convert) file =
  convert <$> parse file
