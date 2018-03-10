{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module LineageFlow.Tracking.Types
  ( Tracking (..)
  , tracking_cell
  , tracking_mother
  , LineageSet (..)
  , lineageset_mothers
  , lineageset_children
  , lineageset_tc
  , lineageset_ct
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm

import qualified Foreign.Storable.Record as Store
import Control.Lens
import Codec.Serialise

data Tracking =
  Tracking
    { _tracking_cell :: Int
    , _tracking_mother :: Int
    } deriving (Show, Generic)

$(makeLenses ''Tracking)

instance Codomain Tracking where
  codType Proxy = "tracking"

instance Serialise Tracking

store :: Store.Dictionary Tracking
store =
  Store.run
    $ Tracking
      <$> Store.element _tracking_cell
      <*> Store.element _tracking_mother

instance Storable Tracking where
  sizeOf = Store.sizeOf store
  alignment = Store.alignment store
  peek = Store.peek store
  poke = Store.poke store

data LineageSet =
  LineageSet
    { _lineageset_mothers :: Mothers
    , _lineageset_children :: Children
    , _lineageset_tc :: TC
    , _lineageset_ct :: CT
    }

$(makeLenses ''LineageSet)
