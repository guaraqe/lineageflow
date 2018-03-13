{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Measurement.Model
  ( MModel (MModel)
  , mmodel_declaration
  , mmodel_query
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Selection.Model
import Data.Map.Strict as Map (Map, lookup)

data MModel = MModel
  { _mmodel_declaration :: CardF MDecl
  , _mmodel_query :: Maybe (CardF MQuery)
  } deriving (Eq, Show)

$(makeLenses ''MModel)
