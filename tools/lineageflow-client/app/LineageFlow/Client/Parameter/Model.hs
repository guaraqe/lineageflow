{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Parameter.Model
  ( PModel (PModel)
  , pmodel_declaration
  , pmodel_query
  ) where

import LineageFlow.Client.Prelude

data PModel = PModel
  { _pmodel_declaration :: CardF PDecl
  , _pmodel_query :: Maybe (CardF PQuery)
  } deriving (Eq, Show)

$(makeLenses ''PModel)
