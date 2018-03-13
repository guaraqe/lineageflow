{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Algorithm.Model
  ( AModel (AModel)
  , amodel_executable
  , amodel_command
  , amodel_declaration
  , amodel_parameters
  , amodel_inputs
  , amodel_outputs
  , getAQuery
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Parameter.Model
import LineageFlow.Client.Measurement.Model

data AModel = AModel
  { _amodel_executable :: Maybe Text
  , _amodel_command :: Maybe Text
  , _amodel_declaration :: Maybe ADecl
  , _amodel_parameters :: [(Text,PModel)]
  , _amodel_inputs :: [(Text,MModel)]
  , _amodel_outputs :: [(Text,MModel)]
  } deriving (Eq, Show)

$(makeLenses ''AModel)

instance InitState AModel where
  initState =
    AModel Nothing Nothing Nothing [] [] []

getAQuery :: AModel -> Maybe AQuery
getAQuery model = do
  let
    f l (t,x) = sequence (t,(view l x))

  p <- traverse (f pmodel_query) (model ^. amodel_parameters)
  i <- traverse (f mmodel_query) (model ^. amodel_inputs)
  o <- traverse (f mmodel_query) (model ^. amodel_outputs)
  e <- model ^. amodel_executable
  c <- model ^. amodel_command

  return $ Query (Assoc [("executable",e),("command",c)]) (AType (Assoc p) (Assoc i) (Assoc o))

