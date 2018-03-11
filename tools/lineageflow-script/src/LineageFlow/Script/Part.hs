{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module LineageFlow.Script.Part
  ( ScriptPart
  , AQueryWith
  , mapAQueryWith
  , toScriptPart
  , scriptPartVars
  ) where

import LineageFlow.Declaration
import LineageFlow.Query

import LineageFlow.Script.Var
import LineageFlow.Script.Query
import Data.Monoid
import Data.Foldable

import Control.Lens
import Data.Aeson.Types

import qualified Rank2

--------------------------------------------------------------------------------

type AQueryWith a = Query (AType (QueryWith a))

mapAQueryWith :: (a -> b) -> AQueryWith a -> AQueryWith b
mapAQueryWith f = fmap g
  where
    h = over query_fields (fmap f)
    g = (Rank2.<$>) h

type ScriptPart = Query (AType ScriptQuery)

instance HasVars ScriptPart where
  getVars = scriptPartVars

instance ToJSON (AType ScriptQuery) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON (AType ScriptQuery) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

toScriptPart :: AQuery -> ScriptPart
toScriptPart (Query fields (AType p i o)) =
  let
    f = toScriptQueryC
  in
    Query fields $ AType (fmap f p) (fmap f i) (fmap f o)

scriptPartVars :: ScriptPart -> [Var]
scriptPartVars (Query _ (AType p i o)) =
  let
    f = concatMap scriptQueryVars . toList
  in
    concatMap f p <> concatMap f i <> concatMap f o
