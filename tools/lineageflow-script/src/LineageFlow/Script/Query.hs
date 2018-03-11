{-# LANGUAGE FlexibleInstances #-}

module LineageFlow.Script.Query
  ( ScriptQuery
  , toScriptQueryC
  , scriptQueryVars
  , toQuery
  ) where

import LineageFlow.Query

import LineageFlow.Script.Var
import LineageFlow.Script.Entry

import Data.Maybe (mapMaybe)
import Data.Functor.Compose (Compose (..))

import Control.Lens
import Data.Text (Text)

--------------------------------------------------------------------------------

type ScriptQuery = QueryWith Entry

instance HasVars (QueryWith Entry a) where
  getVars = scriptQueryVars

toScriptQuery :: Card -> Query a -> ScriptQuery a
toScriptQuery card = bimap (readEntry card) id

toScriptQueryC :: CardF (Query a) -> CardF (ScriptQuery a)
toScriptQueryC = withCard toScriptQuery

scriptQueryVars :: ScriptQuery a -> [Var]
scriptQueryVars (Query fields _) =
  mapMaybe (preview (_2 . _EntryVar)) (getAssoc fields)

toQuery :: (Entry -> Maybe (CardF Text)) -> ScriptQuery a -> Maybe (CardF (Query a))
toQuery f (Query fields x) = getCompose $
  fmap (\fi -> Query (Assoc fi) x) (traverse (traverse (Compose . f)) (getAssoc fields))
