module LineageFlow.Script.Run
  ( runScript
  ) where

import LineageFlow.Declaration
import LineageFlow.Query

import LineageFlow.Script.Entry
import LineageFlow.Script.Var
import LineageFlow.Script.Env
import LineageFlow.Script.Query
import LineageFlow.Script.Part
import LineageFlow.Script.Step

import Control.Monad
import Data.Monoid

import Control.Lens
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

--------------------------------------------------------------------------------
-- Script running

checkCard :: Card -> CardF a -> Maybe (CardF a)
checkCard Single x@(SingleF _) = Just x
checkCard Optional x@(OptionalF _) = Just x
checkCard Many x@(ManyF _) = Just x
checkCard _ _ = Nothing

replaceEntry :: Env -> Entry -> Maybe (CardF Text)
replaceEntry _ (EntryFixed x) =
  Just $ SingleF $ x
replaceEntry env (EntryPrefixed x) =
  Just $ SingleF $ view env_prefix env <> "_" <> x
replaceEntry env (EntryVar x) =
  case lookupAssoc (x ^. var_name) (env ^. env_vars) of
    Nothing -> Nothing
    Just cardf -> checkCard (x ^. var_card) cardf

replaceEntryCard :: Env -> CardF Entry -> Maybe (CardF Text)
replaceEntryCard env e = fmap join $ traverse (replaceEntry env) e

runScriptPart :: Env -> ScriptPart -> Maybe AQuery
runScriptPart env = traverse runAType
  where
    f :: CardF (ScriptQuery a) -> Maybe (CardF (Query a))
    f = fmap join . traverse (toQuery (replaceEntry env))
    runAType (AType p i o) =
      AType <$>
       (traverse f p) <*>
       (traverse f i) <*>
       (traverse f o)

runScriptLoop :: Env -> [Var] -> ScriptPart -> Maybe [AQuery]
runScriptLoop env vars script =
  let
    envs = withLoop vars env
  in
    envs >>= \envs' -> traverse (\e -> runScriptPart e script) envs'

takeSingle :: CardF a -> Maybe a
takeSingle (SingleF a) = Just a
takeSingle _ = Nothing

runEnvWith :: Env -> EnvWith Entry -> Maybe Env
runEnvWith env (Env p v l) =
  let
    f :: Traversable f => f (CardF Entry) -> Maybe (f (CardF Text))
    f = traverse (replaceEntryCard env)
  in
    Env <$>
      (join $ fmap takeSingle $ replaceEntry env p) <*>
      f v <*>
      (traverse f l)

runScriptStep :: Map Text Script -> Env -> ScriptStep -> Maybe [AQuery]
runScriptStep _ env (Step s) = fmap pure (runScriptPart env s)
runScriptStep _ env (Loop vars s) = runScriptLoop env vars s
runScriptStep scripts env (Import c tpl) = do -- Maybe
  script <- Map.lookup c scripts
  localEnv <- runEnvWith env tpl
  runScript scripts localEnv script

runScript :: Map Text Script -> Env -> Script -> Maybe [AQuery]
runScript scripts env = fmap concat . traverse (runScriptStep scripts env)
