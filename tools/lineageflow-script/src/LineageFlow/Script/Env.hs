{-# LANGUAGE FlexibleInstances #-}

module LineageFlow.Script.Env
  ( EnvWith (Env)
  , env_prefix
  , env_vars
  , env_loop
  , Env
  , withLoop
  ) where

import LineageFlow.Declaration

import LineageFlow.Script.Var
import LineageFlow.Script.Entry

import Data.List (transpose)
import Data.Monoid ((<>))
import Data.Foldable

import Control.Lens
import Data.Aeson.Types
import Data.Text (Text)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data EnvWith a = Env
  { _env_prefix :: a
  , _env_vars :: Assoc (CardF a)
  , _env_loop :: Assoc [CardF a]
  } deriving (Show, Eq, Generic, Functor)

$(makeLenses ''EnvWith)

type Env = EnvWith Text

instance ToJSON a => ToJSON (EnvWith a) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON a => FromJSON (EnvWith a) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance HasVars (EnvWith Entry) where
  getVars (Env p v _) =
    getVars p <>
    concatMap (concatMap getVars . toList) v

instance HasLoop (EnvWith Entry) where
  getLoop (Env _ _ l) =
    concatMap (concatMap (concatMap getVars . toList)) l

-- Augments the environment variable set with the given loop variables, with variables in parallel.
withLoop :: [Var] -> Env -> Maybe [Env]
withLoop vars env = do
  let
    lookupVar var = lookupAssoc (var ^. var_name) (env ^. env_loop)

  lists <- traverse lookupVar vars

  let
    renameVars matches = zipWith (\var val -> (var ^. var_name, val)) vars matches
    trans = fmap (Assoc . renameVars) (transpose lists)

  return $ fmap (\t -> over env_vars (t <>) env) trans
