{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Client.Types
  (
    Assoc
  , App
  , getFields
  , getMatching
  , getSearch
  , getExecutables
  , getCommands
  , getDeclaration
  , runAlgorithm
  , runCheck
  , runViewer
  , runApp
  , getScriptList
  , runScriptSave
  , getScript
  , runScriptEnv
  , InitState (..)
  ) where

import LineageFlow.Client.Prelude

import Servant.API
import Servant.Reflex

import Data.Proxy

--------------------------------------------------------------------------------

class InitState a where
  initState :: a

--------------------------------------------------------------------------------

type ReqIn t a = Dynamic t (Either Text a)
type ReqOut t m a = m (Event t (ReqResult () a))
type ReqFun t m a b = ReqIn t a -> Event t () -> ReqOut t m b

data Reqs t m = Reqs
  { req_fields :: m (Dynamic t [Text])
  , req_match :: ReqFun t m (Assoc Text,Text) [Text]
  , req_search ::  ReqFun t m (Assoc Text) [MInfo]
  , req_executables :: m (Dynamic t [Text])
  , req_commands :: ReqFun t m Text [Text]
  , req_declaration :: ReqFun t m (Text,Text) ADecl
  , req_algorithm :: ReqFun t m AQuery Int
  , req_check :: ReqFun t m Int Status
  , req_viewer :: ReqFun t m VQuery Int
  , req_scriptList :: m (Dynamic t [Text])
  , req_scriptSave :: ReqFun t m (Text, ScriptTemplate) Bool
  , req_scriptGet :: ReqFun t m Text ScriptInput
  , req_scriptRun :: ReqFun t m (Text, Env) [(Int,AQuery)]
  }

type App t m = ReaderT (Reqs t m) m

--------------------------------------------------------------------------------

liftMaybe ::
  MonadWidget t m =>
  m (Event t (ReqResult () a)) -> App t m (Event t a)
liftMaybe =  lift . fmap (fmapMaybe reqSuccess)

getFields :: MonadWidget t m => App t m (Dynamic t [Text])
getFields = do
  req <- ask
  lift $ req_fields req

getMatching ::
  MonadWidget t m =>
  Dynamic t (Assoc Text) -> Dynamic t Text -> Event t () ->
  App t m (Event t [Text])
getMatching env tar but = do
  req <- ask
  liftMaybe $ req_match req (fmap pure (liftA2 (,) env tar)) but

getSearch ::
  MonadWidget t m =>
  Dynamic t (Assoc Text) -> Event t () ->
  App t m (Event t [MInfo])
getSearch env but = do
  req <- ask
  liftMaybe $ req_search req (fmap pure env) but

getExecutables :: MonadWidget t m => App t m (Dynamic t [Text])
getExecutables = do
  req <- ask
  lift $ req_executables req

getCommands ::
  MonadWidget t m =>
  Dynamic t Text -> Event t () -> App t m (Event t [Text])
getCommands exe but = do
  req <- ask
  liftMaybe $ req_commands req (fmap pure exe) but

getDeclaration ::
  MonadWidget t m =>
  Dynamic t Text -> Dynamic t Text -> Event t () -> App t m (Event t ADecl)
getDeclaration exe com but = do
  req <- ask
  liftMaybe $ req_declaration req (fmap pure (liftA2 (,) exe com)) but

runAlgorithm ::
  MonadWidget t m =>
  Dynamic t AQuery -> Event t () ->
  App t m (Event t Int)
runAlgorithm que but = do
  req <- ask
  liftMaybe $ req_algorithm req (fmap pure que) but

runCheck ::
  MonadWidget t m =>
  Dynamic t Int -> Event t () -> App t m (Event t Status)
runCheck num but = do
  req <- ask
  liftMaybe $ req_check req (fmap pure num) but

runViewer ::
  MonadWidget t m =>
  Dynamic t VQuery -> Event t () -> App t m (Event t Int)
runViewer que but = do
  req <- ask
  liftMaybe $ req_viewer req (fmap pure que) but

getScriptList ::
  MonadWidget t m =>
  App t m (Dynamic t [Text])
getScriptList = do
  req <- ask
  lift $ req_scriptList req

runScriptSave ::
  MonadWidget t m =>
  Dynamic t Text -> Dynamic t ScriptTemplate -> Event t () -> App t m (Event t Bool)
runScriptSave name script e = do
  req <- ask
  liftMaybe $ req_scriptSave req (fmap pure (liftA2 (,) name script)) e

getScript ::
  MonadWidget t m =>
  Dynamic t Text -> Event t () -> App t m (Event t ScriptInput)
getScript d e = do
  req <- ask
  liftMaybe $ req_scriptGet req (fmap pure d) e

runScriptEnv ::
  MonadWidget t m =>
  Dynamic t Text -> Dynamic t Env -> Event t () -> App t m (Event t [(Int, AQuery)])
runScriptEnv name env e = do
  req <- ask
  liftMaybe $ req_scriptRun req (fmap pure (liftA2 (,) name env)) e

--------------------------------------------------------------------------------

runApp ::
  forall t m .
  ( SupportsServantReflex t m
  , MonadWidget t m
  ) =>
  App t m () -> m ()
runApp action = do
  let
    ( getFields_ :<|>
      getMatching_ :<|>
      getSearch_ :<|>
      getExecutables_ :<|>
      getCommands_ :<|>
      getDeclaration_ :<|>
      runAlgorithm_ :<|>
      runCheck_ :<|>
      runViewer_ :<|>
      getScriptList_ :<|>
      runScriptSave_ :<|>
      getScript_ :<|>
      runScript_
      ) =
        client
          (Proxy :: Proxy API)
          (Proxy :: Proxy m)
          (Proxy :: Proxy ())
          (constDyn (BaseFullUrl Http "localhost" 32323 ""))

  but <- getPostBuild

  fields <- fmap (fmapMaybe reqSuccess) $ getFields_ but
  fieldsDyn <- holdDyn [] fields
  execs <- fmap (fmapMaybe reqSuccess) $ getExecutables_ but
  execsDyn <- holdDyn [] execs
  scripts <- fmap (fmapMaybe reqSuccess) $ getScriptList_ but
  scriptsDyn <- holdDyn [] scripts

  runReaderT action $
    Reqs
      (return fieldsDyn)
      getMatching_
      getSearch_
      (return execsDyn)
      getCommands_
      getDeclaration_
      runAlgorithm_
      runCheck_
      runViewer_
      (return scriptsDyn)
      runScriptSave_
      getScript_
      runScript_

