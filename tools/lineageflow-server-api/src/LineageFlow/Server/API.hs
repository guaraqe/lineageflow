{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module LineageFlow.Server.API
  ( API
  , Status (..)
  , module Export
  ) where

import Servant.API

import LineageFlow.Query as Export
import LineageFlow.Declaration as Export

import LineageFlow.Viewer.Interface
import LineageFlow.Script

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics
import Data.Aeson

infixr 9 :~>
type (:~>) a b = ReqBody '[JSON] a :> Post '[JSON] b

type API =
       Fields
  :<|> Match
  :<|> Search
  :<|> Executables
  :<|> Commands
  :<|> AlgorithmDecl
  :<|> AlgorithmRun
  :<|> CheckRun
  :<|> ViewerRun
  :<|> ScriptList
  :<|> ScriptSave
  :<|> ScriptGet
  :<|> ScriptRun

type Fields = "fields" :> Get '[JSON] [Text]

type Match = "match" :> (Assoc Text,Text) :~> [Text]

type Search = "search" :> Assoc Text :~> [MInfo]

type Executables = "executables" :> Get '[JSON] [Text]

type Commands = "commands" :> Text :~> [Text]

type AlgorithmDecl = "declaration" :> (Text, Text) :~> ADecl

type AlgorithmRun = "algorithm" :> AQuery :~> Int

type CheckRun = "check" :> Int :~> Status

type ViewerRun = "viewer" :> VQuery :~> Int

type ScriptList = "script-list" :> Get '[JSON] [Text]
type ScriptSave = "script-save" :> (Text,ScriptTemplate) :~> Bool
type ScriptGet = "script-get" :> Text :~> ScriptInput
type ScriptRun = "script-run" :> (Text,Env) :~> [(Int, AQuery)]

data Status =
  StatusNever |
  StatusRunning |
  StatusWaiting |
  StatusSuccess |
  StatusFailed
  deriving (Show, Eq, Generic)

instance FromJSON Status
instance ToJSON Status
