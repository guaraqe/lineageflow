{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Playground
  ( useDb
  , useDbEnv
  , listTypes
  , listMeasures
  , retrieveMeasure
  , fetchMeasurement
  , module X
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR
import LineageFlow.Database.SQLite

import LineageFlow.Plot as X
import LineageFlow.Statistics as X

import qualified Data.Text as Text
import Data.Text (Text)

import Foreign.Store

import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)

import Control.Lens

data LFStore = LFStore
  { _lfStore_database :: String
  , _lfStore_store :: HashMap (Text,Text) Word32
  } deriving Show

$(makeLenses ''LFStore)

type MeasureMap f a = HashMap Text (f a)

modifyStore :: Store a -> (a -> a) -> IO ()
modifyStore store f = do
  val <- readStore store
  writeStore store (f val)

lfStore :: Store LFStore
lfStore = Store 0

useDb :: FilePath -> IO ()
useDb dbPath = do
  val <- lookupStore 0
  if isNothing val
    then void $ newStore $ LFStore dbPath HashMap.empty
    else return ()

useDbEnv :: String -> IO ()
useDbEnv name = do
  Just path <- lookup name <$> getEnvironment
  useDb path

listTypes :: IO ([(Text,Text)])
listTypes = do
  let store = lfStore
  HashMap.keys . view lfStore_store <$> readStore store

listMeasures ::
  forall f a .
  (Domain f, Codomain a) =>
  Proxy f -> Proxy a -> IO [Text]
listMeasures domain image =
  let
    dom = domType domain
    img = codType image
  in do
    n <- (HashMap.! (dom,img)) . view lfStore_store <$> readStore lfStore
    HashMap.keys <$> readStore (Store n :: Store (MeasureMap f a))

fetchMeasure ::
  forall f a .
  (CBORGet f a, Domain f, Codomain a) =>
  Proxy f -> Proxy a -> Text -> MQuery -> IO (f a)
fetchMeasure domain image name query =
  let
    dom = domType domain
    img = codType image
  in do
    let store = lfStore
    storeVal <- readStore store
    file <- db_get (sqliteDatabase (view lfStore_database storeVal)) query
    measure <- io_get cbor file :: IO (f a)
    case HashMap.lookup (dom,img) (view lfStore_store storeVal) of
      Nothing -> do
        measureStore@(Store n) <- newStore (HashMap.empty :: MeasureMap f a)
        modifyStore store (over lfStore_store $ HashMap.insert (dom,img) n)
        modifyStore measureStore (HashMap.insert name measure)
      Just n -> do
        modifyStore (Store n) (HashMap.insert name measure)
    return measure

retrieveMeasure ::
  (CBORGet f a, Domain f, Codomain a) =>
  Proxy f -> Proxy a -> Text -> MQuery -> IO (f a)
retrieveMeasure domain image name query =
  let
    dom = domType domain
    img = codType image
  in do
    st <- HashMap.lookup (dom,img) . view lfStore_store <$> readStore (Store 0)
    case st of
      Nothing -> fetchMeasure domain image name query
      Just st' -> do
        mst <- HashMap.lookup name <$> readStore (Store st')
        case mst of
          Nothing -> fetchMeasure domain image name query
          Just m -> return m

fetchMeasurement ::
  forall f a .
  (CBORGet f a, Domain f, Codomain a) =>
  [Text] -> IO (f a)
fetchMeasurement vals = do

  let
    store = lfStore

  storeVal <- readStore store
  fields <- db_fields (sqliteDatabase (view lfStore_database storeVal))

  let
    domain = Proxy :: Proxy f
    codomain = Proxy :: Proxy a

    mquery =
      Query (Assoc $ zip fields vals) $
        MType (domType domain) (codType codomain)

    name = Text.pack $ show mquery

  retrieveMeasure domain codomain name mquery
