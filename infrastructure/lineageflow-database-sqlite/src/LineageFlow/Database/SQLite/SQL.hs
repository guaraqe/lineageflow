{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LineageFlow.Database.SQLite.SQL
  ( Measurements (..)
  , measurements
  , insertMeasurements
  , upsertMeasurements
  , searchMeasurementss
  , matchMeasurementss
  ) where

import LineageFlow.Base.Utils (Assoc (..))

import Data.Foldable
import Data.String
import Data.Maybe

import Data.ByteString.Lazy (ByteString)
import Database.Selda
import Data.Text (unpack, stripPrefix)


data Measurements = Measurements
  { _measurements_species :: Text
  , _measurements_specimen :: Text
  , _measurements_tracking :: Text
  , _measurements_domain :: Text
  , _measurements_subdomain :: Text
  , _measurements_codomain :: Text
  , _measurements_measurement :: Text
  , _measurements_hash :: Text
  , _measurements_aquery :: ByteString
  , _measurements_extra :: ByteString
  } deriving (Eq, Show, Generic)

instance SqlRow Measurements

assocMeasurements :: Measurements -> Assoc Text
assocMeasurements (Measurements species specimen tracking domain subdomain codomain measurement _ _ _) = Assoc $
  ("species",species) :
  ("specimen",specimen) :
  ("tracking",tracking) :
  ("domain",domain) :
  ("subdomain",subdomain) :
  ("codomain",codomain) :
  ("measurement",measurement) :
  []

toCol :: Text -> Col s Text
toCol = fromString . unpack

--------------------------------------------------------------------------------

measurements :: Table Measurements
measurements = tableFieldMod "measurements"
  []
  (fromJust . stripPrefix "_measurements_")

insertMeasurements ::
  MonadSelda m =>
  Measurements -> m ()
insertMeasurements = insert_ measurements . pure

upsertMeasurements ::
  (MonadSelda m) =>
  Measurements -> m ()
upsertMeasurements row =
  let
    assoc = assocMeasurements row
    chck rowp = foldl' (.&&) true $ mapMaybe (restrictMeasurements rowp) (getAssoc assoc)
  in do
    deleteFrom_ measurements chck
    insert_ measurements (pure row)

lookupMeasurements :: Text -> Maybe (Row s Measurements -> Col s Text)
lookupMeasurements "species" = Just $ (! #_measurements_species)
lookupMeasurements "specimen" = Just $ (! #_measurements_specimen)
lookupMeasurements "tracking" = Just $ (! #_measurements_tracking)
lookupMeasurements "domain" = Just $ (! #_measurements_domain)
lookupMeasurements "subdomain" = Just $ (! #_measurements_subdomain)
lookupMeasurements "codomain" = Just $ (! #_measurements_codomain)
lookupMeasurements "measurement" = Just $ (! #_measurements_measurement)
lookupMeasurements "hash" = Just $ (! #_measurements_hash)
lookupMeasurements _ = Nothing

restrictMeasurements :: Row s Measurements -> (Text,Text) -> Maybe (Col s Bool)
restrictMeasurements rowp (key,val) =
  (\f -> f rowp .== toCol val) <$> lookupMeasurements key

restrictAssoc :: Row s Measurements -> Assoc Text -> Query s ()
restrictAssoc rowp assoc =
  restrict $
  foldl' (.&&) true $
  mapMaybe (restrictMeasurements rowp) (getAssoc assoc)

searchMeasurementss ::
  MonadSelda m =>
  Assoc Text -> m [Measurements]
searchMeasurementss assoc = query $ do
  p <- select measurements
  restrictAssoc p assoc
  return p

matchMeasurementss ::
  MonadSelda m =>
  Assoc Text -> Text -> m [Text]
matchMeasurementss assoc key = case lookupMeasurements key of
  Nothing -> return []
  Just f -> query $ do
    p <- select measurements
    restrictAssoc p assoc
    return (f p)
