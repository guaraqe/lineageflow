{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module LineageFlow.Database.SQLite.SQL
  ( measurements
  , insertRow
  , upsertRow
  , searchRows
  , matchRows
  ) where

import LineageFlow.Database.SQLite.Types

import LineageFlow.Database hiding (Query)

import Data.Foldable
import Data.String
import Data.Maybe

import Data.ByteString.Lazy (ByteString)
import Data.Aeson
import Database.Selda
import Data.Text (unpack)

--------------------------------------------------------------------------------

type RowP =
  RowID :*: -- id
  Text :*: -- species
  Text :*: -- specimen
  Text :*: -- tracking
  Text :*: -- domain
  Text :*: -- subdomain
  Text :*: -- codomain
  Text :*: -- measurement
  Text :*: -- hash
  ByteString :*: -- aquery
  ByteString -- extra

toCol :: Text -> Col s Text
toCol = fromString . unpack

pl :: a :*: b -> a
pl (a :*: _) = a

pr :: a :*: b -> b
pr (_ :*: b) = b

{-
rowp_id = pl
-}

rowp_species = pl . pr

rowp_specimen = pl . pr . pr

rowp_tracking = pl . pr . pr . pr

rowp_domain = pl . pr . pr . pr . pr

rowp_subdomain = pl . pr . pr . pr . pr . pr

rowp_codomain = pl . pr . pr . pr . pr . pr . pr

rowp_measurement = pl . pr . pr . pr . pr . pr . pr . pr

rowp_hash = pl . pr . pr . pr . pr . pr . pr . pr . pr

{-
rowp_aquery = pl . pr . pr . pr . pr . pr . pr . pr . pr . pr
rowp_extra = pr . pr . pr . pr . pr . pr . pr . pr . pr . pr
-}

--------------------------------------------------------------------------------

toRow :: RowP -> Row
toRow (_ :*: species :*: specimen :*: tracking :*: domain :*: subdomain :*:
       codomain :*: measurement :*: hash :*: aquery :*: extra) =
  Row species specimen tracking domain subdomain codomain measurement hash aq xtr
  where
    aq = fromJust (decode aquery)
    xtr = fromJust (decode extra)

fromRow :: Row -> RowP
fromRow (Row species specimen tracking domain subdomain codomain measurement hash aq xtr) =
  (def :*: species :*: specimen :*: tracking :*: domain :*: subdomain :*:
   codomain :*: measurement :*: hash :*: aquery :*: extra)
  where
   aquery = encode aq
   extra = encode xtr

litRow :: RowP -> Cols s RowP
litRow (ide :*: species :*: specimen :*: tracking :*: domain :*: subdomain :*:
        codomain :*: measurement :*: hash :*: aquery :*: extra) =
  literal ide :*: literal species :*: literal specimen :*: literal tracking :*: literal domain :*: literal subdomain :*:
  literal codomain :*: literal measurement :*: literal hash :*: literal aquery :*: literal extra

assocRow :: Row -> Assoc Text
assocRow (Row species specimen tracking domain subdomain codomain measurement _ _ _) = Assoc $
  ("species",species) :
  ("specimen",specimen) :
  ("tracking",tracking) :
  ("domain",domain) :
  ("subdomain",subdomain) :
  ("codomain",codomain) :
  ("measurement",measurement) :
  []

--------------------------------------------------------------------------------

measurements :: Table RowP
measurements = table "measurements" $
  autoPrimary "id" :*:
  required "species" :*:
  required "specimen" :*:
  required "tracking" :*:
  required "domain" :*:
  required "subdomain" :*:
  required "codomain" :*:
  required "measurement" :*:
  required "hash" :*:
  required "aquery" :*:
  required "extra"

insertRow ::
  MonadSelda m =>
  Row -> m ()
insertRow = insert_ measurements . pure . fromRow

upsertRow ::
  (MonadSelda m) =>
  Row -> m ()
upsertRow row =
  let
    assoc = assocRow row
    r = fromRow row
    chck rowp = foldl' (.&&) true $ mapMaybe (restrictRow rowp) (getAssoc assoc)
  in do
    deleteFrom_ measurements chck
    insert_ measurements (pure r)

lookupRow :: Text -> Maybe (Cols s RowP -> Cols s Text)
lookupRow "species" = Just $ rowp_species
lookupRow "specimen" = Just $ rowp_specimen
lookupRow "tracking" = Just $ rowp_tracking
lookupRow "domain" = Just $ rowp_domain
lookupRow "subdomain" = Just $ rowp_subdomain
lookupRow "codomain" = Just $ rowp_codomain
lookupRow "measurement" = Just $ rowp_measurement
lookupRow "hash" = Just $ rowp_hash
lookupRow _ = Nothing

restrictRow :: Cols s RowP -> (Text,Text) -> Maybe (Col s Bool)
restrictRow rowp (key,val) =
  (\f -> f rowp .== toCol val) <$> lookupRow key

restrictAssoc :: Cols s RowP -> Assoc Text -> Query s ()
restrictAssoc rowp assoc =
  restrict $
  foldl' (.&&) true $
  mapMaybe (restrictRow rowp) (getAssoc assoc)

searchRows ::
  MonadSelda m =>
  Assoc Text -> m [Row]
searchRows assoc = fmap (fmap toRow) $ query $ do
  p <- select measurements
  restrictAssoc p assoc
  return p

matchRows ::
  MonadSelda m =>
  Assoc Text -> Text -> m [Text]
matchRows assoc key = case lookupRow key of
  Nothing -> return []
  Just f -> query $ do
    p <- select measurements
    restrictAssoc p assoc
    return (f p)
