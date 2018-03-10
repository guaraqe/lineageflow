{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Tracking
import qualified Restrict

main =
  mainWith
    "lf-tracking"
    $(embed "desc/main.md")
    run

data Program =
  CreateLineage Args |
  RestrictTracking Args
  deriving Generic

instance UI Program

run = \case
  CreateLineage args -> runWith cbor args Tracking.algorithm
  RestrictTracking args -> runWith cbor args Restrict.algorithm
