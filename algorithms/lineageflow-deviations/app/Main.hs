import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Sisters
import qualified BornNeighbors
import qualified AllNeighbors

main = mainWith
  "lf-deviations"
  $(embed "desc/main.md")
  run

data Program
  = Sisters Args
  | BornNeighbors Args
  | AllNeighbors Args
  deriving Generic

instance UI Program

run = \case
  Sisters args ->
    runWith cbor args Sisters.algorithm
  BornNeighbors args ->
    runWith cbor args BornNeighbors.algorithm
  AllNeighbors args ->
    runWith cbor args AllNeighbors.algorithm
