import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Delaunay
import qualified Neighbors
import qualified Distance
import qualified FilterBigger
import qualified FilterSmaller
import qualified GlobalNeighbors
import qualified GlobalNeighborsGenealogy
import qualified NeighborhoodRadius
import qualified NeighborhoodProportionalRadius
import qualified Expand
import qualified FullGraph
import qualified Merge

main =
  mainWith
    "lf-triangulations: Manipulation of triangulations for LineageFlow"
    $(embed "desc/main.md")
    run

data Program =
  Delaunay Args |
  Neighbors Args |
  Distance Args |
  FilterBigger Args |
  FilterSmaller Args |
  GlobalNeighbors Args |
  GlobalNeighborsGenealogy Args |
  NeighborhoodRadius Args |
  NeighborhoodProportionalRadius Args |
  Expand Args |
  FullGraph Args |
  Merge Args
  deriving Generic

instance UI Program

run = \case
  Delaunay args ->
    runWith cbor args Delaunay.algorithm
  Neighbors args ->
    runWith cbor args Neighbors.algorithm
  Distance args ->
    runWith cbor args Distance.algorithm
  FilterBigger args ->
    runWith cbor args FilterBigger.algorithm
  FilterSmaller args ->
    runWith cbor args FilterSmaller.algorithm
  GlobalNeighbors args ->
    runWith cbor args GlobalNeighbors.algorithm
  GlobalNeighborsGenealogy args ->
    runWith cbor args GlobalNeighborsGenealogy.algorithm
  NeighborhoodRadius args ->
    runWith cbor args NeighborhoodRadius.algorithm
  NeighborhoodProportionalRadius args ->
    runWith cbor args NeighborhoodProportionalRadius.algorithm
  Expand args ->
    runWith cbor args Expand.algorithm
  FullGraph args ->
    runWith cbor args FullGraph.algorithm
  Merge args ->
    runWith cbor args Merge.algorithm
