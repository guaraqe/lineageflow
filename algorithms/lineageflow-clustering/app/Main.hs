import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Similarity
import qualified Mixing
import qualified Clusters
import qualified LiftClusters

main = mainWith
  "lf-clustering: Clustering of cells"
  $(embed "desc/main.md")
  run

data Program =
  Similarity Args |
  Mixing Args |
  Clusters Args |
  LiftClusters Args
  deriving Generic

instance UI Program

run = \case
  Similarity args ->
    runWith cbor args Similarity.algorithm
  Mixing args ->
    runWith cbor args Mixing.algorithm
  Clusters args ->
    runWith cbor args Clusters.algorithm
  LiftClusters args ->
    runWith cbor args LiftClusters.algorithm
