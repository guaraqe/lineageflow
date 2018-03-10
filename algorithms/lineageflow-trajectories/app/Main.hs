import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified VectorPairPathIntegral
import qualified ScalarPairPathIntegral

main = mainWith
  "lf-trajectories: Path integrals"
  $(embed "desc/main.md")
  run

data Program =
  ScalarPairPathIntegral Args |
  VectorPairPathIntegral Args
  deriving Generic

instance UI Program

run = \case
  VectorPairPathIntegral args ->
    runWith cbor args VectorPairPathIntegral.algorithm
  ScalarPairPathIntegral args ->
    runWith cbor args ScalarPairPathIntegral.algorithm
