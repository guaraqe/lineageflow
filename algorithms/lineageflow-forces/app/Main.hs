import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Forces
--import qualified Pression
--import qualified Tension
import qualified Norm
import qualified Stress
import qualified Asym

main = mainWith
  "lf-forces"
  $(embed "desc/main.md")
  run

data Program =
  Forces Args |
--  Pression Args |
--  Tension Args |
  Norm Args |
  Asym Args |
  Stress Args
  deriving Generic

instance UI Program

run = \case
  Forces args ->
    runWith cbor args Forces.algorithm
--  Pression args ->
--    runWith cbor args Pression.algorithm
--  Tension args ->
--    runWith cbor args Tension.algorithm
  Norm args ->
    runWith cbor args Norm.algorithm
  Asym args ->
    runWith cbor args Asym.algorithm
  Stress args ->
    runWith cbor args Stress.algorithm
