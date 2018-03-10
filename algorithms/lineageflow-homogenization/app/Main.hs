import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified SpaceGaussianVector
import qualified SpaceGaussianTensor
import qualified TimeFlatTensor
import qualified Contacts

main :: IO ()
main =
  mainWith
    "lf-homogenization: Homogenization for LineageFlow"
    $(embed "desc/main.md")
    run

data Program =
  SpaceGaussianVector Args |
  SpaceGaussianTensor Args |
  TimeFlatTensor Args |
  Contacts Args
  deriving Generic

instance UI Program

run :: Program -> IO ()
run = \case
  SpaceGaussianVector args ->
    runWith cbor args SpaceGaussianVector.algorithm
  SpaceGaussianTensor args ->
    runWith cbor args SpaceGaussianTensor.algorithm
  TimeFlatTensor args ->
    runWith cbor args TimeFlatTensor.algorithm
  Contacts args ->
    runWith cbor args Contacts.algorithm
