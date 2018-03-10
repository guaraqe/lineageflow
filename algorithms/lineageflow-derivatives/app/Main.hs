import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import qualified Simple
import qualified Holoborodko
import qualified Lanczos

main :: IO ()
main =
  mainWith
    "lf-derivatives"
    $(embed "desc/main.md")
    run

data Program =
  Simple Args |
  Holoborodko Args |
  Lanczos Args
  deriving Generic

instance UI Program

run :: Program -> IO ()
run = \case
  Simple args ->
    runWith cbor args Simple.algorithm
  Holoborodko args ->
    runWith cbor args Holoborodko.algorithm
  Lanczos args ->
    runWith cbor args Lanczos.algorithm
