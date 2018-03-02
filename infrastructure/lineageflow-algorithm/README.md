# lineageflow-algorithm

This package contains an executable generator from a declarative algorithm definition.
One can using it by having different modules for algorithms:

``` haskell
module Algorithm
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.IO.CBOR

--------------------------------------------------------------------------------

data Parameter = Parameter
  { parameter_name
      :: Int
      :% Optional
      :? "Some parameter"
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { input_name
      :: TCMap Vector
      :% Single
      :? "Some input"
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_name
      :: TCMap Vector
      :% Many
      :? "Many outputs"
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation = ...

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "algorithm"
    $(embed "descriptions/algorithm.md")
    operation
```

and a `Main` that puts them together:

```haskell
import LineageFlow.Prelude
import LineageFlow.IO.CBOR

import qualified Algorithm

main = mainWith
  "lf-executable: Name of your executable"
  $(embed "descriptions/main.md")
  run

data Program =
  Algorithm Args
  deriving Generic

instance UI Program

run = \case
  Algorithm args -> runWith cborDatabase args Algorithm.algorithm
```
