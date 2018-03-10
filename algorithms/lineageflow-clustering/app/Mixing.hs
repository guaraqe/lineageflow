module Mixing
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR
import LineageFlow.Clustering.Mixing

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_weights :: Double :% Many :? "Weights for the measurements."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_similarities
      :: Array (S2 Cell) Scalar
      :% Many
      :? "Similarities."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_mixed
      :: Array (S2 Cell) Scalar
      :% Single
      :? "Mixed similarity."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (M w)) (Input (M d)) = Output (S s)
  where
    s = mix w d

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Mixing of similarities"
    $(embed "desc/mixing.md")
    operation

