module Similarity
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR
import LineageFlow.Clustering.Similarity

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_dissimilarity
      :: Array (S2 Cell) Scalar
      :% Single
      :? "Dissimilarities."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_similarity
      :: Array (S2 Cell) Scalar
      :% Single
      :? "Similarities."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S d)) = Output (S s)
  where
    s = toSimilarity d

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Converter to similarity"
    $(embed "desc/similarity.md")
    operation

