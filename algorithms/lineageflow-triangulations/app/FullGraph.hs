module FullGraph
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.FullGraph

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_mothers
      :: Mothers
      :% Single
      :? "Relation between cells and their mothers."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_fullGraph
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Full graph between cells."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S mothers)) = (Output (S o))
  where
    o = fullGraph mothers

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Full graph between cells."
    $(embed "desc/full-graph.md")
    operation
