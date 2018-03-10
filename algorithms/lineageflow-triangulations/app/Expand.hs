module Expand
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.Expand

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_steps
      :: Int
      :% Single
      :? "Number of steps to walk on the neighborhood graph."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Set of cell contacts."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_expandedContacts
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Expanded set of cell contacts."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (S step)) (Input (S tri)) = (Output (S o))
  where
    o = expand step tri

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Expansion of cell contacts"
    $(embed "desc/expand.md")
    operation
