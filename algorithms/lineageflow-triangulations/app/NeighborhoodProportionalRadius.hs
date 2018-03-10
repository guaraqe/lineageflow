module NeighborhoodProportionalRadius
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.Radius

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_position
      :: DSumMapL Time Cell Vector
      :% Single
      :? "Conversor from temporal to cellular view."
  , i_triangulation
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Local contacts between cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_radius
      :: DSumMapL Time Cell Scalar
      :% Single
      :? "Global contacts between cells."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S p) (S tri)) = (Output (S r))
  where
    r = neighborhoodProportionalRadius tri p

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Proportional neighborhood radius"
    $(embed "desc/neighborhood-proportional-radius.md")
    operation
