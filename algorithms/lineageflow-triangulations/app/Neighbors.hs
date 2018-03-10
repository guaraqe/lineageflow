module Neighbors
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.Neighbors

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_triangulation
      :: DSumMapL Time (DS4 Time Cell) (DS4 Time Cell)
      :% Single
      :? "3D triangulation."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S tri)) = (Output (S o))
  where
    o = Compose $ _fmap contacts $ getCompose tri

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Neighbors from triangulation"
    $(embed "desc/neighbors.md")
    operation
