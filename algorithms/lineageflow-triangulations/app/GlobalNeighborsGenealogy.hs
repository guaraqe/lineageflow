module GlobalNeighborsGenealogy
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.GlobalNeighbors

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
      :? "Relation from cells to their mothers."
  , i_tc
      :: TC
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
  { o_contacts
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Global contacts between cells."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S m) (S tc) (S tri)) = (Output (S o))
  where
    o = globalNeighborsGenealogy m tc tri

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Global neighbors from local neighbors"
    $(embed "desc/global-neighbors-genealogy.md")
    operation
