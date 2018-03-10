module Contacts
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Homogenization.Triangulation

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_filterSize
      :: Int
      :% Single
      :? "Filter size."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts between cells."
  , i_tc
      :: DSumTA Time Cell
      :% Single
      :? "Conversion from temporal to cellular point of view."
  , i_ct
      :: DSumTA Cell Time
      :% Single
      :? "Conversion from cellular to temporal point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Aggregated contacts between cells."
  , o_connexity
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Connexity factor for each contact."

  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  (Parameter (S size))
  (Input (S contacts) (S tc) (S ct)) =
  Output (S con) (S val)
  where
    (con, val) = homogenizeTri size tc ct contacts

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Temporal flat homogenization of cell contacts"
  $(embed "desc/time-flat-contacts.md")
  operation
