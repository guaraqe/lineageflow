module TimeFlatVector
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Homogenization.Time

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
  { i_measurement
      :: TCMap Vector
      :% Single
      :? "Vector measurement to be homogenized."
  , i_tc
      :: DSumTL Time Cell
      :% Single
      :? "Conversion from temporal to cellular point of view."
  , i_ct
      :: DSumTL Cell Time
      :% Single
      :? "Conversion from cellular to temporal point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_homogenized
      :: TCMap Vector
      :% Single
      :? "Homogenized measurement."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  (Parameter (S size))
  (Input (S measure) (S tc) (S ct)) =
  Output (S hom)
  where
    hom =
      timeHomogenizeVector tc ct (flat size) measure

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Temporal flat homogenization of vector measurements"
  $(embed "desc/time-flat-vector.md")
  operation
