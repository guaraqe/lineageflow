module Simple
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Derivatives.Types
import LineageFlow.Derivatives.Simple

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
      :? "Vector measurement to be derived."
  , i_tc
      :: TC
      :% Single
      :? "Converter from temporal to cellular point of view."
  , i_ct
      :: CT
      :% Single
      :? "Converter from cellular to temporal point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_derived
      :: TCMap Vector
      :% Single
      :? "Derived measurement."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  (Parameter (S size))
  (Input (S measure) (S tc) (S ct)) =
  Output (S derivative)
  where
    derivative = tcDeriveV tc ct (simpleFw size) measure

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Simple derivative"
  $(embed "desc/simple.md")
  operation
