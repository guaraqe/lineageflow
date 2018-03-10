module Lanczos
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Derivatives.Lanczos

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_filterSize
      :: Int
      :% Single
      :? "Filter size."
  , p_derivativeOrder
      :: Int
      :% Single
      :? "Order of the derivative to be calculated."
  , p_smoothingOrder
      :: Int
      :% Single
      :? "Order of the polynomial used for smoothing."
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
  (Parameter (S n) (S r) (S p))
  (Input (S measure) (S tc) (S ct)) =
  Output (S derivative)
  where
    derivative = tcDeriveV tc ct (lanczos r n 0 p) measure

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Lanczos derivative"
  $(embed "desc/lanczos.md")
  operation
