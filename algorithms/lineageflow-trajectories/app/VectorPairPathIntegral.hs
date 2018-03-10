module VectorPairPathIntegral
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Trajectories.PathIntegral

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { args_children
      :: Children
      :% Single
      :? "Cells' children."
  , args_ct
      :: CT
      :% Single
      :? "Converter from cell to time view."
  , args_measure
      :: TCMap Vector
      :% Single
      :? "Vector measurement to be integrated."
  , args_pairs
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Pairs of cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_integral
      :: Array (S2 Cell) Scalar
      :% Single
      :? "Integrated measurement."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  Parameter (Input (S children) (S ct) (S measurement') (S pairs)) =
  Output (S integral')
  where
    measurement = t2c ct measurement'
    integral = pathPairIntegral norm children ct measurement pairs
    integral' = _fmap (fromMaybe (0/0)) integral

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Pair path integral of scalar mesurements"
    $(embed "desc/vector.md")
    operation

