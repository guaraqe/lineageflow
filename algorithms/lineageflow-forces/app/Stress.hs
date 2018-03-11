module Stress
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Forces.Coarse

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_sigma :: Double :% Single :? "Coarse graining radius."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts between cells."
  , i_forces
      :: DSumMapL Time (DS2 Time Cell) Vector
      :% Single
      :? "Forces between cells for each contact."
  , i_position
      :: TCMap Vector
      :% Single
      :? "Position of cells."
  , i_velocity
      :: TCMap Vector
      :% Single
      :? "Velocity of cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_stress
      :: DSumMapL Time Cell Tensor
      :% Single
      :? "Estimated stress tensors."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (S sigma)) (Input (S nei) (S for) (S pos) (S vel)) = (Output (S stress))
  where
    stress = stressTensor sigma nei for pos vel

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Stress tensor"
    $(embed "desc/stress.md")
    operation

