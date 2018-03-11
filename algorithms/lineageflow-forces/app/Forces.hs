module Forces
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Forces

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts between cells."
  , i_connexity
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Connexity factor for each contact."
  , i_acceleration
      :: TCMap Vector
      :% Single
      :? "Acceleration of cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_forces
      :: DSumMapL Time (DS2 Time Cell) Vector
      :% Single
      :? "Estimated forces."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S nei) (S con) (S acc)) = (Output (S forces))
  where
    forces =
      Compose $ _zipWith3 forcesLeastSquares (getCompose nei) (getCompose con) (getCompose acc)

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Force Inference"
    $(embed "desc/forces.md")
    operation

