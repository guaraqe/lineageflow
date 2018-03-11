module Pression
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
  { args_contacts
      :: DSumMapL Time Array TContact TContact
      :% Single
      :? "Single args"
  , args_forces
      :: (List Time :.: Array TContact) Vector
      :% Single
      :? "List of argss"
  , args_position
      :: TCMap Vector
      :% Single
      :? "Non-empty list of argss"
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_pression
      :: (List Time :.: Array TContact) Scalar
      :% Single
      :? "Single output"
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S nei) (S for) (S pos)) = (Output (S pre))
  where
    pre = forceToPressionTC pos nei for

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Calculation of pressions from forces"
    $(embed "descriptions/algorithm.md")
    operation

