module Tension
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Database.CBOR

import LineageFlow.Forces

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: (List Time :.: Array TContact) TContact
      :% Single
      :? "Single i"
  , i_forces
      :: (List Time :.: Array TContact) Vector
      :% Single
      :? "List of is"
  , i_position
      :: TCMap Vector
      :% Single
      :? "Non-empty list of is"
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_tension
      :: (List Time :.: Array TContact) Scalar
      :% Single
      :? "Single o"
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S nei) (S for) (S pos)) = (Output (S pre))
  where
    pre = forceToTensionTC pos nei for

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Calculation of pressions from forces"
    $(embed "descriptions/algorithm.md")
    operation

