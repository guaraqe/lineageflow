module Norm
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_forces
      :: DSumMapL Time (DS2 Time Cell) Vector
      :% Single
      :? "Forces between cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_forcesNorm
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Norm of forces between cells."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S for)) = (Output (S pre))
  where
    pre = _fmap norm for

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Absolute value of forces."
    $(embed "desc/norm.md")
    operation
