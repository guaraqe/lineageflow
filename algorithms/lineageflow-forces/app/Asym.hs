module Asym
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
  { args_stresses
      :: DSumMapL Time Cell Tensor
      :% Single
      :? "Stress tensor of cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_forcesNorm
      :: DSumMapL Time Cell Vector
      :% Single
      :? "Asymmetric component of stress tensors."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S for)) = (Output (S pre))
  where
    pre = _fmap asym for

asym :: Tensor -> Vector
asym (V3 (V3 _ xy xz) (V3 yx _ yz) (V3 zx zy _)) =
  V3 ((zy - yz)/2) ((xz - zx)/2) ((yx - xy)/2)

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Asymmetric Component of Stress Tensors"
    $(embed "desc/asym.md")
    operation

