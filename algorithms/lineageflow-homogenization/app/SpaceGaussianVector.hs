module SpaceGaussianVector
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Homogenization.Space

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_filterSize
      :: Double
      :% Single
      :? "Filter size."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_position
      :: TCMap Vector
      :% Single
      :? "Position of cells."
  , i_measurement
      :: TCMap Vector
      :% Single
      :? "Vector measurement to be homogenized."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_homogenized
      :: TCMap Vector
      :% Single
      :? "Homogenized measurement"
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (S size)) (Input (S position) (S measure)) = Output (S hom)
  where
    hom =
      spaceHomogenizeVector (spaceGaussianNormalizedDense size) position measure

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Spatial gaussian homogenization of vector measurements"
  $(embed "desc/space-gaussian-vector.md")
  operation
