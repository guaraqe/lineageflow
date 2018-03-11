module Sisters
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Deviations.Algorithm

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { args_position
      :: DSumMap BArray Time Array Cell Vector
      :% Single
      :? "Cells' positions."
  , args_children
      :: Children
      :% Single
      :? "Mapping of cells to children."
  , args_ct
      :: DSumMap BArray Cell Array Time (Time, Dep Time Cell)
      :% Single
      :? "Converter from cellular to temporal point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { output_deviation
      :: TCCMap Vector
      :% Single
      :? "Relative displacement between sisters."
  , output_increments
      :: TCCMap Vector
      :% Single
      :? "Relative increments between sisters."
  , output_msd
      :: TCCMap Scalar
      :% Single
      :? "MSD with time average."
  , output_msd0
      :: TCCMap Scalar
      :% Single
      :? "MSD without time average."
  , output_autocorrelation
      :: TCCMap Scalar
      :% Single
      :? "Autocorrelation with time average."
  , output_autocorrelation0
      :: TCCMap Scalar
      :% Single
      :? "Autocorrelation without time average"
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  Parameter
  (Input (S pos) (S children) (S ct)) =
  Output (S disp) (S incr) (S msd) (S msd0) (S auto) (S auto0)
  where
    (disp, incr, msd, msd0, auto, auto0) = result ct pos (sisters children)

--------------------------------------------------------------------------------

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Deviation of Sister Cells"
    $(embed "desc/sisters.md")
    operation
