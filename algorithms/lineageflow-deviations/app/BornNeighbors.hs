module BornNeighbors
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Deviations.Algorithm
import LineageFlow.Deviations.Neighbors

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_radius
      :: Double
      :% Single
      :? "Radius around cells."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_position
      :: DSumMap BArray Time Array Cell Vector
      :% Single
      :? "Cells' positions."
  , i_children
      :: Children
      :% Single
      :? "Mapping from cells to children."
  , i_tc
      :: DSumMap BArray Time Array Cell (Cell, Dep Cell Time)
      :% Single
      :? "Converter between temporal and cellular point of view."
  , i_ct
      :: DSumMap BArray Cell Array Time (Time, Dep Time Cell)
      :% Single
      :? "Converter between cellular and temporal point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_divergence
      :: TCCMap Vector
      :% Single
      :? "Relative displacements between neighbors."
  , o_increments
      :: TCCMap Vector
      :% Single
      :? "Relative increments between cells."
  , o_msd
      :: TCCMap Scalar
      :% Single
      :? "MSD with time average."
  , o_msd0
      :: TCCMap Scalar
      :% Single
      :? "MSD without time average."
  , o_autocorrelation
      :: TCCMap Scalar
      :% Single
      :? "Autocorrelation with time average."
  , o_autocorrelation0
      :: TCCMap Scalar
      :% Single
      :? "Autocorrelation without time average."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  (Parameter (S d))
  (Input (S pos) (S children) (S tc) (S ct)) =
  Output (S disp) (S incr) (S msd) (S msd0) (S auto) (S auto0)
  where
   pairs = neighborsList tc ct pos d (bornCells children)
   (disp, incr, msd, msd0, auto, auto0) = result ct pos pairs

--------------------------------------------------------------------------------

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Deviation of Neighbors from Divided Cells"
    $(embed "desc/born.md")
    operation
