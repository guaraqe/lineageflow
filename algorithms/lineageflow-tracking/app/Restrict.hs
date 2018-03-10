module Restrict
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.IO.CBOR
import LineageFlow.Algorithm
import LineageFlow.Tracking

import qualified LineageFlow.ArrayU as ArrayU

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_first
      :: Int
      :% Single
      :? "First time step, starting from 0."
  , p_last
      :: Int
      :% Single
      :? "Last time step."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_tracking
      :: TCMap Tracking
      :% Single
      :? "Cell tracking."
  , i_voxelPosition
      :: TCMap Vector
      :% Single
      :? "Cell position in voxel coordinates."
  , i_position
      :: TCMap Vector
      :% Single
      :? "Cell position."
  , i_time
      :: Array Time Time
      :% Single
      :? "Absolute time."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_restrictedTracking
      :: TCMap Tracking
      :% Single
      :? "Restricted cell tracking."
  , o_restrictedVoxelPosition
      :: TCMap Vector
      :% Single
      :? "Restricted cell position in voxel coordinates."
  , o_restrictedPosition
      :: TCMap Vector
      :% Single
      :? "Restricted cell position."
  , o_restrictedTime
      :: Array Time Time
      :% Single
      :? "Restricted absolute time."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (S t0) (S t1)) (Input (S t) (S v) (S p) (S a)) = Output (S rt) (S rv) (S rp) (S ra)
  where
    tF = t1 - t0 + 1
    rt = over (composed . ixed) (take tF . drop t0) t
    rv = over (composed . ixed) (take tF . drop t0) v
    rp = over (composed . ixed) (take tF . drop t0) p
    ra = over ixed (ArrayU.take tF . ArrayU.drop t0) a

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Restriction of cell trackings"
  $(embed "desc/restrict.md")
  operation
