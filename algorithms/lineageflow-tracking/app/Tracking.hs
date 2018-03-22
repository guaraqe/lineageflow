module Tracking
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR
import LineageFlow.Tracking

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_tracking
      :: TCMap Tracking
      :% Single
      :? "Cell tracking."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_mothers
      :: Mothers
      :% Single
      :? "Mapping from cells to their mothers."
  , o_children
      :: Children
      :% Single
      :? "Mapping from cells to their children."
  , o_tc
      :: TC
      :% Single
      :? "Conversor between temporal and cellular point of view."
  , o_ct
      :: CT
      :% Single
      :? "Conversor between cellular and temporal point of view."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S tracking)) =
  Output
    (S mothers)
    (S children)
    (S tc)
    (S ct)
  where
    LineageSet mothers children tc ct =
      lineageSetFromTracking tracking

algorithm :: Algorithm Parameter Input Output
algorithm = makeAlgorithm
  "Temporal lineage generator"
  $(embed "desc/tracking.md")
  operation
