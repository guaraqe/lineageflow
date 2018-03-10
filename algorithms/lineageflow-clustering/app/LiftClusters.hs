module LiftClusters
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
  { i_clusters
      :: BArray Cell Int
      :% Many
      :? "Clusters to be lifted."
  , i_tc
      :: DSumTA Time Cell
      :% Single
      :? "Converter from cell to time point of view."
  , i_ct
      :: DSumTA Cell Time
      :% Single
      :? "Converter from time to cell point of view."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_liftedClusters
      :: DSumMapA Time Cell Int
      :% Many
      :? "Lifted clusters."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (M c) (S tc) (S ct)) = Output (M lc)
  where
    lc' = fmap (Compose . _zipWith (\v x -> _fmap (const x) v) (getCompose ct)) c
    lc = fmap (dSumMapTranspose tc) lc'

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Lift clusters"
    $(embed "desc/lift-clusters.md")
    operation

