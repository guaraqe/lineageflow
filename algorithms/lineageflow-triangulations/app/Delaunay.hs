module Delaunay
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.Delaunay

import qualified Data.List as List (unzip)

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_positions
      :: TCMap Vector
      :% Single
      :? "Position of cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_delaunay
      :: DSumMapL Time (DS4 Time Cell) (DS4 Time Cell)
      :% Single
      :? "Delaunay tesselation."
  , o_orientation
      :: DSumMapL Time (DS4 Time Cell) Sign
      :% Single
      :? "Orientation of simplices."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S pos)) = (Output (S del) (S o))
  where
    (del', o') = List.unzip $ _fmap delaunay $ unIx $ getCompose pos
    del = Compose (mkIx del')
    o = Compose (mkIx o')

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Delaunay Tesselation"
    $(embed "desc/delaunay.md")
    operation
