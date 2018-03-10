module Merge
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Triangulations.Neighbors

import qualified Data.Set as Set

--------------------------------------------------------------------------------

data Parameter = Parameter
  deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: Array (S2 Cell) (S2 Cell)
      :% Many
      :? "Contacts between cells."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_mergedContacts
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Merged contacts."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (M tri)) = (Output (S o))
  where
    toSet = Set.fromList . toList
    fromSet = fromList . Set.toList
    o = fromSet . Set.unions . fmap toSet $ tri

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Merge contacts"
    $(embed "desc/merge.md")
    operation
