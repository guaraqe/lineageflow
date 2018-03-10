module Clusters
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR
import LineageFlow.Clustering.Clusters
import LineageFlow.Clustering.Eigenvectors

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_number
      :: Int
      :% Single
      :? "Range of clusters to be found."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: Array (S2 Cell) (S2 Cell)
      :% Single
      :? "Cell contacts."
  , i_similarity
      :: Array (S2 Cell) Scalar
      :% Single
      :? "Dissimilarities."
  , i_mothers
      :: Mothers
      :% Single
      :? "Mothers."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_clusters
      :: Array Cell Int
      :% Many
      :? "Calculated clusters."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation (Parameter (S n)) (Input (S ix) (S vals) (S m)) = Output (M c)
  where
    dim = _length m
    e = eigenvectors dim n ix vals
    c = fmap (\k -> spectralClustering k e) [2 .. n]

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Clustering calculation"
    $(embed "desc/clusters.md")
    operation

