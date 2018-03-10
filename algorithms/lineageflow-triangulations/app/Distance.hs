module Distance
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
  { i_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts between cells."
  , i_position
      :: TCMap Vector
      :% Single
      :? "Position of cells."

  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_distances
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Distances."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation Parameter (Input (S tri) (S pos)) = (Output (S o))
  where
    o =
      Compose $
      _zipWith
        (\t p -> _fmap (\(S2 i j) -> norm (p ! i - p ! j)) t)
        (getCompose tri)
        (getCompose pos)

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Contact Distances"
    $(embed "desc/distance.md")
    operation
