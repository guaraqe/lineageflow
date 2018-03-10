module FilterBigger
  ( algorithm
  ) where

import LineageFlow.Prelude
import LineageFlow.Algorithm
import LineageFlow.IO.CBOR

import LineageFlow.Statistics.Filter

--------------------------------------------------------------------------------

data Parameter = Parameter
  { p_bound
      :: Double
      :% Single
      :? "Minimum value to be accepted by the filtering."
  } deriving Generic

instance P Parameter
instance PD Parameter

--------------------------------------------------------------------------------

data Input = Input
  { i_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Contacts between cells."
  , i_measurement
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Measurement to be used as criterium."
  } deriving Generic

instance I CBORGet Input
instance MD Input

--------------------------------------------------------------------------------

data Output = Output
  { o_contacts
      :: DSumMapL Time (DS2 Time Cell) (DS2 Time Cell)
      :% Single
      :? "Filtered contacts."
  , o_measurement
      :: DSumMapL Time (DS2 Time Cell) Scalar
      :% Single
      :? "Filtered measurement."
  } deriving Generic

instance O CBORPut Output
instance MD Output

--------------------------------------------------------------------------------

operation :: Parameter -> Input -> Output
operation
  (Parameter (S b))
  (Input (S tri) (S sca)) =
  (Output (S triF) (S scaF))
  where
    (triF, scaF) =
      bimap Compose Compose . _unzip $
        _zipWith (nanFilterDomainWith (> b))
          (getCompose tri) (getCompose sca)

algorithm :: Algorithm Parameter Input Output
algorithm =
  makeAlgorithm
    "Contact filtering: Keep larger"
    $(embed "desc/filter-bigger.md")
    operation
