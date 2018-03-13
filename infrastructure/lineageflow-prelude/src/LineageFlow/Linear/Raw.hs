module LineageFlow.Linear.Raw
  (
  module X
  ) where

import LineageFlow.ArrayU as X
import LATS.Matrix.Raw as X hiding
  ( Vector
  , toList
  , (!)
  , accum
  , find
  , maxIndex
  , minIndex
  )
