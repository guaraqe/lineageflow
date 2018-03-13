module LineageFlow.Prelude.PointsOfView
  ( TCMap
  , CTMap
  ) where

import LineageFlow.Types
import LineageFlow.Prelude.Containers

type TCMap a = DSumMap List Time Array Cell a
type CTMap a = DSumMap List Cell Array Time a
