module LineageFlow.Prelude.Lineage
  ( TC
  , CT
  , Mothers
  , Children
  ) where

import BasePrelude
import LineageFlow.Types
import LineageFlow.Prelude.Containers
import LineageFlow.Prelude.PointsOfView

type TC = TCMap (Cell, Dep Cell Time)
type CT = CTMap (Time, Dep Time Cell)

type Mothers = Array Cell (Maybe Cell)
type Children = Array Cell (Maybe (Cell,Cell))
