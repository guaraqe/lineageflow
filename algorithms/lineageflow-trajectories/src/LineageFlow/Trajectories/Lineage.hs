module LineageFlow.Trajectories.Lineage
  ( childrenToLineage
  , makeTrajectories
  ) where

import LineageFlow.Prelude hiding (Last)
import qualified LineageFlow.ArrayU as ArrayU
import LineageFlow.Trajectories.Types

import Control.Lens (over)

-- Creation function that automatically gives the good depth for each children
children :: a -> p -> p -> Lineage p a -> Lineage p a -> Lineage p a
children n p q c1 c2 = LineageSplit n (1 + max (depth c1) (depth c2)) p q c1 c2

-- Function that extracts the depth counter
depth :: Lineage p a -> Int
depth (LineageEnd _) = 1
depth (LineageSplit _ d _ _ _ _) = d

-- Converts children vector to genealogy
childrenToLineage ::
  Array Cell (Maybe (Cell,Cell)) -> Cell -> Lineage (Product Double) Cell
childrenToLineage c n = case c ! n of
    Nothing ->
      LineageEnd n
    Just (l,r) ->
      children
        n
        (Product 0.5)
        (Product 0.5)
        (childrenToLineage c l)
        (childrenToLineage c r)

makeTrajectories ::
  CT -> CTMap a -> List Cell (Trajectory Cell Time a)
makeTrajectories (Compose ct) (Compose measure) =
  _zipWith putInterval ct measure
  where
    putInterval ct' measure' = Trajectory t0 t1 measure'
      where
        t0 = fst . ArrayU.head . unIx $ ct'
        t1 = over time (+1) $ fst . ArrayU.last . unIx $ ct'
