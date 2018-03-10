module LineageFlow.Trajectories.PathSpace
  ( historySpace
  , historyPairSpace ) where

import LineageFlow.Prelude
import LineageFlow.Trajectories.Types

-- Single path probability space
historySpace :: Monoid p => Lineage p a -> [Weighted p [a]]
historySpace (LineageEnd n) = [Weighted mempty [n]]
historySpace (LineageSplit n _ p q c1 c2) =
  fmap leftSide  (historySpace c1) <>
  fmap rightSide (historySpace c2)
  where
    leftSide  (Weighted r l) = Weighted (p <> r) (n : l)
    rightSide (Weighted r l) = Weighted (q <> r) (n : l)

-- Pairs of paths probability space
historyPairSpace ::
  (Monoid p, Eq a) =>
  Lineage p a -> Lineage p a -> [Weighted p ([a],[a])]
historyPairSpace (LineageEnd n) (LineageEnd m) = [Weighted mempty ([n],[m])]

historyPairSpace (LineageEnd n) (LineageSplit m _ p q c1 c2) =
  fmap leftSide  (historyPairSpace (LineageEnd n) c1) <>
  fmap rightSide (historyPairSpace (LineageEnd n) c2)
  where
    leftSide  (Weighted r (l1, l2)) = Weighted (p <> r) (l1, (m : l2))
    rightSide (Weighted r (l1, l2)) = Weighted (q <> r) (l1, (m : l2))

historyPairSpace (LineageSplit m _ p q c1 c2) (LineageEnd n) =
  fmap leftSide  (historyPairSpace c1 (LineageEnd n)) <>
  fmap rightSide (historyPairSpace c2 (LineageEnd n))
  where
    leftSide  (Weighted r (l1, l2)) = Weighted (p <> r) ((m : l1), l2)
    rightSide (Weighted r (l1, l2)) = Weighted (q <> r) ((m : l1), l2)

historyPairSpace (LineageSplit n s1 p q c1 c2) (LineageSplit m s2 r s d1 d2)
  | n == m =
      fmap (bothSides p)  (historyPairSpace c1 d1) <>
      fmap (bothSides q)  (historyPairSpace c2 d2)
  | s1 > s2 =
      fmap (leftSide p)  (historyPairSpace c1 (LineageSplit m s2 r s d1 d2)) <>
      fmap (leftSide q)  (historyPairSpace c2 (LineageSplit m s2 r s d1 d2))

  | otherwise =
      fmap (rightSide r) (historyPairSpace (LineageSplit n s1 p q c1 c2) d1) <>
      fmap (rightSide s) (historyPairSpace (LineageSplit n s1 p q c1 c2) d2)
  where
    bothSides p' (Weighted r' (l1, l2)) = Weighted (p' <> r') ((n : l1), (n : l2))
    leftSide  p' (Weighted r' (l1, l2)) = Weighted (p' <> r') ((n : l1), l2)
    rightSide p' (Weighted r' (l1, l2)) = Weighted (p' <> r') (l1, (m:l2))
