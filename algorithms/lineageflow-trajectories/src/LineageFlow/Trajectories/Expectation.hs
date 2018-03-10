{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Trajectories.Expectation
  ( measureAverage
  , measureAverageIntegral
  ) where

import LineageFlow.Prelude

import LineageFlow.Trajectories.Types
import LineageFlow.Trajectories.PathSpace
import qualified LineageFlow.ArrayU as ArrayU

-- Concatenate two trajectories if consecutive, otherwise, returns Nothing
concatenate ::
  (Eq t, Storable a) =>
  Trajectory c t a -> Trajectory c t a -> Maybe (Trajectory c t a)
concatenate (Trajectory start1 end1 data1) (Trajectory start2 end2 data2) =
  if end1 == start2
    then Just (Trajectory start1 end2 (mkIx (unIx data1 <> unIx data2)))
    else Nothing

-- Auxiliary function for concatenating lists of trajectories
mFun :: (a -> a -> Maybe a) -> [a] -> Maybe a
mFun _ [] = Nothing
mFun _ [x] = Just x
mFun f (x:xs) = f x =<< mFun f xs

-- Calculate the difference trajectory if it applies
difference ::
  (Ord t, Newtype t Int, Storable x, Storable y, Storable z) =>
  (x -> y -> z) -> Trajectory c t x -> Trajectory c t y -> Maybe (Trajectory (c,c) t z)
difference f (Trajectory start1 end1 vec1) (Trajectory start2 end2 vec2) =
  if start >= end
    then Nothing
    else Just (Trajectory (pack start) (pack end) diff)
  where
    diff =
      _zipWith
        f
        (mkIx . ArrayU.take len . ArrayU.drop del1 . unIx $ vec1)
        (mkIx . ArrayU.take len . ArrayU.drop del2 . unIx $ vec2)
    start = max (unpack start1) (unpack start2)
    end   = min (unpack end1) (unpack end2)
    del1  = start - unpack start1
    del2  = start - unpack start2
    len   = end - start

-- Auxiliary function for pairs of Maybes (pm)
pmApply :: (a -> a -> Maybe b) -> Weighted p (Maybe a, Maybe a) -> Weighted p (Maybe b)
pmApply f (Weighted p (x, y)) = Weighted p . join $ f <$> x <*> y

-- Auxiliary function for probs of pairs (pp)
ppApply :: (a -> b) -> Weighted p (a, a) -> Weighted p (b, b)
ppApply f = fmap (bimap f f)

-- Given the trajectory list and a Path Pairs Space, get Trajectory Pairs Space
trajList ::
  (Eq t, Storable a, Newtype i Int) =>
  BArray i (Trajectory c t a) ->
  [Weighted p ([i],[i])] ->
  [Weighted p (Maybe (Trajectory c t a), Maybe (Trajectory c t a))]
trajList lt = map (ppApply (mFun concatenate . map (\i -> lt ! i)))

-- Given a measure between trajectories, a list of trajectories and a Path
-- Pairs Space, calculate the average
measureAverage' ::
  (Eq t, Storable a, Newtype i Int, Monoid p, Monoid m) =>
  (p -> m -> m) ->
  (p -> m -> m) ->
  (Trajectory c t a -> Trajectory c t a -> Maybe m) ->
  BArray i (Trajectory c t a) ->
  [Weighted p ([i],[i])] ->
  Maybe m
measureAverage' times divis distance lt =
  pAverage times divis . map (pmApply distance) . trajList lt

measureAverage ::
  (Num a, Ord t, Storable a, Newtype t Int, Newtype i Int, Eq i, Monoid p, Monoid m) =>
  (p -> m -> m) ->
  (p -> m -> m) ->
  (Trajectory (S2 c) t a -> Maybe m) ->
  BArray i (Trajectory c t a) ->
  BArray i (Lineage p i) ->
  (S2 i) ->
  Maybe m
measureAverage times divis f lt lg (S2 n m) =
  measureAverage' times divis f' lt (historyPairSpace (lg ! n) (lg ! m))
  where f' t1 t2 = f =<< fmap (over trajectory_data mvIx) (difference (-) t1 t2)

measureAverageIntegral ::
  (Num a, Ord t, Storable a, Newtype t Int, Newtype i Int, Eq i) =>
  (Trajectory (S2 c) t a -> Maybe (Weighted (Sum Double) (Sum Double))) ->
  BArray i (Trajectory c t a) ->
  BArray i (Lineage (Product Double) i) ->
  S2 i ->
  Maybe (Weighted (Sum Double) (Sum Double))
measureAverageIntegral f =
  measureAverage
    (\(Product p) (Weighted (Sum k) (Sum a)) ->
        Weighted (Sum (k * p)) (Sum (a * p)))
    (\(Product p) (Weighted (Sum k) (Sum a)) ->
        Weighted (Sum (k / p)) (Sum (a / p)))
    f

-- Auxiliary function for Maybe averages
pAverage ::
  ( Monoid p
  , Monoid a
  ) =>
  (p -> a -> a) ->
  (p -> a -> a) ->
  [Weighted p (Maybe a)] ->
  Maybe a
pAverage times divis l = do
    w <- weightedMaybeCombine times l
    p <- weightMaybe l
    return (divis p w)

--------------------------------------------------------------------------------

filterJust :: [Weighted p (Maybe a)] -> Maybe [Weighted p a]
filterJust list =
  case result of
    [] -> Nothing
    l -> Just l
  where
    result = foldr (\(Weighted p a) ->
      case a of
        Nothing -> id
        Just x -> (Weighted p x:)) [] list

-- Auxiliary function for Maybe probs
weightMaybe :: Monoid p => [Weighted p (Maybe t)] -> Maybe p
weightMaybe lp = weight <$> filterJust lp

weight :: Monoid p => [Weighted p t] -> p
weight = foldMap (\(Weighted p _) -> p)

-- Auxiliary function for Maybe sums
weightedMaybeCombine :: Monoid a => (p -> a -> a) -> [Weighted p (Maybe a)] -> Maybe a
weightedMaybeCombine f lp = weightedCombine f <$> filterJust lp

weightedCombine :: Monoid a => (p -> a -> a) -> [Weighted p a] -> a
weightedCombine f = foldr (\(Weighted p a) b -> f p a <> b) mempty
