{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Trajectories.Types
  ( Lineage (..)
  , Trajectory (..)
  , trajectory_start
  , trajectory_end
  , trajectory_data
  , Weighted (..)
  , WD
  ) where

import LineageFlow.Prelude hiding ((<>))
import Data.Semigroup
import Control.DeepSeq
import Control.Lens

-- The entries are: ID, depth, left probability, right probability, left and right
data Lineage p a =
  LineageEnd !a |
  LineageSplit !a !Int !p !p (Lineage p a) (Lineage p a)

instance (NFData p, NFData a) => NFData (Lineage p a) where
  rnf (LineageEnd x) = rnf x
  rnf (LineageSplit x i p1 p2 g1 g2) =
    rnf x `seq` rnf i `seq` rnf p1 `seq` rnf p2 `seq` rnf g1 `seq` rnf g2

--------------------------------------------------------------------------------

-- | Trajectories composed of a half open interval and the data.
data Trajectory c t a = Trajectory
 { _trajectory_start :: !t
 , _trajectory_end :: !t
 , _trajectory_data :: !(Array (Dep c t) a)
 }

$(makeLenses ''Trajectory)

--------------------------------------------------------------------------------

data Weighted k a = Weighted !k !a
  deriving (Show, Eq)

instance Functor (Weighted k) where
  fmap f (Weighted k a) = Weighted k (f a)

type WD = Weighted (Sum Double) (Sum Double)

instance (Semigroup k, Semigroup a) => Semigroup (Weighted k a) where
  (Weighted w1 x1) <> (Weighted w2 x2) = Weighted (w1 <> w2) (x1 <> x2)

instance (Monoid k, Monoid a) => Monoid (Weighted k a) where
  mempty = Weighted mempty mempty
  mappend (Weighted w1 x1) (Weighted w2 x2) =
    Weighted (mappend w1 w2) (mappend x1 x2)
