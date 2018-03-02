{-# LANGUAGE PatternSynonyms #-}

module LineageFlow.Algorithm.Help
  (
  -- * Help
    Help (..)
  , (:?)
  , CardW (..)
  , (:%)
  , pattern S
  , pattern O
  , pattern M
  ) where

import LineageFlow.Algorithm.Imports
import LineageFlow.Declaration

--------------------------------------------------------------------------------

-- | Constructor for adding a description as a phantom type.
data Help (d :: Symbol) t = Help t
  deriving (Show, Generic, Functor, Foldable, Traversable)

type (:?) t d = Help d t

--------------------------------------------------------------------------------

data CardW a (b :: Card) = CardW { cardW :: CardF a }
  deriving (Show, Eq)

type (:%) a b = CardW a b

--------------------------------------------------------------------------------

instance FromJSON t => FromJSON (Help d t)
instance ToJSON t => ToJSON (Help d t)

pattern S :: a -> a :% b :? c
pattern S a = Help (CardW (SingleF a))

pattern O :: Maybe a -> a :% b :? c
pattern O a = Help (CardW (OptionalF a))

pattern M :: [a] -> a :% b :? c
pattern M a = Help (CardW (ManyF a))
