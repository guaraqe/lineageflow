{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module LineageFlow.Base.Card
  (
  -- * Cardinalities
    Card (..)
  -- * Containers for different cardinalities
  , CardF (..)
  -- * Prims for @CardF@
  , _SingleF
  , _OptionalF
  , _ManyF
  ) where

import LineageFlow.Base.Imports
import Data.Char (toLower)

--------------------------------------------------------------------------------

-- | @Card@ describe the possible cardinalities supported here.  This type is
-- just used in type-level definitions for the declaration of algorithms in
-- executables.
data Card = Single | Optional | Many
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Card where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = helper }

instance FromJSON Card where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = helper }

helper :: String -> String
helper = fmap toLower

--------------------------------------------------------------------------------

-- | Constructors for different cardinalities. They may contain single
-- elements, optional elements or lists of elements.
data CardF a =
  SingleF a |
  OptionalF (Maybe a) |
  ManyF [a]
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

$(makePrisms ''CardF)

instance Applicative CardF where
  pure = SingleF
  (SingleF f) <*> (SingleF x) = SingleF (f x)
  (SingleF f) <*> (OptionalF x) = OptionalF (fmap f x)
  (SingleF f) <*> (ManyF x) = ManyF (fmap f x)
  (OptionalF f) <*> (SingleF x) = OptionalF (f <*> pure x)
  (OptionalF f) <*> (OptionalF x) = OptionalF (f <*> x)
  (OptionalF f) <*> (ManyF x) = ManyF (maybe [] pure f <*> x)
  (ManyF f) <*> (SingleF x) = ManyF (f <*> pure x)
  (ManyF f) <*> (OptionalF x) = ManyF (f <*> maybe [] pure x)
  (ManyF f) <*> (ManyF x) = ManyF (f <*> x)

instance Monad CardF where
  return = pure
  SingleF x >>= f = f x
  OptionalF Nothing >>= _ = OptionalF Nothing
  OptionalF (Just x) >>= f = f x
  ManyF x >>= f = toMany (traverse f x)
    where
      toMany (SingleF a) = ManyF a
      toMany (OptionalF Nothing) = ManyF []
      toMany (OptionalF (Just a)) = ManyF a
      toMany (ManyF a) = ManyF (concat a)

instance ToJSON a => ToJSON (CardF a) where
  toJSON = genericToJSON $
    defaultOptions { constructorTagModifier = init . helper }

instance FromJSON a => FromJSON (CardF a) where
  parseJSON = genericParseJSON $
    defaultOptions { constructorTagModifier = init . helper }

--------------------------------------------------------------------------------

{-
data WithCard (card :: Card) a where
  WithSingle :: a -> WithCard 'Single a
  WithOptional :: Maybe a -> WithCard 'Optional a
  WithMany :: [a] -> WithCard 'Many a

getFromCard :: WithCard card a -> Maybe a
getFromCard (WithSingle a) = Just a
getFromCard (WithOptional a) = a
getFromCard (WithMany []) = Nothing
getFromCard (WithMany (a:_)) = Just a

data SomeCard f a =
  SomeSingle (f 'Single a) |
  SomeOptional (f 'Optional a) |
  SomeMany  (f 'Many a)
-}

--------------------------------------------------------------------------------
