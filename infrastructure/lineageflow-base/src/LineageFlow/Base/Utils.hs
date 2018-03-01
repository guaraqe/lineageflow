module LineageFlow.Base.Utils
  (
  -- * Association lists
    Assoc (..)
  , lookupAssoc
  -- * Helpers for Aeson instances.
  , convertField
  , convertConstructor
  ) where

import LineageFlow.Base.Imports
import Data.Char
import Data.Semigroup
import qualified Data.HashMap.Strict as HashMap

--------------------------------------------------------------------------------

-- | Converts record field names.  Removes everything before underscores.
convertField :: String -> String
convertField =
  concatMap (\c -> if isLower c then [c] else ['-',toLower c] ) . go []
  where
    go y [] = reverse y
    go y (x:xs) =
      if x == '_'
        then go [] xs
        else go (x:y) xs

-- | Converts constructors by transforming camel case to dashed style.
convertConstructor :: String -> String
convertConstructor =
  tail .
  concatMap (\c -> if isLower c then [c] else ['-',toLower c] )

--------------------------------------------------------------------------------

-- | Association lists.
newtype Assoc a = Assoc { getAssoc :: [(Text,a)] }
  deriving (Show, Eq, Functor, Foldable, Traversable, Semigroup, Monoid)

instance ToJSON a => ToJSON (Assoc a) where
  toJSON = toJSON . HashMap.fromList . getAssoc
  toEncoding = pairs . foldMap f . getAssoc
    where
      f (t,v) = t .= v

instance FromJSON a => FromJSON (Assoc a) where
  parseJSON v = Assoc . HashMap.toList <$> parseJSON v

-- | Lookups an element in an association list by name.
lookupAssoc :: Text -> Assoc a -> Maybe a
lookupAssoc x = lookup x . getAssoc
