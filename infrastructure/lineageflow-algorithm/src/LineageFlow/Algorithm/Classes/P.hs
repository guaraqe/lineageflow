module LineageFlow.Algorithm.Classes.P
  (
  -- * Parameters
    P (..)
  ) where

import LineageFlow.Algorithm.Imports
import LineageFlow.Algorithm.Help

import LineageFlow.Declaration

import qualified Data.Text as Text

class PRead f where
  pRead :: CardF Text -> f a

instance Read a => PRead (K1 i (a :% s :? d)) where
  pRead p =
    K1 $ Help $ CardW $ fmap (read . Text.unpack) p

--------------------------------------------------------------------------------

type ParMap = Assoc (CardF String)

-- | Class for getting parameters from input.
class P a where
  pGet :: ParMap -> a
  default pGet :: (Generic a, PG (Rep a)) => ParMap -> a
  pGet pm = to $ pGetG pm

class PG f where
  pGetG :: ParMap -> f a

instance PG U1 where
  pGetG _ = U1

instance (PG f, PG g) => PG (f :*: g) where
  pGetG qm = pGetG qm :*: pGetG qm

instance PG f => PG (M1 C p f) where
  pGetG qm = M1 $ pGetG qm

instance PG f => PG (M1 D p f) where
  pGetG qm = M1 $ pGetG qm

instance (Selector t, PRead f) => PG (M1 S t f) where
  pGetG qm = got
    where
      name = selName got
      got =
        case lookupAssoc (Text.pack (convertField name)) qm of
          Nothing -> error $ "Parameter " ++ name ++ " not given."
          Just q -> M1 $ pRead (fmap Text.pack q)
