module LineageFlow.Algorithm.Classes.I
  (
  -- * Inputs
    I (..)
  ) where

import LineageFlow.Algorithm.Help

import LineageFlow.IO
import LineageFlow.Declaration

import qualified Data.Text as Text
import GHC.Generics

import System.IO.Unsafe

--------------------------------------------------------------------------------

type QueryMap = Assoc (CardF FilePath)

-- | Class for getting measurements from input.
class I kg a where
  iGet :: IOMethod kg kp -> QueryMap -> IO a
  default iGet :: (Generic a, IG kg (Rep a)) => IOMethod kg kp -> QueryMap -> IO a
  iGet db qm = to <$> iGetG db qm

--------------------------------------------------------------------------------

class MGet kg f where
  mGetG :: IOMethod kg kp -> CardF FilePath -> IO (f a)

instance kg f a => MGet kg (K1 i (f a :% s :? d)) where
  mGetG db q = K1 . Help . CardW <$> mapM (io_get db) q

--------------------------------------------------------------------------------

class IG kg f where
  iGetG ::
    IOMethod kg kp -> QueryMap -> IO (f a)

instance (IG kg f, IG kg g) => IG kg (f :*: g) where
  iGetG db qm = (:*:) <$> iGetG db qm <*> iGetG db qm

instance IG kg f => IG kg (M1 C p f) where
  iGetG db qm = M1 <$> iGetG db qm

instance IG kg f => IG kg (M1 D p f) where
  iGetG db qm = M1 <$> iGetG db qm

instance (Selector t, MGet kg f) => IG kg (M1 S t f) where
  iGetG db qm = got
    where
      name = selName (unsafePerformIO got)
      got =
        case lookupAssoc (Text.pack (convertField name)) qm of
          Nothing -> error $ "Input measure " ++ name ++ " not given."
          Just q -> M1 <$> mGetG db q
