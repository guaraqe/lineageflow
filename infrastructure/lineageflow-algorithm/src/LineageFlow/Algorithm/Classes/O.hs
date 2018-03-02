module LineageFlow.Algorithm.Classes.O
  (
  -- * Output
    O (..)
  ) where

import LineageFlow.Algorithm.Help

import LineageFlow.IO
import LineageFlow.Declaration

import qualified Data.Text as Text
import GHC.Generics

--------------------------------------------------------------------------------

type QueryMap = Assoc (CardF FilePath)

-- | Class for putting measurements from input.
class O kp a where
  oPut :: IOMethod kg kp -> QueryMap -> a -> IO ()
  default oPut :: (Generic a, OG kp (Rep a)) => IOMethod kg kp -> QueryMap -> a -> IO ()
  oPut db qm m = oPutG db qm (from m)

--------------------------------------------------------------------------------

class MPut kp f where
  mPut :: IOMethod kg kp -> CardF FilePath -> f a -> IO ()

instance kp f a => MPut kp (K1 i (f a :% s :? d)) where
  mPut db q (K1 (Help (CardW m))) = mapM_ (uncurry $ io_put db) (zipCardF q m)

--------------------------------------------------------------------------------

class OG kp f where
  oPutG ::
    IOMethod kg kp -> QueryMap -> f a -> IO ()

instance (OG kp f, OG kp g) => OG kp (f :*: g) where
  oPutG db qm (x :*: y) = do
    oPutG db qm x
    oPutG db qm y

instance OG kp f => OG kp (M1 C p f) where
  oPutG db qm (M1 m) = oPutG db qm m

instance OG kp f => OG kp (M1 D p f) where
  oPutG db qm (M1 m) = oPutG db qm m

instance (Selector t, MPut kp f) => OG kp (M1 S t f) where
  oPutG db qm m1@(M1 m) = got
    where
      name = selName m1
      got = case lookupAssoc (Text.pack (convertField name)) qm of
          Nothing -> error $ "Output measure " ++ name ++ " not given."
          Just q -> mPut db q m

--------------------------------------------------------------------------------

zipCardF :: CardF a -> CardF b -> CardF (a,b)
zipCardF (SingleF x) (SingleF y) = SingleF (x,y)
zipCardF (OptionalF x) (OptionalF y) = OptionalF ((,) <$> x <*> y)
zipCardF (ManyF lx) (ManyF ly) = ManyF (zip lx ly)
zipCardF _ _ = error "LineageFlow.Algorithm.Output.zipCardF: Shape mismatch."
