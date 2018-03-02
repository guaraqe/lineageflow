module LineageFlow.Algorithm.Classes.PD
  (
  -- * Param declarations
    PD (..)
  ) where

import LineageFlow.Algorithm.Imports
import LineageFlow.Algorithm.Help
import LineageFlow.Algorithm.Classes

import LineageFlow.Declaration

import qualified Data.Text as Text
import Data.Monoid
import Data.Proxy

--------------------------------------------------------------------------------

type ParMeta = Assoc (CardF PDecl)

class PD a where
  pDecl :: Proxy a -> ParMeta
  default pDecl :: (Generic a, PDG (Rep a)) => Proxy a -> ParMeta
  pDecl Proxy = pDeclG (Proxy @ (Rep a))

--------------------------------------------------------------------------------

class PDG (f :: * -> *) where
  pDeclG :: Proxy f -> ParMeta

instance PDG U1 where
  pDeclG Proxy = Assoc []

instance (PDG f, PDG g) => PDG (f :*: g) where
  pDeclG Proxy = pDeclG (Proxy @ f) <> pDeclG (Proxy @ g)

instance PDG f => PDG (M1 C p f) where
  pDeclG Proxy = pDeclG (Proxy @ f)

instance PDG f => PDG (M1 D p f) where
  pDeclG Proxy = pDeclG (Proxy @ f)

instance (Selector t, PS f) => PDG (M1 S t f) where
  pDeclG Proxy = Assoc [(name, ps (Proxy @ f) name)]
    where name = Text.pack . convertField $ selName (undefined :: M1 S t f a)

--------------------------------------------------------------------------------

class PS (a :: * -> *) where
  ps :: Proxy a -> Text -> CardF PDecl

instance (Param a, KnownSymbol d) => PS (K1 i (a :% 'Single :? d)) where
  ps Proxy name = SingleF $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (parType (Proxy @ a))

instance (Param a, KnownSymbol d) => PS (K1 i (a :% 'Optional :? d)) where
  ps Proxy name = OptionalF . pure $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (parType (Proxy @ a))

instance (Param a, KnownSymbol d) => PS (K1 i (a :% 'Many :? d)) where
  ps Proxy name = ManyF . pure $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (parType (Proxy @ a))
