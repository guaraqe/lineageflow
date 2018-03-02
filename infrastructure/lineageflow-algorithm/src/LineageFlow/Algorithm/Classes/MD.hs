module LineageFlow.Algorithm.Classes.MD
  (
  -- * Measurement declaration
    MD (..)
  ) where

import LineageFlow.Algorithm.Imports
import LineageFlow.Algorithm.Help
import LineageFlow.Algorithm.Classes

import LineageFlow.Declaration

import qualified Data.Text as Text
import Data.Monoid
import Data.Proxy

--------------------------------------------------------------------------------

type InMeta = Assoc (CardF MDecl)

class MD a where
  mDecl :: Proxy a -> InMeta
  default
    mDecl :: (Generic a, MDG (Rep a)) =>
      Proxy a -> InMeta
  mDecl Proxy = mDeclG (Proxy @ (Rep a))

--------------------------------------------------------------------------------

class MDG (f :: * -> *) where
  mDeclG :: Proxy f -> InMeta

instance (MDG f, MDG g) => MDG (f :*: g) where
  mDeclG Proxy =
    mDeclG (Proxy @ f) <> mDeclG (Proxy @ g)

instance MDG f => MDG (M1 C p f) where
  mDeclG Proxy = mDeclG (Proxy @ f)

instance MDG f => MDG (M1 D p f) where
  mDeclG Proxy = mDeclG (Proxy @ f)

instance (Selector t, MS f) => MDG (M1 S t f) where
  mDeclG Proxy = Assoc [(name, ms (Proxy @ f) name)]
    where name = Text.pack . convertField $ selName (undefined :: M1 S t f a)

--------------------------------------------------------------------------------

class MS (a :: * -> *) where
  ms :: Proxy a -> Text -> CardF MDecl

instance (KnownSymbol d, Domain f, Codomain a) => MS (K1 i ((f a) :% 'Single :? d)) where
  ms Proxy name = SingleF $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (MType (domType (Proxy @ f)) (codType (Proxy @ a)))

instance (KnownSymbol d, Domain f, Codomain a) => MS (K1 i ((f a) :% 'Optional :? d)) where
  ms Proxy name = OptionalF . pure $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (MType (domType (Proxy @ f)) (codType (Proxy @ a)))

instance (KnownSymbol d, Domain f, Codomain a) => MS (K1 i ((f a) :% 'Many :? d)) where
  ms Proxy name = ManyF . pure $
    Decl
      name
      (Text.pack (symbolVal (Proxy @ d)))
      (MType (domType (Proxy @ f)) (codType (Proxy @ a)))
