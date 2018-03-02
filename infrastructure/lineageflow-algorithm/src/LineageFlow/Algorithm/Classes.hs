module LineageFlow.Algorithm.Classes
  (
  -- * Class for parameters
    Param (..)
  -- * Class for domains
  , Domain (..)
  -- * Class for codomains
  , Codomain (..)
  ) where

import LineageFlow.Declaration
import LineageFlow.Types

import Data.Proxy

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Monoid

--------------------------------------------------------------------------------

class Param a where
  parType :: Proxy a -> PType

instance Param Int where
  parType Proxy = PType "int"

instance Param Double where
  parType Proxy = PType "double"

--------------------------------------------------------------------------------

class Domain (f :: * -> *) where
  domType :: Proxy f -> Text

instance Domain Identity where
  domType Proxy = "value"

instance Codomain i => Domain (f i) where
  domType Proxy = codType (Proxy @i)

instance (Codomain i, Codomain j) => Domain (Compose (f i) (g j)) where
  domType Proxy = codType (Proxy @(i,j))

--------------------------------------------------------------------------------

class Codomain a where
  codType :: Proxy a -> Text

instance Codomain Time where
  codType Proxy = "time"

instance Codomain Cell where
  codType Proxy = "cell"

instance (Codomain a, Codomain b) => Codomain (a,b) where
  codType Proxy = "(" <> codType (Proxy @a) <> ","  <> codType (Proxy @b) <> ")"

instance (Codomain a, Codomain b) => Codomain (Dep a b) where
  codType Proxy = "[" <> codType (Proxy @b) <> ","  <> codType (Proxy @a) <> "]"

instance Codomain a => Codomain (Maybe a) where
  codType Proxy = codType (Proxy @a) <> "?"

instance Codomain Int where
  codType Proxy = "number"

instance Codomain Scalar where
  codType Proxy = "scalar"

instance Codomain Vector where
  codType Proxy = "vector"

instance Codomain Tensor where
  codType Proxy = "tensor"

instance Codomain a => Codomain (S1 a) where
  codType Proxy = "s1 " <> codType (Proxy @a)

instance Codomain a => Codomain (S2 a) where
  codType Proxy = "s2 " <> codType (Proxy @a)

instance Codomain a => Codomain (S3 a) where
  codType Proxy = "s3 " <> codType (Proxy @a)

instance Codomain a => Codomain (S4 a) where
  codType Proxy = "s4 " <> codType (Proxy @a)

instance Codomain Sign where
  codType Proxy = "sign"
