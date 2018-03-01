{-# LANGUAGE
  StandaloneDeriving
, FlexibleInstances
#-}

module LineageFlow.Declaration
  (
  -- * Declaration
    Decl (Decl)
  -- * Particular declarations
  , PDecl
  , MDecl
  , ADecl
  -- * Lenses for @Decl@
  , decl_name
  , decl_desc
  , decl_type
  -- * Extra
  , getDecl
  , module Export
  ) where

import LineageFlow.Base as Export
import Data.Text (Text)

import GHC.Generics
import Data.Aeson.Types
import Control.Lens

--------------------------------------------------------------------------------

-- | Declarations of a given type @a@ contain names and descriptions attached
-- to an element of type `a`.
data Decl a = Decl
  { _decl_name :: Text
  , _decl_desc :: Text
  , _decl_type :: a
  } deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

$(makeLenses ''Decl)

instance ToJSON a => ToJSON (Decl a) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON a => FromJSON (Decl a) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

--------------------------------------------------------------------------------

-- | Declaration of parameters
type PDecl = Decl PType

-- | Declaration of measurements
type MDecl = Decl MType

-- | Declaration of algorithms. One needs to describe not only the algorithm,
-- but also the internal components.
type ADecl = Decl (AType Decl)

--------------------------------------------------------------------------------

deriving instance Eq (AType Decl)
deriving instance Show (AType Decl)

instance ToJSON (AType Decl) where
  toJSON = genericToJSON $
    defaultOptions { fieldLabelModifier = convertField }

instance FromJSON (AType Decl) where
  parseJSON = genericParseJSON $
    defaultOptions { fieldLabelModifier = convertField }

--------------------------------------------------------------------------------

-- | Recover declarations from different cardinalities. May throw exception if
-- declaration is empty, which should not happen.
getDecl :: CardF (Decl a) -> Decl a
getDecl (SingleF x) = x
getDecl (OptionalF (Just x)) = x
getDecl (ManyF (x:_)) = x
getDecl (OptionalF Nothing) =
  error "LineageFlow.Interface.Decl.getDecl: Empty optional declaration."
getDecl (ManyF []) =
  error "LineageFlow.Interface.Decl.getDecl: Empty list declaration."
