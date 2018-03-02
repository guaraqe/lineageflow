module LineageFlow.Types.Cell
       (
       -- * @Cell@
         Cell (..)
       -- * Lens for @Cell@
       , cell
       ) where

import Control.Lens
import Control.Newtype

import Foreign.Storable

----------------------------------------------------------------------

-- | Identifiers for cells.
newtype Cell = Cell { _cell :: Int }
  deriving (Show, Eq, Ord, Storable)

$(makeLenses ''Cell)

instance Newtype Cell Int where
  pack = Cell
  {-# INLINE pack #-}
  unpack = _cell
  {-# INLINE unpack #-}
