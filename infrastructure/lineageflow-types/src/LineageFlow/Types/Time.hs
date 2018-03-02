module LineageFlow.Types.Time
       (
       -- * @Time@
         Time (..)
       -- * Lens for @Time@
       , time
       ) where

import Control.Lens
import Control.Newtype

import Foreign.Storable

----------------------------------------------------------------------

-- | Identifiers for time steps
newtype Time = Time { _time :: Int }
  deriving (Show, Eq, Ord, Storable)

$(makeLenses ''Time)

instance Newtype Time Int where
  pack = Time
  {-# INLINE pack #-}
  unpack = _time
  {-# INLINE unpack #-}
