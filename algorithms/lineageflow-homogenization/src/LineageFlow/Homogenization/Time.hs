module LineageFlow.Homogenization.Time
  ( flat
  , module LineageFlow.Homogenization.Types
  ) where

import LineageFlow.Prelude
import LineageFlow.Linear.Raw (corr)
import qualified LineageFlow.ArrayU as ArrayU

import LineageFlow.Homogenization.Types

nan :: Double
nan = 0/0

convol :: ArrayU Scalar -> Int -> Int -> ArrayU Scalar -> ArrayU Scalar
convol k n c v =
  if _length k > _length v
     then _fmap (const nan) v
     else prefix <> corr k v <> suffix
  where
    prefix = fromList $ replicate c nan
    suffix = fromList $ replicate (n-c-1) nan

--------------------------------------------------------------------------------

flat :: Int -> Homogenizer i
flat timeScale = Homogenizer $
  mkIx . convol diffFilter filterSize filterCenter . unIx
  where
    n = timeScale * 2 + 1
    diffFilter = _fmap (/ fromIntegral n) (ArrayU.replicate n 1)
    filterSize = _length diffFilter
    filterCenter = timeScale


