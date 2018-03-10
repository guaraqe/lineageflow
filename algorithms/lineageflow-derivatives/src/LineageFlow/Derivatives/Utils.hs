module LineageFlow.Derivatives.Utils
  ( convol
  ) where

import LineageFlow.Prelude

import LineageFlow.Linear.Raw (corr)

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
{-# INLINE convol #-}
