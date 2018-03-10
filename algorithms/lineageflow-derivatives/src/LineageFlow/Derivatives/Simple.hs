module LineageFlow.Derivatives.Simple
  ( simpleFw
  , module LineageFlow.Derivatives.Types
  ) where

import LineageFlow.Prelude
import LineageFlow.Derivatives.Types

import qualified LineageFlow.ArrayU as ArrayU

nan :: Scalar
nan = 0/0

-------------------------------------------------------------------------------

simpleFw :: Int -> Deriver
simpleFw n = Deriver (simpleFwS n)
{-# INLINE simpleFw #-}

simpleFwS :: Int -> ArrayU Scalar -> ArrayU Scalar
simpleFwS n = \v ->
  if _length v <= n
    then ArrayU.replicate (_length v) nan
    else
      _fmap (/ fromIntegral n) $
        (_zipWith (-) (ArrayU.drop n v) v) <> ArrayU.replicate n nan
{-# INLINE simpleFwS #-}
