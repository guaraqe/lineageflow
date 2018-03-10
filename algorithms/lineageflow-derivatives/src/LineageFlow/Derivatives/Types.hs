module LineageFlow.Derivatives.Types
  ( Deriver (..)
  , tcDeriveS
  , tcDeriveV
  , deriveS
  , deriveV
  , derive2S
  , derive2V
  ) where

import LineageFlow.Prelude

newtype Deriver = Deriver
  { derive :: ArrayU Scalar -> ArrayU Scalar }

tcDeriveS :: TC -> CT -> Deriver -> TCMap Scalar -> TCMap Scalar
tcDeriveS tc ct der measure =
  over (tc2ct tc ct) (\(Compose m) -> Compose (fmap (deriveS der) m)) measure
{-# INLINE tcDeriveS #-}

tcDeriveV :: TC -> CT -> Deriver -> TCMap Vector -> TCMap Vector
tcDeriveV tc ct der measure =
  over (tc2ct tc ct) (\(Compose m) -> Compose (fmap (deriveV der) m)) measure
{-# INLINE tcDeriveV #-}

deriveS :: Deriver -> Array t Scalar -> Array t Scalar
deriveS (Deriver d) = over ixed d

derive2S :: Deriver -> Array t Scalar -> Array t Scalar
derive2S d = deriveS d . deriveS d

deriveV :: Deriver -> Array t Vector -> Array t Vector
deriveV (Deriver d) = \v ->
  _zipWith3 V3
    (over ixed d $ _fmap (view _x) v)
    (over ixed d $ _fmap (view _y) v)
    (over ixed d $ _fmap (view _z) v)

derive2V :: Deriver -> Array t Vector -> Array t Vector
derive2V d = deriveV d . deriveV d

