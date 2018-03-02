{-# OPTIONS_GHC -fno-warn-orphans #-}

module LineageFlow.Types.Numerical
       (
       -- * Scalar Measurements
         Scalar
       -- * Vector Measurements
       , Vector
       , V3 (..)
       , (!.!)
       , norm
       , normalize
       , project
       , _x
       , _y
       , _z
       , cross
       , triple
       -- * Tensor Measurements
       , Tensor
       , tensorProductWith
       , (!*!)
       , (!*)
       , (*!)
       , (!!*)
       , (*!!)
       , (!!/)
       , transpose
       , det33
       , trace
       , diagonal
       , inv33
       , identity
       , module Foreign.Storable.Tuple ) where

import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Storable.Tuple

import Linear.Matrix
import Linear.V3

----------------------------------------------------------------------

-- | 'Scalar' types are represented by `Double`. The type synonym is
-- there for a clearer semantics.
type Scalar = Double

----------------------------------------------------------------------

-- | 'Vector' types are the same as the ones used from the @linear@
-- package. The constructor is kept the same. Many utility functions
-- have been reexported.
type Vector = V3 Scalar

infixl 7 !.!

-- | Inner product for 'Vector'.
(!.!) :: Vector -> Vector -> Scalar
(V3 x1 y1 z1) !.! (V3 x2 y2 z2) = x1*x2 + y1*y2 +z1*z2
{-# INLINE (!.!) #-}

-- | 'Vector' norm.
norm :: Vector -> Scalar
norm v1 = sqrt (v1 !.! v1)
{-# INLINE norm #-}

-- | Normalize 'Vector' to unit norm.
normalize :: Vector -> Vector
normalize v = fmap (/n) v
  where n = norm v
{-# INLINE normalize #-}

project :: Vector -> Vector -> (Scalar, Vector)
project v1 v2 =
  let
    n = normalize v2
    p = v1 !.! n
  in
    (p, v1 - fmap (*p) n)
{-# INLINE project #-}

----------------------------------------------------------------------

-- | The 'Tensor' type is just a 'Vector' of 'Vector' and as such, is
-- a row-major representation of a matrix. Products, transpositions
-- and others have been reexported from @linear@.
type Tensor = M33 Double

----------------------------------------------------------------------

tensorProductWith ::
  (Double -> Double -> Double) -> Vector -> Vector -> Tensor
tensorProductWith f = \v1 v2 -> fmap (\x -> fmap (f x) v2) v1
{-# INLINE tensorProductWith #-}


--------------------------------------------------------------------------------

instance Storable a => Storable (Maybe a) where
    sizeOf x = sizeOf (stripMaybe x) + 1
    alignment x = alignment (stripMaybe x)
    peek ptr = do
        filled <- peekByteOff ptr $ sizeOf $ stripMaybe $ stripPtr ptr
        if filled == (1 :: Word8)
            then do
                x <- peek $ stripMaybePtr ptr
                return $ Just x
            else return Nothing
    poke ptr Nothing = pokeByteOff ptr
                       (sizeOf $ stripMaybe $ stripPtr ptr)
                       (0 :: Word8)
    poke ptr (Just a) = do
        poke (stripMaybePtr ptr) a
        pokeByteOff ptr (sizeOf a) (1 :: Word8)

stripMaybe :: Maybe a -> a
stripMaybe _ = error "stripMaybe"

stripMaybePtr :: Ptr (Maybe a) -> Ptr a
stripMaybePtr = castPtr

stripPtr :: Ptr a -> a
stripPtr _ = error "stripPtr"
