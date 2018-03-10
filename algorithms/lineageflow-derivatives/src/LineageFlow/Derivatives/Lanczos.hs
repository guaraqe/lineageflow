{-# LANGUAGE NPlusKPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LineageFlow.Derivatives.Lanczos
  ( lanczos
  , module LineageFlow.Derivatives.Types
  ) where

import LineageFlow.Prelude
import LineageFlow.Derivatives.Types
import LineageFlow.Derivatives.Utils

import qualified LineageFlow.ArrayU as ArrayU

-- Legendre polynomial of size @n@ and order @r@
legendre :: Int -> Int -> Double -> Double
legendre _ 0 _ = 1
legendre _ 1 x = x
legendre n (r + 1) x =
 let
   n' = fromIntegral n
   r' = fromIntegral r
 in
   (2 * r' + 1) * x * legendre n r x / (r' + 1) -
   r' * ((2 * n' + 1) ** 2 - r' ** 2) * legendre n (r - 1) x / (4 * n' ** 2 * (r' + 1))

-- Derivative of order @d@ of Legendre polynomial of size @n@ and order @r@
legendre' :: Int -> Int -> Int -> Double -> Double
legendre' 0 n r x = legendre n r x
legendre' _ _ 0 _ = 0
legendre' _ _ 1 _ = 1
legendre' d n (r + 1) x =
 let
   n' = fromIntegral n
   r' = fromIntegral r
   d' = fromIntegral d
 in
   (2 * r' + 1) * (d' * legendre' (d - 1) n r x + x * legendre' d n r x) / (r' + 1) -
   r' * ((2 * n' + 1) ** 2 - r' ** 2) * legendre' d n (r - 1) x / (4 * n' ** 2 * (r' + 1))

inner :: Int -> (Double -> Double) -> (Double -> Double) -> Double
inner n p q =
  let
    n' = fromIntegral n
    rng = fmap fromIntegral [-n .. n]
  in
    foldl' (+) 0 (fmap (\x -> p(x / n') * q(x / n')) rng) / n'

coefficient :: Int -> Int -> Int -> Int -> Int -> Double
coefficient r n s p j =
  let
    n' = fromIntegral n
    j' = fromIntegral j
    r' = fromIntegral r
    s' = fromIntegral s
  in
    foldl' (+) 0 $ flip fmap [r .. r + p - 1] $
      \l -> legendre' r n l (s' / n') * legendre n l (j' / n') /
        (inner n (legendre n l) (legendre n l) * n' ** (r' + 1) )

lanczosFilter :: Int -> Int -> Int -> Int -> ArrayU Double
lanczosFilter r n s p = _fmap (coefficient r n s p) $ ArrayU.enumFromN (-n) (2 * n + 1)

-- | The deriver @lanczos r n p@ calculates derivatives of order @r@ with a
-- smoothing over @n@ time steps, with a center in @s@ using polynomials of
-- order up to @p@.
lanczos :: Int -> Int -> Int -> Int -> Deriver
lanczos r n s p = Deriver $ \v ->
  convol (lanczosFilter r n s p) (2 * n + 1) (n + s) v
