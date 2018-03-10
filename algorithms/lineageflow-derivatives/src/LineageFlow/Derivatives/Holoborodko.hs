module LineageFlow.Derivatives.Holoborodko
  ( holo
  , module LineageFlow.Derivatives.Types
  ) where

import LineageFlow.Prelude
import LineageFlow.Derivatives.Types
import LineageFlow.Derivatives.Utils

import Math.Combinatorics.Exact.Binomial

holoFilter :: Int -> ArrayU Double
holoFilter n = fromList $ antiReflect [ holoFilterElement n k | k <- [1..n] ]

holoFilterElement :: Int -> Int -> Double
holoFilterElement n k = fromIntegral num / fromIntegral den where
    m = n - 1
    num = choose (2*m) (m-k+1) - choose (2*m) (m-k-1) :: Int
    den = 2^(2*m+1) :: Int

antiReflect :: (Num a) => [a] -> [a]
antiReflect l = map negate (reverse l) <> [0] <> l

--------------------------------------------------------------------------------

scalarDerivative :: Int -> ArrayU Scalar -> ArrayU Scalar
scalarDerivative timeScale = convol diffFilter filterSize filterCenter where
    diffFilter = holoFilter timeScale
    filterSize = _length diffFilter
    filterCenter = timeScale

holo :: Int -> Deriver
holo n = Deriver (scalarDerivative n)
