{-# LANGUAGE FlexibleContexts #-}

module LineageFlow.Forces.Eigen
  ( pseudoSolve
  ) where

import LineageFlow.Prelude

import LineageFlow.Forces.Sparse

import Data.Eigen.SparseLA
import qualified Data.Eigen.SparseMatrix as Eigen
import qualified Data.Eigen.Matrix as Eigen (Matrix (..))
import Foreign.C.Types

import System.IO.Unsafe (unsafePerformIO)

pseudoSolve ::
  DArray Time (DS2 Time Cell) (DS2 Time Cell) ->
  DArray Time (DS2 Time Cell) Scalar ->
  DArray Time Cell Vector ->
  DArray Time (DS2 Time Cell) Vector
pseudoSolve neigh conn v =
  let
    cellN = _length (unIx v)
    neighN = _length neigh
    mtEigen = Eigen.fromList cellN cellN $ matrixSquare cellN neigh conn

    neighM = contactsToSparse neigh conn

    vx = Eigen.fromMatrix (Eigen.Matrix cellN 1 (_fmap (CDouble . view _x) (unIx v)))
    vy = Eigen.fromMatrix (Eigen.Matrix cellN 1 (_fmap (CDouble . view _y) (unIx v)))
    vz = Eigen.fromMatrix (Eigen.Matrix cellN 1 (_fmap (CDouble . view _z) (unIx v)))

    getVector (Eigen.Matrix _ _ x) = _fmap (\(CDouble a) -> a) x
  in
    (\(V3 x y z) -> _zipWith3 V3 x y z) .
    fmap (sparseAssocTimesVector neighN neighM . mkIx . getVector . Eigen.toMatrix) .
    unsafePerformIO $ ( do
    r <- runSolverT (SparseQR COLAMDOrdering) $ do
        compute (Eigen.compress mtEigen)
        x <- solve vx
        y <- solve vy
        z <- solve vz
        x `seq` y `seq` z `seq` return (V3 x y z)
    r `seq` return r
    )
{-# INLINE pseudoSolve #-}
