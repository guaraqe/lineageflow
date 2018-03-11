{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LineageFlow.Viewer.Tensor
  ( eigen3D
  ) where

import LineageFlow.Prelude

import qualified Language.C.Inline as C
import Foreign.Marshal
import Foreign.C.Types
import Foreign

import Unsafe.Coerce

C.include "../../../cbits/dsyevh3.h"

dsyevh3 :: Ptr (Ptr CDouble) -> Ptr (Ptr CDouble) -> Ptr CDouble -> IO CInt
dsyevh3 mat col eig =
  [C.exp| int { dsyevh3($(double mat[3][3]),$(double col[3][3]),$(double* eig)) } |]

eigen3D :: Tensor -> (Vector, Tensor)
eigen3D t = unsafePerformIO $
  with ((t + transpose t) / 2) $ \tensor ->
  alloca $ \(vectors :: Ptr Tensor) ->
  alloca $ \(eigen :: Ptr Vector) -> do
    dsyevh3 (unsafeCoerce tensor) (unsafeCoerce vectors) (unsafeCoerce eigen)
    (,) <$> peek eigen <*> peek vectors
