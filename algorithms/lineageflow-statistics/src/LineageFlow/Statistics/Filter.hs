module LineageFlow.Statistics.Filter
  ( nanFilterWith
  , nanFilterDomainWith
  ) where

import LineageFlow.Prelude hiding (unzip)
import qualified LineageFlow.ArrayU as ArrayU

--------------------------------------------------------------------------------

nanFilterWith ::
  (Dom (Array i) a) =>
  (a -> Scalar) -> (Scalar -> Bool) -> Array i a -> Array i a
nanFilterWith f = \b ->
  mkIx .
  _fmap fst .
  ArrayU.filter (\(_,x) -> b x && not (isNaN x)) .
  _fmap (\x -> (x,f x))
  . unIx
{-# INLINE nanFilterWith #-}

nanFilterDomainWith ::
  (Dom ArrayU j) =>
  (Scalar -> Bool) ->
  Array (Dep i j) j ->
  Array (Dep i j) Scalar ->
  ( Array (Dep i j) j
  , Array (Dep i j) Scalar )
nanFilterDomainWith = \b d i ->
  bimap mkIx mkIx .
  _unzip .
  ArrayU.filter (\(_,x) -> b x && not (isNaN x)) $
  _zipWith (,) (unIx d) (unIx i)
{-# INLINE nanFilterDomainWith #-}
