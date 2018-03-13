module LineageFlow.Prelude.Fold
  ( Fold (..)
  , _fold'
  , _foldM
  ) where

import BasePrelude
import Control.ConstraintClasses

import Control.Foldl

-- | Apply a strict left 'Fold' to a 'Foldable' container
_fold' :: (CFoldable f, Dom f a) => Fold a b -> f a -> b
_fold' (Fold step begin done) as = _foldr cons done as begin
  where
    cons a k x = k $! step x a
{-# INLINE _fold' #-}

-- | Like 'fold', but monadic
_foldM :: (CFoldable f, Monad m, Dom f a) => FoldM m a b -> f a -> m b
_foldM (FoldM step begin done) as0 = do
    x0 <- begin
    _foldr step' done as0 $! x0
  where
    step' a k x = do
        x' <- step x a
        k $! x'
{-# INLINE _foldM #-}
