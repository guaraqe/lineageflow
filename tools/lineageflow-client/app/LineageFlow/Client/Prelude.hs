module LineageFlow.Client.Prelude
  ( module Export
  , bindDyn
  , listDyn
  , listDynA
  , traverseDyn
  , traverseDynA
  , (<$>:)
  , (<*>:)
  , mayEE
  , mayDE
  , mayDE2
  , mayDE3
  , mayDD
  , uncurryDyn
  , uncurryDyn3
  ) where

import LineageFlow.Server.API as Export
import LineageFlow.Viewer.Interface as Export
import LineageFlow.Script as Export

import Reflex as Export hiding (Query, Global)
import Reflex.Dom as Export hiding (Query, Alt, askEvents, run, Global)
import Reflex.Elm as Export

import Control.Lens as Export hiding (element)
import Control.Applicative as Export
import Control.Monad as Export
import Control.Monad.Reader as Export
import Data.Monoid as Export

import System.Exit as Export
import Data.Text as Export (Text)

import Data.Maybe as Export

import Data.Functor.Compose

bindDyn ::
  MonadWidget t m =>
  (a -> m (Dynamic t b)) -> b -> Dynamic t a -> m (Dynamic t b)
bindDyn f start x = do
  y <- dyn $ fmap f x
  fmap join $ holdDyn (constDyn start) y
{-# INLINE bindDyn #-}

listDyn ::
  MonadWidget t m =>
  (Dynamic t a -> m (Dynamic t b)) -> Dynamic t [a] -> m (Dynamic t [b])
listDyn f x = do
  y <- dyn $ fmap (fmap sequence . traverse f . fmap pure) $ x
  z <- holdDyn (pure []) y
  return (join z)

listDynA ::
  (MonadWidget t m, Applicative f) =>
  (Dynamic t a -> m (Dynamic t (f b))) -> Dynamic t [a] -> m (Dynamic t (f [b]))
listDynA f = fmap (fmap sequenceA) . listDyn f

traverseDyn ::
  MonadWidget t m =>
  (a -> m (Dynamic t b)) -> Dynamic t [a] -> m (Dynamic t [b])
traverseDyn f = bindDyn (fmap sequence . traverse f) []
{-# INLINE traverseDyn #-}

traverseDynA ::
  (MonadWidget t m, Applicative f) =>
  (a -> m (Dynamic t (f b))) -> Dynamic t [a] -> m (Dynamic t (f [b]))
traverseDynA f = fmap (fmap sequenceA) . traverseDyn f
{-# INLINE traverseDynA #-}

(<$>:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$>:) f = getCompose . fmap f . Compose
{-# INLINE (<$>:) #-}

(<*>:) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<*>:) f x = getCompose $ Compose f <*> Compose x
{-# INLINE (<*>:) #-}


mayEE ::
  (Reflex t, Monad m) =>
  (Event t a -> m (Event t b)) -> Event t (Maybe a) -> m (Event t (Maybe b))
mayEE f e =
  let
    justs = fmapMaybe id e
    nots = ffilter isNothing e
  in do
    val <- f justs
    return $ leftmost [fmap Just val, fmap (const Nothing) nots]
{-# INLINE mayEE #-}

mayDE ::
  (Reflex t, MonadHold t m) =>
  (Dynamic t a -> m (Event t b)) -> a -> Dynamic t (Maybe a) -> m (Event t (Maybe b))
mayDE f x e =
  let
    justs = fmapMaybe id (updated e)
    nots = ffilter isNothing (updated e)
  in do
    d <- holdDyn x justs
    val <- f d
    return $ leftmost [fmap Just val, fmap (const Nothing) nots]
{-# INLINE mayDE #-}

mayDE2 ::
  (Reflex t, MonadHold t m) =>
  (Dynamic t a -> Dynamic t b -> m (Event t c)) ->
  a -> b ->
  Dynamic t (Maybe a) -> Dynamic t (Maybe b) -> m (Event t (Maybe c))
mayDE2 f a b x y = mayDE (uncurryDyn f) (a,b) (liftA2 (liftA2 (,)) x y)
{-# INLINE mayDE2 #-}

mayDE3 ::
  (Reflex t, MonadHold t m) =>
  (Dynamic t a -> Dynamic t b -> Dynamic t c -> m (Event t d)) ->
  a -> b -> c ->
  Dynamic t (Maybe a) -> Dynamic t (Maybe b) -> Dynamic t (Maybe c) -> m (Event t (Maybe d))
mayDE3 f a b c x y z = mayDE (uncurryDyn3 f) (a,b,c) (liftA3 (liftA3 (,,)) x y z)
{-# INLINE mayDE3 #-}

mayDD ::
  (Reflex t, MonadHold t m) =>
  (Dynamic t a -> m (Dynamic t b)) -> a -> Dynamic t (Maybe a) -> m (Dynamic t (Maybe b))
mayDD f x e = do
  val <- mayDE (fmap updated . f) x e
  holdDyn Nothing val
{-# INLINE mayDD #-}

uncurryDyn ::
  Reflex t =>
  (Dynamic t a -> Dynamic t b -> c) -> Dynamic t (a, b) -> c
uncurryDyn f x = f (fmap (view _1) x) (fmap (view _2) x)
{-# INLINE uncurryDyn #-}

uncurryDyn3 ::
  Reflex t =>
  (Dynamic t a -> Dynamic t b -> Dynamic t c-> d) -> Dynamic t (a, b, c) -> d
uncurryDyn3 f x = f (fmap (view _1) x) (fmap (view _2) x) (fmap (view _3) x)
{-# INLINE uncurryDyn3 #-}
