{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.App
  ( App (..)
  , runApp
  , viewM
  , setM
  , overM
  , makeServer
  , module Types.ServerState
  ) where

import Imports
import Types.ServerState
import Control.Exception.Base

newtype App a = App
  { getApp :: ReaderT Processes Handler a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Processes)

runApp :: App a -> Processes -> IO a
runApp app ps =
  fmap (either (error . displayException) id) $
  runHandler $
  runReaderT (getApp app) ps

viewM :: (MonadReader (IORef a) m, MonadIO m) => Lens' a b -> m b
viewM l = do
  ioref <- ask
  liftIO $ fmap (view l) $ readIORef ioref

setM :: (MonadReader (IORef a) m, MonadIO m) => Lens' a b -> b -> m ()
setM l a = do
  ioref <- ask
  liftIO $ modifyIORef' ioref (set l a)

overM :: (MonadReader (IORef a) m, MonadIO m) => Lens' a b -> (b -> b) -> m ()
overM l f = do
  ioref <- ask
  liftIO $ modifyIORef' ioref (over l f)

-- Change to hoistServer later
makeServer :: Processes -> ServerT API App -> Server API
makeServer ps = enter $ NT $ \app -> do
  runReaderT (getApp app) ps
