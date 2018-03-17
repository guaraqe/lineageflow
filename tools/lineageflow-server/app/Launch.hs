{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Launch
  ( launch
  ) where

import Imports
import Types.App

import LineageFlow.Algorithm

import Data.Maybe
import Data.Functor.Const
import Data.Functor.Identity

import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files (createSymbolicLink)
import System.Process

import qualified Data.Text as Text

--------------------------------------------------------------------------------

launch :: AQuery -> App ProcessHandle
launch aquery =
  let
    exec = fromMaybe (error "Query does not contain executable") $
      lookupAssoc "executable" $ aquery ^. query_fields
    comm = fromMaybe (error "Query does not contain command") $
      lookupAssoc "command" $ aquery ^. query_fields
    inputs = aquery ^. query_type . atype_inputs
    outputs = aquery ^. query_type . atype_outputs
  in do
    tmp <- liftIO $ getCanonicalTemporaryDirectory
    inputFolder <- liftIO $ createTempDirectory tmp "input"
    outputFolder <- liftIO $ createTempDirectory tmp "output"

    deployAllInputs inputFolder inputs
    let apath = makeAPath inputFolder outputFolder aquery
    (err,handl) <- liftIO $ launchAlgorithm exec comm apath
    exit <- liftIO $ waitForProcess handl

    case exit of
      ExitFailure code -> liftIO $ do
        putStrLn $ "Executable " <> Text.unpack exec <> " failed with code " <> show code
        errmsg <- hGetContents err
        putStrLn $ "Error message:\n" <> errmsg
      ExitSuccess -> do
        saveAllOutputs aquery outputFolder outputs
        liftIO $ removeDirectoryRecursive inputFolder
        liftIO $ removeDirectoryRecursive outputFolder

    return handl

--------------------------------------------------------------------------------

saveAllOutputs :: AQuery -> FilePath -> Assoc (CardF MQuery) -> App ()
saveAllOutputs aquery output mqueries = void $ mapAssocM f mqueries
  where
    f name = saveOutput aquery (output </> Text.unpack name)

saveOutput :: AQuery -> FilePath -> CardF MQuery -> App ()
saveOutput aquery output cardf = void $ cardNameM f output cardf
  where
    f path mquery = do
      dbpath <- viewM server_dbPath
      liftIO $ db_put (sqliteDatabase dbpath) aquery mquery path

--------------------------------------------------------------------------------

deployAllInputs :: FilePath -> Assoc (CardF MQuery) -> App ()
deployAllInputs input mqueries = void $ mapAssocM f mqueries
  where
    f name = deployInput (input </> Text.unpack name)

deployInput :: FilePath -> CardF MQuery -> App ()
deployInput input cardf = void $ cardNameM f input cardf
  where
    f path mquery = do
      file <- getFilePath mquery
      liftIO $ createSymbolicLink file path

getFilePath :: MQuery -> App FilePath
getFilePath mquery = do
  dbpath <- viewM server_dbPath
  liftIO $ db_get (sqliteDatabase dbpath) mquery

--------------------------------------------------------------------------------

makeAPath :: FilePath -> FilePath -> AQuery -> APath
makeAPath input output (Query _ (AType p i o)) = AType p' i' o'
  where
    f path _ = Const path
    p' = fmap (fmap mapPar) p
    i' = mapAssoc (\name -> cardName f (input </> Text.unpack name)) i
    o' = mapAssoc (\name -> cardName f (output </> Text.unpack name)) o

mapPar :: Query PType -> Const String PType
mapPar (Query fields _) =
  let
    value = Text.unpack $ fromJust $ lookupAssoc "value" fields
  in
    Const value

--------------------------------------------------------------------------------

mapAssoc :: (Text -> a -> b) -> Assoc a -> Assoc b
mapAssoc f = runIdentity . mapAssocM (\t a -> Identity (f t a))

mapAssocM :: Applicative m => (Text -> a -> m b) -> Assoc a -> m (Assoc b)
mapAssocM f (Assoc l) = Assoc <$> traverse (\(t,v) -> fmap (t,) (f t v)) l

cardName :: (FilePath -> a -> b) -> FilePath -> CardF a -> CardF b
cardName f path = runIdentity . cardNameM (\t a -> Identity (f t a)) path

cardNameM ::
  Applicative m =>
  (FilePath -> a -> m b) -> FilePath -> CardF a -> m (CardF b)
cardNameM f path (ManyF l) = fmap ManyF $ sequenceA $
  zipWith (\x n -> f (path ++ "-" ++ show n) x) l [1 :: Int ..]
cardNameM f path cardf = traverse (f path) cardf
