module Algorithm ( handler ) where

import Imports
import Types.App

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

handler :: AQuery -> App Int
handler alg = do
  processes <- ask
  liftIO $ do
    id' <- nextInt <$> readIORef processes
    modifyIORef' processes $
      over server_waiting (IntMap.insert id' alg)
    return id'

maxIntMap :: IntMap AQuery -> Int
maxIntMap m =
  case IntMap.toDescList m of
    [] -> 0
    (n,_):_ -> n

maxSet :: Set Int -> Int
maxSet m =
  if Set.null m
    then 0
    else Set.findMax m

nextInt :: ServerState -> Int
nextInt (ServerState _ r w s f _ _) =
  let
    fixed = max (maxSet (Set.union s f)) (maxIntMap w)
  in
    maybe fixed (max fixed) r + 1
