{-# LANGUAGE MultiWayIf #-}

module Check ( handler ) where

import Imports

import Types.App

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set

handler :: Int -> App Status
handler n = do
  running <- viewM server_running
  success <- viewM server_succeded
  failed <- viewM server_failed
  waiting <- Set.fromList . IntMap.keys <$> viewM server_waiting

  if | Just n == running    -> return StatusRunning
     | Set.member n waiting -> return StatusWaiting
     | Set.member n success -> return StatusSuccess
     | Set.member n failed  -> return StatusFailed
     | otherwise            -> return StatusNever
