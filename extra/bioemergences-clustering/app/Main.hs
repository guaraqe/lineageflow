{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified PositionAndVelocity as PositionAndVelocity
import qualified PositionOnly as PositionOnly

import LineageFlow.Prelude hiding (option)
import Options.Applicative

data Input
  = InputPositionOnly PositionOnly.Input
  | InputPositionAndVelocity PositionAndVelocity.Input

input :: Parser Input
input =
  let
    commandPositionOnly =
      command "position-only" $
      fmap InputPositionOnly $
      info (PositionOnly.input <**> helper) $
      fullDesc <>
      progDesc "Clustering of cell tractories using cells' positions." <>
      header "bioemergences-clustering"

    commandPositionAndVelocity =
      command "position-and-velocity" $
      fmap InputPositionAndVelocity $
      info (PositionAndVelocity.input <**> helper) $
      fullDesc <>
      progDesc "Clustering of cell tractories using cells' positions and velocities." <>
      header "bioemergences-clustering"
  in
    subparser $ commandPositionOnly <> commandPositionAndVelocity

run :: Input -> IO ()
run (InputPositionOnly opts) = PositionOnly.run opts
run (InputPositionAndVelocity opts) = PositionAndVelocity.run opts

main :: IO ()
main =
  customExecParser (prefs showHelpOnError) opts >>= run
  where
    opts =
      info (input <**> helper) $
      fullDesc <>
      progDesc "Complete pipeline for clustering of cell tractories." <>
      header "bioemergences-clustering"


