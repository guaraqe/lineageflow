module LineageFlow.Algorithm
  ( module Export
  , embed
  ) where

import LineageFlow.Algorithm.Algorithm as Export
import LineageFlow.Algorithm.Help as Export
import LineageFlow.Algorithm.UI as Export

import LineageFlow.Algorithm.Classes as Export
import LineageFlow.Algorithm.Classes.P as Export
import LineageFlow.Algorithm.Classes.I as Export
import LineageFlow.Algorithm.Classes.O as Export
import LineageFlow.Algorithm.Classes.PD as Export
import LineageFlow.Algorithm.Classes.MD as Export

import Data.FileEmbed
import Language.Haskell.TH.Syntax

embed :: FilePath -> Q Exp
embed = embedStringFile
