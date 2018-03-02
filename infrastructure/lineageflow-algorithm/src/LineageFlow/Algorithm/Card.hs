{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module LineageFlow.Base.Card
  (
  -- * Cardinalities
    Card (..)
  , (:%) (..)
  ) where

import LineageFlow.Base.Imports
import Data.Char (toLower)


