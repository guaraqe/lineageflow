{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Selection.Model
  ( SModel (SModel)
  , smodel_fixed
  , smodel_flexible
  , runSModel
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import Data.Map.Strict as Map (Map, lookup)

data SModel = SModel
  { _smodel_fixed :: Map Text Text
  , _smodel_flexible :: Map Text Text
  } deriving (Eq, Show)

$(makeLenses ''SModel)


instance InitState SModel where
 initState = SModel mempty mempty

runSModel :: MType -> [Text] -> SModel -> Maybe MQuery
runSModel mtype fields (SModel assoc1 assoc2) = do -- Maybe
  let assoc = assoc1 <> assoc2
  f <- traverse (flip Map.lookup assoc) fields
  return $ Query (Assoc $ zip fields f) mtype
