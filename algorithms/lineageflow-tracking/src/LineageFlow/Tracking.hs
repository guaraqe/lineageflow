{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StrictData #-}

module LineageFlow.Tracking
  ( lineageSetFromTracking
  , module LineageFlow.Tracking.Types
  ) where

import LineageFlow.Prelude
import LineageFlow.Tracking.Types

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import qualified Data.RList as RList
import Data.RList (RList)

import Data.NonEmpty

import Control.Lens hiding ((&))

-------------------------------------------------------------------------------

data ChildrenNumber = Two | Other
  deriving (Show, Eq)

data Result =
  Result
    { _result_cell :: Cell
    , _result_cell_index ::  Dep Cell Time
    , _result_children_number :: ChildrenNumber
    } deriving (Show)

-------------------------------------------------------------------------------

data Status =
  Status
    { _status_mothers :: Map Cell Cell
    , _status_tc :: NonEmptyR RList (RList (Cell, Dep Cell Time))
    , _status_ct :: Map Cell [(Time, Dep Time Cell)]
    , _status_time :: Time
    , _status_time_index :: Dep Time Cell
    , _status_cell :: Cell
    , _status_result :: Map Int Result
    , _status_result_next :: Map Int Result
    } deriving (Show)

$(makeLenses ''Status)

initialStatus :: Status
initialStatus =
  Status
    Map.empty
    (pure empty)
    Map.empty
    (Time 0)
    (Dep (Cell 0))
    (Cell 0)
    Map.empty
    Map.empty

nextStatus :: Status -> Status
nextStatus status =
  status
    & over (status_time . time) (+1)
    & set status_time_index (Dep (Cell 0))
    & set status_result (view status_result_next status)
    & set status_result_next Map.empty
    & over status_tc budgeR

-------------------------------------------------------------------------------

mapLast :: (a -> a) -> NonEmptyR f a -> NonEmptyR f a
mapLast f (xs :>: x) = xs :>: f x

transposeMap :: Ord a => Map a a -> Map a [a]
transposeMap =
      Map.toList
  >>> fmap (\(x,y) -> (y,[x]))
  >>> Map.fromListWith (<>)

-------------------------------------------------------------------------------

iterator
  :: Status
  -> Map Int ChildrenNumber
  -> Tracking
  -> Status
iterator status future (Tracking cell_id mother_id) =
  let
    past = Map.lookup mother_id (view status_result status)
    cnumber =
      case Map.lookup cell_id future of
        Nothing -> Other
        Just Other -> Other
        Just Two -> Two
  in
    case past of
      Nothing ->
        let
          this_cell = view status_cell status
          this_cell_index = Dep (Time 0)
          this_time = view status_time status
          this_time_index = view status_time_index status
        in
          status
            & over status_tc
              (mapLast (flip RList.snoc (this_cell,this_cell_index)))
            & over status_ct
              (Map.insert this_cell [(this_time,this_time_index)])
            & over (status_time_index . dep . cell) (+1)
            & over (status_cell . cell) (+1)
            & over status_result_next
              (Map.insert cell_id (Result this_cell this_cell_index cnumber))

      Just (Result mother_cell _ Two) ->
        let
          this_cell = view status_cell status
          this_cell_index = Dep (Time 0)
          this_time = view status_time status
          this_time_index = view status_time_index status
        in
          status
            & over status_mothers (Map.insert this_cell mother_cell)
            & over status_tc
              (mapLast (flip RList.snoc (this_cell,this_cell_index)))
            & over status_ct
              (Map.insert this_cell [(this_time,this_time_index)])
            & over (status_time_index . dep . cell) (+1)
            & over (status_cell . cell) (+1)
            & over status_result_next
              (Map.insert cell_id (Result this_cell this_cell_index cnumber))


      Just (Result mother_cell mother_cell_index Other) ->
        let
          this_cell = mother_cell
          this_cell_index = over (dep . time) (+1) mother_cell_index
          this_time = view status_time status
          this_time_index = view status_time_index status
        in
          status
            & over status_tc
              (mapLast (flip RList.snoc (this_cell,this_cell_index)))
            & over status_ct
              (Map.insertWith (<>) this_cell [(this_time,this_time_index)])
            & over (status_time_index . dep . cell) (+1)
            & over status_result_next
              (Map.insert cell_id (Result this_cell this_cell_index cnumber))

iterateFrame
  :: Status
  -> Map Int ChildrenNumber
  -> Array (Dep Time Cell) Tracking
  -> Status
iterateFrame status future = _foldl' (\st -> iterator st future) status


toChildrenNumber :: [Int] -> ChildrenNumber
toChildrenNumber [] = error "No child in Just, algorithm error"
toChildrenNumber (_:[]) = Other
toChildrenNumber (_:_:[]) = Two
toChildrenNumber (_:_:_) = Other


trackingToChildren :: Array (Dep Time Cell) Tracking -> Map Int ChildrenNumber
trackingToChildren =
    Map.map toChildrenNumber
  . Map.fromListWith (<>)
  . fmap (\(Tracking c m) -> (m,[c]))
  . toList

iterateTime :: [Array (Dep Time Cell) Tracking] -> Status
iterateTime tracking =
  let
    futures = tail (fmap trackingToChildren tracking) <> [Map.empty]
  in
    foldl'
      (\sta (fut,tra) -> nextStatus (iterateFrame sta fut tra))
      initialStatus
      (zip futures tracking)

-------------------------------------------------------------------------------

getLineageSet :: Status -> LineageSet
getLineageSet status =
  let
    sMothers = view status_mothers status
    sTC = view status_tc status
    sCT = view status_ct status
    sCell = view status_cell status
  in
    LineageSet
      (convertMothers sMothers sCell)
      (convertChildren (transposeMap sMothers) sCell)
      (convertTC sTC)
      (convertCT sCT)

convertMothers :: Map Cell Cell -> Cell -> Array Cell (Maybe Cell)
convertMothers cMothers (Cell total) =
  fromList $
    fmap (\i -> Map.lookup (Cell i) cMothers) [0 .. total - 1]

convertChildren :: Map Cell [Cell] -> Cell -> Array Cell (Maybe (Cell,Cell))
convertChildren cChildren (Cell total) =
  fromList $
    fmap (\i -> toChildren $ Map.lookup (Cell i) cChildren) [0 .. total - 1]

toChildren :: Maybe [Cell] -> Maybe (Cell,Cell)
toChildren Nothing = Nothing
toChildren (Just []) = error "No child in Just, algorithm error"
toChildren (Just (_:[])) = error "Only one child, algorithm error"
toChildren (Just (x:y:[])) = Just (x,y)
toChildren (Just (_:_:_)) = error "Too many children, non-supported data"

convertTC
  :: NonEmptyR RList (RList (Cell,Dep Cell Time))
  -> TC
convertTC = Compose . mkIx . fmap (fromList . RList.toList) . RList.toList . initR

convertCT :: Map Cell [(Time,Dep Time Cell)] -> CT
convertCT = Compose . mkIx . fmap (\(_,l) -> fromList (reverse l)) . Map.toAscList

-------------------------------------------------------------------------------

lineageSetFromTracking :: [Array (Dep Time Cell) Tracking] -> LineageSet
lineageSetFromTracking  = getLineageSet . iterateTime
