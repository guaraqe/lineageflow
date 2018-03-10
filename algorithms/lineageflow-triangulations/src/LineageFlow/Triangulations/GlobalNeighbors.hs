module LineageFlow.Triangulations.GlobalNeighbors
  ( globalNeighbors
  , globalNeighborsGenealogy
  ) where

import LineageFlow.Prelude

import qualified LineageFlow.ArrayU as ArrayU

import qualified Data.Set as Set

globalNeighbors ::
  TC ->
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  Array (S2 Cell) (S2 Cell)
globalNeighbors tc contacts =
  let
    contactsLocal t c =
      _fmap (\(S2 i j) -> fst $ s2normalize (S2 (fst $ t!i) (fst $ t!j))) c

    contactsGlobalIndex =
      _zipWith
         contactsLocal
        (getCompose tc)
        (getCompose contacts)

    contactsGlobalSet = Set.unions . unIx $
      _fmap (Set.fromList . toList) contactsGlobalIndex

  in
    mkIx $ fromList $ Set.toList contactsGlobalSet

globalNeighborsGenealogy ::
  Mothers ->
  TC ->
  DSumMapL Time (DS2 Time Cell) (DS2 Time Cell) ->
  Array (S2 Cell) (S2 Cell)
globalNeighborsGenealogy mothers tc contacts = mkIx $
  (unIx $ globalNeighbors tc contacts) <> (unIx $ genealogy mothers)


genealogy :: Mothers -> Array (S2 Cell) (S2 Cell)
genealogy mothers = mkIx $
  _concatMap genealogyOf $ unIx $ _imap const mothers
  where
    genealogyOf i = _fmap (s2 i) (ancestorsOf i mothers)

ancestorsOf :: Cell -> Array Cell (Maybe Cell) -> ArrayU Cell
ancestorsOf c mothers =
  case mothers ! c of
    Nothing -> ArrayU.empty
    Just i -> ArrayU.cons i (ancestorsOf i mothers)
