{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module LineageFlow.Client.Viewer.Model
  ( VModel (VModel)
  , vmodel_selection
  , vmodel_scalar
  , vmodel_vector
  , vmodel_tensor
  , vmodel_group
  , vmodel_tri
  , vmodel_triScalar
  , CModel (CModel)
  , cmodel_selection
  , cmodel_choices
  , runVModel
  ) where

import LineageFlow.Client.Prelude
import LineageFlow.Client.Types
import LineageFlow.Client.Selection.Model

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

data CModel = CModel
  { _cmodel_selection :: Map Int Text
  , _cmodel_choices :: [Text]
  } deriving (Eq, Show)

$(makeLenses ''CModel)

instance InitState CModel where
  initState = CModel Map.empty []

data VModel = VModel
  { _vmodel_selection :: SModel
  , _vmodel_scalar :: CModel
  , _vmodel_vector :: CModel
  , _vmodel_tensor :: CModel
  , _vmodel_group :: CModel
  , _vmodel_tri :: CModel
  , _vmodel_triScalar :: CModel
  } deriving (Eq, Show)

$(makeLenses ''VModel)

instance InitState VModel where
  initState =
    VModel initState initState initState initState initState initState initState

runVModel :: VModel -> [Text] -> Maybe VQuery
runVModel vmodel fields = do -- Maybe
  let
    assoc = vmodel ^. vmodel_selection
    selected = assoc ^. smodel_fixed <> assoc ^. smodel_flexible

    ffields = filter (/= "measurement") fields
    ffieldsTri = filter (/= "subdomain") ffields

  f <- traverse (flip Map.lookup selected) ffields
  fTri <- traverse (flip Map.lookup selected) ffieldsTri

  let
    addFields a d t =
      Query (Assoc $ a <> zip ffields f) (MType d t)
    addFieldsTri a d t =
      Query (Assoc $ a <> zip ffields fTri) (MType d t)


    addVertex t n = addFields [("measurement",n)] "(time,[cell,time])" t

    scalars =
      fmap (addVertex "scalar")
        (Map.elems $ vmodel ^. vmodel_scalar . cmodel_selection)
    vectors =
      fmap (addVertex "vector")
        (("position" :) $ Map.elems $ vmodel ^. vmodel_vector . cmodel_selection)
    tensors =
      fmap (addVertex "tensor")
        (Map.elems $ vmodel ^. vmodel_tensor . cmodel_selection)

    groups =
      fmap (addVertex "number")
        (Map.elems $ vmodel ^. vmodel_group . cmodel_selection)

    addTri s =
      addFieldsTri
        [("subdomain",s), ("measurement","identity")]
        "(time,[s2 [cell,time],time])"
        "s2 [cell,time]"

    tri = fmap addTri (Map.lookup 1 $ vmodel ^. vmodel_tri . cmodel_selection)

    addTriScalar s n =
      addFieldsTri
        [("subdomain",s), ("measurement",n)]
        "(time,[s2 [cell,time],time])"
        "scalar"

    triScalars =
      fmap
        (\s -> fmap (addTriScalar s) (Map.elems $ vmodel ^. vmodel_triScalar . cmodel_selection))
        (Map.lookup 1 $ vmodel ^. vmodel_tri .cmodel_selection)

  return $ VQuery scalars vectors tensors groups tri triScalars


