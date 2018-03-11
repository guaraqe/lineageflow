{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module LineageFlow.Viewer.Input
  ( Measures (..)
  , measures_scalar
  , measures_vector
  , measures_tensor
  , measures_group
  , measures_tri
  , measures_triScalar
  , measuresTime
  , getMeasures
  , getLabels
  , TCMapA
  , TSMapA
  , positionCenter
  ) where

import LineageFlow.Prelude hiding (range)
import LineageFlow.Algorithm
import LineageFlow.Viewer.Interface
import LineageFlow.IO.CBOR

import Statistics.Sample (mean, range)

import Data.Yaml (FromJSON (..), ToJSON (..))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)

import Control.Lens
import GHC.Exts (Constraint)

--------------------------------------------------------------------------------

type TCMapA a = DSumMapA Time Cell a
type TSMapA a = DSumMapA Time (DS2 Time Cell) a

data Measures = Measures
  { _measures_scalar :: Map Text (TCMapA Scalar)
  , _measures_vector :: Map Text (TCMapA Vector)
  , _measures_tensor :: Map Text (TCMapA Tensor)
  , _measures_group :: Map Text (TCMapA Int)
  , _measures_tri :: Maybe (Map Text (TSMapA (S2 (Dep Time Cell))))
  , _measures_triScalar :: Maybe (Map Text (TSMapA Scalar))
  } deriving Generic

$(makeLenses ''Measures)

measuresTime :: Measures -> Int
measuresTime (Measures _ vector _ _ _ _) =
  let (Compose m) = snd . head $ Map.toList vector
  in _length m

getLabels :: Measures -> ([Text], [Text], [Text], [Text], [Text], [Text])
getLabels (Measures scalar vector tensor group tri triScalar) =
  ( Map.keys scalar
  , Map.keys vector
  , Map.keys tensor
  , Map.keys group
  , maybe [] Map.keys tri
  , maybe [] Map.keys triScalar
  )

--------------------------------------------------------------------------------

queryMeasure ::
  forall f a kg kp.
  (Domain f, Codomain a, kg f a) =>
  IOMethod kg kp -> (Text, FilePath) -> IO (Text, f a)
queryMeasure method (name,path) = do
  m <- io_get method path
  return (name, m)

queryMap ::
  forall f a kg kp.
  (Domain f, Codomain a, kg f a) =>
  IOMethod kg kp -> [(Text,FilePath)] -> IO (Map Text (f a))
queryMap db l =
  Map.fromList <$> traverse (queryMeasure db) l

--------------------------------------------------------------------------------

getMeasures :: InputCon kg => IOMethod kg kp -> VPath -> IO Measures
getMeasures db input =
  Measures <$>
    queryMap db (input ^. viewer_scalar) <*>
    queryMap db (input ^. viewer_vector) <*>
    queryMap db (input ^. viewer_tensor) <*>
    queryMap db (input ^. viewer_group) <*>
    traverse (queryMap db . pure) (input ^. viewer_tri) <*>
    traverse (queryMap db) (input ^. viewer_triScalar)

type InputCon (kg :: (* -> *) -> * -> Constraint) =
  ( kg (BArray Time .:. Array (Dep Time Cell)) Scalar
  , kg (BArray Time .:. Array (Dep Time Cell)) Vector
  , kg (BArray Time .:. Array (Dep Time Cell)) Tensor
  , kg (BArray Time .:. Array (Dep Time Cell)) Int
  , kg (BArray Time .:. Array (Dep Time (DS2 Time Cell))) (S2 (Dep Time Cell))
  , kg (BArray Time .:. Array (Dep Time (DS2 Time Cell))) Scalar
  )

positionCenter :: TCMapA Vector -> (Vector, Double)
positionCenter (Compose vv) =
  let
    v = vv ! (Time 0)
    x = _fmap (view _x) v
    y = _fmap (view _y) v
    z = _fmap (view _z) v
  in
    ( V3 (mean x) (mean y) (mean z)
    , maximum [range x, range y, range z]
    )
