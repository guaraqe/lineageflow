{-# LANGUAGE DeriveGeneric     #-}

module LineageFlow.Export.Format.Emb
  ( Emb (..)
  , toEmb
  , embFile
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.BArrayU as BArrayU
import qualified LineageFlow.ArrayU as ArrayU

import qualified Data.ByteString.Lazy           as Lazy
import           Data.Csv

--------------------------------------------------------------------------------

data Emb = Emb
  { emb_id_center :: Int
  , emb_id_relative :: Int
  , emb_x :: Double
  , emb_y :: Double
  , emb_z :: Double
  , emb_timestep :: Int
  , emb_row_mother :: Int
  , emb_validation :: Int
  , emb_id_mother :: Int
  } deriving (Show, Eq, Generic)

instance ToRecord Emb

-------------------------------------------------------------------------------

type CT' = DSumMapA Cell Time (Time, Dep Time Cell)

dummyEmb :: Int -> Emb
dummyEmb t = Emb 0 0 0 0 0 t 0 0 0

dummyEmbUpTo :: Int -> BArrayU Emb
dummyEmbUpTo n = _fmap dummyEmb $ BArrayU.enumFromN 0 n

getRow :: Cell -> Dep Cell Time -> CT' -> Mothers -> (Dep Time Cell, Dep Time Cell, Cell)
getRow c t ct mothers
  | t == Dep (Time 0) =
      case mothers ! c of
        Nothing -> (snd $ ct ! (c,t), Dep $ Cell $ 0, Cell $ -1)
        Just m ->
          ( snd $ ct ! (c,t)
          , snd $ ArrayU.last $ unIx $ getCompose ct ! c
          , m )
  | otherwise =
       ( snd $ ct ! (c,t)
       , snd $ ct ! (c,t')
       , c )
       where t' = over (dep . time) (\x -> x - 1) t

toEmbLocal ::
  CT' ->
  Mothers ->
  Time ->
  BArrayU (Cell, Dep Cell Time) ->
  BArrayU Vector ->
  BArrayU Emb
toEmbLocal ct mothers tim tc pos =
  let
    f (c,t) (V3 x y z) =
      let (i,mi,m) = getRow c t ct mothers
      in
        Emb
          (view cell c)
          (view cell $ view dep $ i)
          x y z
          (view time tim)
          (view cell $ view dep mi)
          (-1)
          (view cell m)
  in
    BArrayU.zipWith f tc pos

addTime :: Time -> BArrayU Emb -> BArrayU Emb
addTime t = BArrayU.cons (dummyEmb (view time t))

toEmb :: CT' -> Mothers -> TC -> Array Time Time -> TCMap Vector -> BArrayU Emb
toEmb ct mothers tc' time' position' =
  let
    time = toList $ unIx $ time'
    position = fmap (BArrayU.convert . unIx) $ unIx $ getCompose $ position'
    --selection = fmap (BArrayU.convert . unIx) $ unIx $ getCompose $ selection'
    tc = fmap (BArrayU.convert . unIx) $ unIx $ getCompose $ tc'

    t0 = _time $ head time

    l = zipWith3 (toEmbLocal ct mothers) time tc position
  in
    dummyEmbUpTo t0 <> BArrayU.concat (zipWith addTime time l)

--------------------------------------------------------------------------------

embFile :: BArrayU Emb -> Lazy.ByteString
embFile = encodeWith (defaultEncodeOptions {encDelimiter = fromIntegral (ord ';')} ) . toList

{-
testCT :: CT'
testCT = Compose $ fromList [
  fromList [(Time 0, Dep $ Cell 0)],
  fromList [(Time 1, Dep $ Cell 0)],
  fromList [(Time 1, Dep $ Cell 1)]
  ]

testMothers :: Mothers
testMothers = fromList [Nothing, Just $ Cell 0, Just $ Cell 0]

testTC :: TC
testTC = Compose $ fromList [
  fromList [(Cell 0,Dep $ Time 0)],
  fromList [(Cell 1,Dep $ Time 0),(Cell 2, Dep $ Time 0)]
  ]

testTime :: Array Time Time
testTime = fromList [Time 2, Time 3]

testPosition :: TCMap Vector
testPosition = Compose $ fromList [
  fromList [V3 0 0 0],
  fromList [V3 1 1 1, V3 2 2 2]
  ]

testSelection :: TCMap Int
testSelection = Compose $ fromList [
  fromList [0],
  fromList [1, 2]
  ]
-}
