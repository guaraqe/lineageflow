{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module LineageFlow.IO.CBOR.Classes
  ( CBORGet (..)
  , CBORPut (..)
  ) where

import LineageFlow.Prelude
import qualified LineageFlow.BArrayU as BArrayU
import Codec.Serialise

--------------------------------------------------------------------------------
-- Get

class (Serialise (f a), Dom f a) => CBORGet f a where
  cborGetMeasurement :: FilePath -> IO (f a)

instance (Serialise (f a), Dom f a) => CBORGet f a where
  cborGetMeasurement = readFileDeserialise

--------------------------------------------------------------------------------
-- Put

class (Serialise (f a), Dom f a) => CBORPut f a where
  cborPutMeasurement :: FilePath -> f a -> IO ()

instance (Serialise (f a), Dom f a) => CBORPut f a where
  cborPutMeasurement = writeFileSerialise

--------------------------------------------------------------------------------
-- Domain

instance (Storable a, Serialise a) => Serialise (Array i a) where
  encode = encode . unIx
  decode = mkIx <$> decode
  encodeList = encodeList . fmap unIx
  decodeList = fmap mkIx <$> decodeList

instance Serialise a => Serialise (BArray i a) where
  encode = encode . unIx
  decode = mkIx <$> decode
  encodeList = encodeList . fmap unIx
  decodeList = fmap mkIx <$> decodeList

-- This instance is made so that the serialisation of lists and arrays is the
-- same, fusion takes care of making it irrelevant.
instance Serialise a => Serialise (List i a) where
  encode = encode . BArrayU.fromList . unIx
  decode = (mkIx . BArrayU.toList) <$> decode

deriving instance Serialise (f (g a)) => Serialise (Compose f g a)

--------------------------------------------------------------------------------
-- Codomain

instance Serialise a => Serialise (V3 a)

deriving instance Serialise Cell
deriving instance Serialise Time
deriving instance Serialise j => Serialise (Dep i j)

instance Serialise a => Serialise (S2 a)
instance Serialise a => Serialise (S3 a)
instance Serialise a => Serialise (S4 a)

instance Serialise Sign
