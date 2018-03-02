module LineageFlow.Types.Simplex
  (
  -- * 1-simplexes
    S1 (..)
  , DS1
  , s1
  , s1normalize
  -- * 2-simplexes
  , S2 (..)
  , DS2
  , s2
  , s2normalize
  -- * 3-simplexes
  , S3 (..)
  , DS3
  , s3
  , s3normalize
  -- * 4-simplexes
  , S4 (..)
  , DS4
  , s4
  , s4normalize
  -- * Signatures
  , Sign (..)
  ) where

import LineageFlow.Types.Dep

import Control.Monad
import Control.Lens
import Data.Semigroup
import GHC.Generics (Generic)

import Foreign.Storable
import Foreign.Storable.Record as Store
import Foreign.Storable.Generic
import Data.Foldable (foldl')

import Control.Monad.Writer.Strict hiding ((<>))

import Control.Newtype

--------------------------------------------------------------------------------

swapW :: (Ord a) => Lens' b a -> Lens' b a -> Sign -> b -> Writer Sign b
swapW lx ly value s =
  let x = view lx s
      y = view ly s
  in writer $ if x > y
                 then (s, mempty)
                 else ((set lx y . set ly x) s, value)

composeM :: Monad m => [a -> m a] -> a -> m a
composeM = foldl' (>=>) return

----------------------------------------------------------------------

class HasV0 f a | f -> a where
  _v0 :: Lens' f a

class HasV1 f a | f -> a where
  _v1 :: Lens' f a

class HasV2 f a | f -> a where
  _v2 :: Lens' f a

class HasV3 f a | f -> a where
  _v3 :: Lens' f a

----------------------------------------------------------------------

-- | 'S1' represents 1-simplexes.
data S1 a = S1 !a
  deriving (Eq,Ord,Show,Read,Functor,Generic)

type DS1 i j = S1 (Dep i j)

instance HasV0 (S1 a) a where
  _v0 = lens (\(S1 v0) -> v0) (\(S1 _) n0 -> S1 n0)

s1 :: a -> S1 a
s1 = S1
{-# INLINE s1 #-}

s1normalize :: S1 a -> (S1 a, Sign)
s1normalize = \a -> (a,Plus)
{-# INLINE s1normalize #-}

instance Newtype a Int => Newtype (S1 a) Int where
   pack = S1 . pack
   {-# INLINE pack #-}
   unpack = \(S1 a) -> unpack a
   {-# INLINE unpack #-}

-- | Storable instances
storeS1 :: Storable a => Store.Dictionary (S1 a)
storeS1 = Store.run $
    S1 <$> Store.element a
    where a (S1 i) = i

instance Storable a => Storable (S1 a) where
    sizeOf = Store.sizeOf storeS1
    alignment = Store.alignment storeS1
    peek = Store.peek storeS1
    poke = Store.poke storeS1

----------------------------------------------------------------------

-- | 'S2' represents 2-simplexes.
data S2 a = S2 !a !a
  deriving (Eq,Ord,Show,Read,Functor,Generic)

type DS2 i j = S2 (Dep i j)

instance HasV0 (S2 a) a where
  _v0 = lens (\(S2 v0 _) -> v0) (\(S2 _ v1) n0 -> S2 n0 v1)

instance HasV1 (S2 a) a where
  _v1 = lens (\(S2 _ v1) -> v1) (\(S2 v0 _) n1 -> S2 v0 n1)

s2 :: Ord a => a -> a -> S2 a
s2 i j = fst $ s2normalize (S2 i j)
{-# INLINE s2 #-}

s2normalize :: Ord a => S2 a -> (S2 a,Sign)
s2normalize = \(S2 i j) ->
  if i > j
     then (S2 i j, Plus)
     else (S2 j i, Minus)
{-# INLINE s2normalize #-}

-- (i,j) with i > j to unique integer - lower diagonal
instance Newtype a Int => Newtype (S2 a) Int where
  pack n =
    let i = floor (sqrt (1 + 8* fromIntegral n :: Double) / 2)
    in S2 (pack i) (pack (n - choose2 i))
  {-# INLINE pack #-}
  unpack (S2 i j) =
    choose2 (unpack i) + unpack j
  {-# INLINE unpack #-}

choose2 :: Int -> Int
choose2 n = div (n*(n-1)) 2

--------------------------------------------------------------------------------
-- | Storable instances
storeS2 :: Storable a => Store.Dictionary (S2 a)
storeS2 = Store.run $
    S2 <$> Store.element a
       <*> Store.element b
    where a (S2 i _) = i
          b (S2 _ j) = j

instance Storable a => Storable (S2 a) where
    sizeOf = Store.sizeOf storeS2
    alignment = Store.alignment storeS2
    peek = Store.peek storeS2
    poke = Store.poke storeS2

----------------------------------------------------------------------

-- | 'S3' represents 3-simplexes.
data S3 a = S3 !a !a !a
  deriving (Eq,Ord,Show,Read,Functor,Generic)

type DS3 i j = S3 (Dep i j)

instance HasV0 (S3 a) a where
  _v0 = lens (\(S3 v0 _ _) -> v0) (\(S3 _ v1 v2) n0 -> S3 n0 v1 v2)

instance HasV1 (S3 a) a where
  _v1 = lens (\(S3 _ v1 _) -> v1) (\(S3 v0 _ v2) n1 -> S3 v0 n1 v2)

instance HasV2 (S3 a) a where
  _v2 = lens (\(S3 _ _ v2) -> v2) (\(S3 v0 v1 _) n2 -> S3 v0 v1 n2)

s3 :: Ord a => a -> a -> a -> S3 a
s3 i j k = fst $ s3normalize (S3 i j k)
{-# INLINE s3 #-}

s3normalize :: Ord a => S3 a -> (S3 a,Sign)
s3normalize =
  let
    swapList =
      [ swapW _v1 _v2 Minus
      , swapW _v0 _v2 Minus
      , swapW _v0 _v1 Minus ]
  in
    runWriter . composeM swapList
{-# INLINE s3normalize #-}

-- | Storable instances
storeS3 :: Storable a => Store.Dictionary (S3 a)
storeS3 = Store.run $
    S3 <$> Store.element a
       <*> Store.element b
       <*> Store.element c
    where a (S3 i _ _) = i
          b (S3 _ j _) = j
          c (S3 _ _ k) = k

instance Storable a => Storable (S3 a) where
    sizeOf = Store.sizeOf storeS3
    alignment = Store.alignment storeS3
    peek = Store.peek storeS3
    poke = Store.poke storeS3

----------------------------------------------------------------------

-- | 'S4' represents 4-simplexes.
data S4 a = S4 !a !a !a !a
  deriving (Eq,Ord,Show,Read,Functor,Generic)

type DS4 i j = S4 (Dep i j)

instance HasV0 (S4 a) a where
  _v0 = lens (\(S4 v0 _ _ _) -> v0) (\(S4 _ v1 v2 v3) n0 -> S4 n0 v1 v2 v3)

instance HasV1 (S4 a) a where
  _v1 = lens (\(S4 _ v1 _ _) -> v1) (\(S4 v0 _ v2 v3) n1 -> S4 v0 n1 v2 v3)

instance HasV2 (S4 a) a where
  _v2 = lens (\(S4 _ _ v2 _) -> v2) (\(S4 v0 v1 _ v3) n2 -> S4 v0 v1 n2 v3)

instance HasV3 (S4 a) a where
  _v3 = lens (\(S4 _ _ _ v3) -> v3) (\(S4 v0 v1 v2 _) n3 -> S4 v0 v1 v2 n3)

s4 :: Ord a => a -> a -> a -> a -> S4 a
s4 i j k l = fst $ s4normalize (S4 i j k l)
{-# INLINE s4 #-}

s4normalize :: Ord a => S4 a -> (S4 a, Sign)
s4normalize =
  let
    swapList =
      [ swapW _v0 _v2 Minus
      , swapW _v1 _v3 Minus
      , swapW _v0 _v1 Minus
      , swapW _v2 _v3 Minus
      , swapW _v1 _v2 Minus ]
  in runWriter . composeM swapList
{-# INLINE s4normalize #-}

-- Storable instances
storeS4 :: Storable a => Store.Dictionary (S4 a)
storeS4 = Store.run $
    S4 <$> Store.element a
       <*> Store.element b
       <*> Store.element c
       <*> Store.element d
    where a (S4 i _ _ _) = i
          b (S4 _ j _ _) = j
          c (S4 _ _ k _) = k
          d (S4 _ _ _ l) = l

instance Storable a => Storable (S4 a) where
    sizeOf = Store.sizeOf storeS4
    alignment = Store.alignment storeS4
    peek = Store.peek storeS4
    poke = Store.poke storeS4

----------------------------------------------------------------------

-- | 'Sign' represents the signature of a simplex. The 'Monoid'
-- instance represents multiplication.
data Sign = Plus | Minus
  deriving (Eq,Show,Read,Generic)

instance Storable Sign where
    sizeOf = sizeOfDefault
    alignment = alignmentDefault
    peek = peekDefault
    poke = pokeDefault

instance Semigroup Sign where
  Plus  <> Plus  = Plus
  Plus  <> Minus = Minus
  Minus <> Plus  = Minus
  Minus <> Minus = Plus

instance Monoid Sign where
  mempty  = Plus
  mappend = (<>)
