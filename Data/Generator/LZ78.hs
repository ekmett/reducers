{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.Compressed.LZ78
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. 'LZ78' is a compression
-- algorithm that does so, without requiring the dictionary to be populated
-- with all of the possible values of a data type unlike its later 
-- refinement LZW, and which has fewer comparison reqirements during encoding
-- than its earlier counterpart LZ77. Since we aren't storing these as a 
-- bitstream the LZSS refinement of only encoding pointers once you cross
-- the break-even point is a net loss. 
-----------------------------------------------------------------------------


module Data.Generator.LZ78 
    ( module Data.Generator
    -- * Lempel-Ziv 78 
    , LZ78
    -- * Encoding
    , encode
    , encodeEq
    , encodeHashable
    -- * Decoding (reduce)
    , decode
    -- * Recoding
    , recode
    , recodeEq
    , recodeHashable
    ) where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq,(|>))
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import qualified Data.List as List
import Data.Generator
import Data.Foldable
import Data.Function (on)
import Data.Functor
import Data.Hashable
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..), WrappedMonoid(..))
import Data.Semigroup.Reducer (Reducer(..), Count(..))

data Token a = Token {-# UNPACK #-} !Int a deriving (Eq, Ord)

instance Hashable a => Hashable (Token a) where
  hash (Token i a) = hashWithSalt i a

-- | An LZ78 compressed 'Generator'.
data LZ78 a 
  = Cons {-# UNPACK #-} !(Token a) (LZ78 a) 
  | Nil
--  | Fork (LZ78 a) (LZ78 a)
--  | Reset (LZ78 a)
--  | Replicate {-# UNPACK #-} !Int (LZ78 a)
  deriving (Eq,Or)

instance Show a => Show (LZ78 a) where
  showsPrec d xs = showParen (d > 10) $ 
    showString "encode " . showsPrec 11 (toList xs)

instance Eq a => Eq (LZ78 a) where
  (==) = (==) `on` decode

instance Ord a => Ord (LZ78 a) where
  compare = compare `on` decode

instance (Read a, Hashable a, Eq a) => Read (LZ78 a) where
  readPrec = parens $ prec 10 $ do
    Ident "encode" <- lexP
    encode <$> step readPrec


instance Generator (LZ78 a) where
  type Elem (LZ78 a) = a
  mapTo = go init where
    init = Seq.singleton mempty
    go _ _ m Nil = m
    go s f m (Cons (Token w c) ws) = m `mappend` go (s |> v) f v ws where 
      v = Seq.index s w `mappend`  unit (f c)
--    go s f m (Fork l r) = go init f (go s m f l) r
--    go _ f m (Reset ws) = go init f m ws
--    go _ _ m (Replicate 0 _)  = m
--    go s f m (Replicate n ws) = m `mappend` replicate1p (n-1) (go s f mempty ws)

-- follow up with recode if mapping a non-injective function
instance Functor LZ78 where
  fmap f (Cons (Token i a) as) = Cons (Token i (f a)) (fmap f as) 
  fmap _ Nil = Nil
  a <$ xs = Replicate (getCount (reduce xs)) (Cons 0 a Nil)

instance Foldable LZ78 where
  foldMap f = unwrapMonoid . mapReduce f
  fold      = unwrapMonoid . reduce

-- | /O(n)/ Construct an LZ78-compressed 'Generator' using a 'HashMap' internally.
encode :: (Hashable a, Eq a) => [a] -> LZ78 a
encode = go HashMap.empty 1 0 where
  go _ _ _ [] = Nil
  go _ _ p [c] = Cons (Token p c) Nil
  go d f p (c:cs) = let t = Token p c in case HashMap.lookup t d of
    Just p' -> go d f p' cs
    Nothing -> Cons t (go (HashMap.insert t f d) (succ f) 0 cs)

-- | /O(n log n)/ Contruct an LZ78-compressed 'Generator' using a 'Map' internally.
encodeOrd :: Ord a => [a] -> LZ78 a
encodeOrd = go Map.empty 1 0 where
  go _ _ _ [] = Nil
  go _ _ p [c] = Cons (Token p c) Nil
  go d f p (c:cs) = let t = Token p c in case Map.lookup t d of
    Just p' -> go d f p' cs
    Nothing -> Cons t (go (Map.insert t f d) (succ f) 0 cs)

-- | /O(n^2)/ Contruct an LZ78-compressed 'Generator' using a list internally, requires an instance of Eq, 
-- less efficient than encode.
encodeEq :: Eq a => [a] -> LZ78 a
encodeEq = go [] 1 0 where
  go _ _ _ [] = Nil
  go _ _ p [c] = Cons (Token p c) Nil
  go d f p (c:cs) = let t = Token p c in case List.lookup t d of
    Just p' -> go d f p' cs
    Nothing -> Cons t (go ((t, f):d) (succ f) 0 cs)

-- | A type-constrained 'reduce' operation
decode :: LZ78 a -> [a]
decode = reduce

-- | /O(n)/. Recompress with 'Hashable'
recode :: (Eq a, Hashable a) => LZ78 a -> LZ78 a 
recode = encode . decode 

-- | /O(n log n)/. Recompress with 'Ord'
recodeOrd :: Ord a => LZ78 a -> LZ78 a
recodeOrd = encodeOrd . decode

-- | /O(n^2)/. Recompress with 'Eq'
recodeEq :: Eq a => LZ78 a -> LZ78 a 
recodeEq = encodeEq . decode 

{-
instance Semigroup (LZ78 a) where
  Nil <> y    = y
  x <> Nil    = x
  x <> y      = Fork x y
  replicate1p n = Replicate (n + 1)

instance Monoid (LZ78 a) where
  mappend = (<>) 
  mempty  = Nil

-- deliberately not providing the reducer
instance (Eq a, Hashable a) => Reducer [a] (LZ78 a) where
  unit = encode

data LZ78Reducer a where
  Empty   :: LZ78Reducer
  Hashed  :: (Hashable a, Eq a) => LZ78 a -> LZ78Reducer a
  Equated :: Eq a              => LZ78 a -> LZ78Reducer a
  Ordered :: Ord a             => LZ78 a -> LZ78Reducer a

instance Semigroup (LZ78Reducer a) where
  Empty <> a = a
  a <> Empty = a
  Hashed l <> r = encode (decode l ++ decode r)
  Equated l <> r = encodeEq (decode l ++ decode r)
  Ordered l <> r = encodeOrd (decode l ++ decode r)

-- these would make for a more reducer-like API

data LZ78Builder a where
  Empty :: LZ78Builder a
  Hashed :: (Hashable a, Eq a) => !(HashMap (Token a) Int) -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> !(Seq (Token a)) -> LZ78Builder a
  Equated :: Eq a => [(Token a, Int)] -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> !(Seq (Token a)) -> LZ78Builder a
  Ordered :: Ord a => !(Map (Token a) Int) -> {-# UNPACK #-} !Int -> {-# UNPACK #-} !Int -> !(Seq (Token a)) -> LZ78Builder a
-}
