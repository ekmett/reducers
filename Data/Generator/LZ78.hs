{-# LANGUAGE TypeFamilies, BangPatterns, ParallelListComp #-} 
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.LZ78
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- Compression algorithms are all about exploiting redundancy. When applying
-- an expensive 'Reducer' to a redundant source, it may be better to 
-- extract the structural redundancy that is present. 'LZ78' is a compression
-- algorithm that does so, without requiring the dictionary to be populated
-- with all of the possible values of a data type unlike its later 
-- refinement LZW, and which has fewer comparison reqirements during encoding
-- than its earlier counterpart LZ77. 
-----------------------------------------------------------------------------

module Data.Generator.LZ78 
    ( 
    -- * Lempel-Ziv 78 
      LZ78
    -- * Encoding
    , encode    -- /O(n)/
    , encodeOrd -- /O(n log n)/
    , encodeEq  -- /O(n^2)/
    -- * Decoding (reduce)
    , decode
    -- * Recoding
    , recode    -- /O(n)/
    , recodeOrd -- /O(n log n)/
    , recodeEq  -- /O(n^2)/
    ) where

import Control.Applicative
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
import Data.Key as Key
import Data.Pointed
import Text.Read
import Control.Comonad
import Data.Hashable
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..), WrappedMonoid(..))
import Data.Semigroup.Reducer (Reducer(..), Count(..))

data Token a = Token {-# UNPACK #-} !Int a deriving (Eq, Ord)

instance Functor Token where
  fmap f (Token i a) = Token i (f a)

instance Foldable Token where
  foldMap f (Token _ a) = f a

instance Traversable Token where
  traverse f (Token i a) = Token i <$> f a

instance Extend Token where
  extend f t@(Token i _) = Token i (f t)
  duplicate t@(Token i _) = Token i t

instance Comonad Token where
  extract (Token _ a) = a

instance Hashable a => Hashable (Token a) where
  hash (Token i a) = hashWithSalt i a


-- | An LZ78 compressed 'Generator'.
data LZ78 a 
  = Cons {-# UNPACK #-} !(Token a) (LZ78 a) 
  | Nil

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

instance Functor LZ78 where
  fmap f (Cons (Token i a) as) = Cons (Token i (f a)) (fmap f as) 
  fmap _ Nil = Nil
  a <$ xs = go 0 (getCount (reduce xs)) where
     go !_ 0 = Nil
     go k  n | n > k = Cons (Token k a) (go (k + 1) (n - k - 1))
             | otherwise = Cons (Token (n - 1) a) Nil

instance Pointed LZ78 where
  point a = Cons (Token 0 a) Nil

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

data Entry i a = Entry i a

instance Functor (Entry i) where
  fmap f (Entry i a) = Entry i (f a)

instance Extend (Entry i) where
  extend f e@(Entry i _) = Entry i (f e)
  duplicate e@(Entry i _) = Entry i e

instance Comonad (Entry i) where
  extract (Entry _ a) = a

instance Eq i => Eq (Entry i a) where
  Entry i _ == Entry j _ = i == j

instance Ord i => Ord (Entry i a) where
  compare (Entry i _) (Entry j _) = compare i j

instance Hashable i => Hashable (Entry i a) where
  hash (Entry i _) = hash i
  hashWithSalt n (Entry i _) = hashWithSalt n i

-- | exposes internal structure
entries :: LZ78 a -> LZ78 (Entry Int a)
entries = go 0 where
  go k (Cons (Token i t) xs) = Cons (Token i (Entry k t)) $ (go $! k + 1) xs
  go _ Nil = Nil

instance Applicative LZ78 where
  pure a = Cons (Token 0 a) Nil
  fs <*> as = extract <$> encode 
    [ Entry (i,j) (f a) 
    | Entry i f <- decode (entries fs)
    , Entry j a <- decode (entries as) 
    ]
  as *> bs = fmap extract $ encode $ Prelude.concat $ replicate (reduceWith getCount as)  $  decode (entries bs)
  as <* bs = fmap extract $ encode $ Prelude.concat $ replicate (reduceWith getCount bs) <$> decode (entries as)

instance Monad LZ78 where
  return a = Cons (Token 0 a) Nil
  (>>) = (*>)
  as >>= k = extract <$> encode 
    [ Entry (i,j) b 
    | Entry i a <- decode (entries as)
    , Entry j b <- decode (entries (k a))
    ]

type instance Key LZ78 = Int

instance Lookup LZ78 where
  lookup i xs = Key.lookup i (decode xs)
  
instance Indexable LZ78 where
  index xs i = index (decode xs) i

instance FoldableWithKey LZ78 where 
  foldMapWithKey f xs = foldMapWithKey f (decode xs)

instance Zip LZ78 where
  zipWith f as bs = extract <$> encode 
    [ Entry (i,j) (f a b)
    | Entry i a <- decode (entries as)
    | Entry j b <- decode (entries bs)
    ] 
