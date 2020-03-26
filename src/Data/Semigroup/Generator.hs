{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, CPP #-}
#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 710
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Generator
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A 'Generator1' @c@ is a possibly-specialized container, which contains values of
-- type 'Elem' @c@, and which knows how to efficiently apply a 'Reducer' to extract
-- an answer.
--
-- 'Generator1' is to 'Generator' as 'Foldable1' is to 'Foldable'.
-----------------------------------------------------------------------------

module Data.Semigroup.Generator
  (
  -- * Generators
    Generator1(..)
  -- * Combinators
  , reduce1
  , mapReduceWith1
  , reduceWith1
  ) where

import Data.List.NonEmpty
import Data.Semigroup.Foldable
import Data.Semigroup.Reducer
import Data.Generator

-- #if !(MIN_VERSION_base(4,8,0))
-- import Data.Monoid (Monoid(..))
-- import Data.Foldable (fold,foldMap)
-- #endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..)) -- , WrappedMonoid(..))
#endif

-- | minimal definition 'mapReduce1' or 'mapTo1'
class Generator c => Generator1 c where
  mapReduce1 :: Reducer e m => (Elem c -> e) -> c -> m
  mapTo1     :: Reducer e m => (Elem c -> e) -> m -> c -> m
  mapFrom1   :: Reducer e m => (Elem c -> e) -> c -> m -> m

  mapTo1 f m = (<>) m . mapReduce1 f
  mapFrom1 f = (<>) . mapReduce1 f

instance Generator1 (NonEmpty e) where
  mapReduce1 f = foldMap1 (unit . f)

{-
mapReduceDefault :: (Generator1 c, Reducer (Elem c) m, Monoid m) => (Elem c -> e) -> c -> m
mapReduceDefault f = unwrapMonoid . mapReduce1 f

mapToDefault :: (Generator1 c, Reducer (Elem c) m, Monoid m) => (Elem c -> e) -> m -> c -> m
mapToDefault f = unwrapMonoid . mapTo1 f

mapFromDefault :: (Generator1 c, Reducer (Elem c) m, Monoid m) => (Elem c -> e) -> m -> c -> m
mapFromDefault f = unwrapMonoid . mapFrom1 f
-}

-- | Apply a 'Reducer' directly to the elements of a 'Generator'
reduce1 :: (Generator1 c, Reducer (Elem c) m) => c -> m
reduce1 = mapReduce1 id
{-# SPECIALIZE reduce1 :: Reducer a m => NonEmpty a -> m #-}

mapReduceWith1 :: (Generator1 c, Reducer e m) => (m -> n) -> (Elem c -> e) -> c -> n
mapReduceWith1 f g = f . mapReduce1 g
{-# INLINE mapReduceWith1 #-}

reduceWith1 :: (Generator1 c, Reducer (Elem c) m) => (m -> n) -> c -> n
reduceWith1 f = f . reduce1
{-# INLINE reduceWith1 #-}
