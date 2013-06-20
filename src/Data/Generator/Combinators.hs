{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generator.Combinators
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, MPTCs)
--
-- Utilities for working with Monoids that conflict with names from the "Prelude",
-- "Data.Foldable", "Control.Monad" or elsewhere. Intended to be imported qualified.
--
-- > import Data.Generator.Combinators as Generator
--
-----------------------------------------------------------------------------

module Data.Generator.Combinators
    (
    -- * Monadic Reduction
      mapM_
    , forM_
    , msum
    -- * Applicative Reduction
    , traverse_
    , for_
    , asum
    -- * Logical Reduction
    , and
    , or
    , any
    , all
    -- * Monoidal Reduction
    , foldMap
    , fold
    , toList 
    -- * List-Like Reduction
    , concatMap
    , elem
    , filter
    , filterWith
    --, find
    , sum
    , product
    , notElem
    ) where

import Prelude hiding 
  ( mapM_, any, all, elem, filter, concatMap, and, or
  , sum, product, notElem, replicate, cycle, repeat
  )
import Control.Applicative
import Control.Monad (MonadPlus)
import Data.Generator
import Data.Monoid (Monoid(..))
import Data.Semigroup (Sum(..), Product(..), All(..), Any(..), WrappedMonoid(..))
import Data.Semigroup.Applicative (Traversal(..))
import Data.Semigroup.Alternative (Alternate(..))
import Data.Semigroup.Monad (Action(..))
import Data.Semigroup.MonadPlus (MonadSum(..))
import Data.Semigroup.Reducer (Reducer(..))

-- | Efficiently 'mapReduce' a 'Generator' using the 'Traversal' monoid. A specialized version of its namesake from "Data.Foldable"
--
-- @
--     'mapReduce' 'getTraversal'
-- @
traverse_ :: (Generator c, Applicative f) => (Elem c -> f b) -> c -> f ()
traverse_ = mapReduceWith getTraversal
{-# INLINE traverse_ #-}
    
-- | Convenience function as found in "Data.Foldable"
--
-- @
--     'flip' 'traverse_'
-- @
for_ :: (Generator c, Applicative f) => c -> (Elem c -> f b) -> f ()
for_ = flip traverse_
{-# INLINE for_ #-}

-- | The sum of a collection of actions, generalizing 'concat'
--
-- @
--    'reduceWith' 'getAlt'
-- @ 
asum :: (Generator c, Alternative f, f a ~ Elem c) => c -> f a
asum = reduceWith getAlternate
{-# INLINE asum #-}

-- | Efficiently 'mapReduce' a 'Generator' using the 'Action' monoid. A specialized version of its namesake from "Data.Foldable" and "Control.Monad"
-- 
-- @
--    'mapReduceWith' 'getAction'
-- @ 
mapM_ :: (Generator c, Monad m) => (Elem c -> m b) -> c -> m ()
mapM_ = mapReduceWith getAction
{-# INLINE mapM_ #-}

-- | Convenience function as found in "Data.Foldable" and "Control.Monad"
--
-- @
--     'flip' 'mapM_'
-- @
forM_ :: (Generator c, Monad m) => c -> (Elem c -> m b) -> m ()
forM_ = flip mapM_
{-# INLINE forM_ #-}

-- | The sum of a collection of actions, generalizing 'concat'
--
-- @
--     'reduceWith' 'getMonadSum'
-- @
msum :: (Generator c, MonadPlus m, m a ~ Elem c) => c -> m a
msum = reduceWith getMonadSum
{-# INLINE msum #-}

-- | Efficiently 'mapReduce' a 'Generator' using the 'WrappedMonoid' monoid. A specialized version of its namesake from "Data.Foldable"
--
-- @
--     'mapReduceWith' 'unwrapMonoid'
-- @
foldMap :: (Monoid m, Generator c) => (Elem c -> m) -> c -> m
foldMap = mapReduceWith unwrapMonoid
{-# INLINE foldMap #-}

-- | Type specialization of "foldMap" above
concatMap :: Generator c => (Elem c -> [b]) -> c -> [b]
concatMap = foldMap
{-# INLINE concatMap #-}

-- | Efficiently 'reduce' a 'Generator' using the 'WrappedMonoid' monoid. A specialized version of its namesake from "Data.Foldable"
--
-- @
--     'reduceWith' 'unwrapMonoid'
-- @
fold :: (Monoid m, Generator c, Elem c ~ m) => c -> m
fold = reduceWith unwrapMonoid
{-# INLINE fold #-}

-- | Convert any 'Generator' to a list of its contents. Specialization of 'reduce'
toList :: Generator c => c -> [Elem c]
toList = reduce
{-# INLINE toList #-}

-- | Efficiently 'reduce' a 'Generator' that contains values of type 'Bool'
--
-- @
--     'reduceWith' 'getAll'
-- @
and :: (Generator c, Elem c ~ Bool) => c -> Bool
and = reduceWith getAll
{-# INLINE and #-}

-- | Efficiently 'reduce' a 'Generator' that contains values of type 'Bool'
--
-- @
--     'reduceWith' 'getAny'
-- @
or :: (Generator c, Elem c ~ Bool) => c -> Bool
or = reduceWith getAny
{-# INLINE or #-}

-- | Efficiently 'mapReduce' any 'Generator' checking to see if any of its values match the supplied predicate
--
-- @
--     'mapReduceWith' 'getAny'
-- @
any :: Generator c => (Elem c -> Bool) -> c -> Bool
any = mapReduceWith getAny
{-# INLINE any #-}

-- | Efficiently 'mapReduce' any 'Generator' checking to see if all of its values match the supplied predicate
--
-- @
--     'mapReduceWith' 'getAll'
-- @
all :: Generator c => (Elem c -> Bool) -> c -> Bool
all = mapReduceWith getAll
{-# INLINE all #-}

-- | Efficiently sum over the members of any 'Generator'
--
-- @
--     'reduceWith' 'getSum'
-- @
sum :: (Generator c, Num (Elem c)) => c -> Elem c
sum = reduceWith getSum
{-# INLINE sum #-}

-- | Efficiently take the product of every member of a 'Generator'
--
-- @
--     'reduceWith' 'getProduct'
-- @
product :: (Generator c, Num (Elem c)) => c -> Elem c
product = reduceWith getProduct
{-# INLINE product #-}

-- | Check to see if 'any' member of the 'Generator' matches the supplied value
elem :: (Generator c, Eq (Elem c)) => Elem c -> c -> Bool
elem = any . (==)
{-# INLINE elem #-}

-- | Check to make sure that the supplied value is not a member of the 'Generator'
notElem :: (Generator c, Eq (Elem c)) => Elem c -> c -> Bool
notElem x = not . elem x
{-# INLINE notElem #-}

-- | Efficiently 'mapReduce' a subset of the elements in a 'Generator'
filter :: (Generator c, Reducer (Elem c) m, Monoid m) => (Elem c -> Bool) -> c -> m
filter p = foldMap f where
    f x | p x = unit x
        | otherwise = mempty
{-# INLINE filter #-}

-- | Allows idiomatic specialization of filter by proving a function that will be used to transform the output
filterWith :: (Generator c, Reducer (Elem c) m, Monoid m) => (m -> n) -> (Elem c -> Bool) -> c -> n 
filterWith f p = f . filter p
{-# INLINE filterWith #-}

{-

-- | A specialization of 'filter' using the 'First' 'Monoid', analogous to 'Data.List.find'
--
-- @
--     'filterWith' 'getFirst'
-- @
find :: Generator c => (Elem c -> Bool) -> c -> Maybe (Elem c)
find = filterWith getFirst
{-# INLINE find #-}

-}
