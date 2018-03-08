{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Semigroup.Union
    ( module Data.Semigroup.Reducer
    -- * Unions of Containers
    , HasUnion(..)
    , HasUnion0(..)
    , Union(Union,getUnion)
    -- * Unions of Containers of Semigroups
    , HasUnionWith(..)
    , HasUnionWith0(..)
    , UnionWith(UnionWith,getUnionWith)
    ) where

import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)

import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.List as List

import Data.Hashable
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
import Data.Foldable
import Data.Traversable
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Semigroup.Reducer
import Data.Semigroup.Instances ()

-- | A Container suitable for the 'Union' 'Monoid'
class HasUnion f where
  union :: f -> f -> f

{-# SPECIALIZE union :: IntMap a -> IntMap a -> IntMap a #-}
{-# SPECIALIZE union :: Ord k => Map k a -> Map k a -> Map k a #-}
{-# SPECIALIZE union :: Eq a => [a] -> [a] -> [a] #-}
{-# SPECIALIZE union :: Ord a => Set a -> Set a -> Set a #-}
{-# SPECIALIZE union :: IntSet -> IntSet -> IntSet #-}
{-# SPECIALIZE union :: Eq a => HashSet a -> HashSet a -> HashSet a #-}
{-# SPECIALIZE union :: Eq k => HashMap k a -> HashMap k a -> HashMap k a #-}

class HasUnion f => HasUnion0 f where
  empty :: f

instance HasUnion (IntMap a) where
  union = IntMap.union

instance HasUnion0 (IntMap a) where
  empty = IntMap.empty

instance (Eq k, Hashable k) => HasUnion (HashMap k a) where
  union = HashMap.union

instance (Eq k, Hashable k) => HasUnion0 (HashMap k a) where
  empty = HashMap.empty

instance Ord k => HasUnion (Map k a) where
  union = Map.union

instance Ord k => HasUnion0 (Map k a) where
  empty = Map.empty

instance Eq a => HasUnion [a] where
  union = List.union

instance Eq a => HasUnion0 [a] where
  empty = []

instance Ord a => HasUnion (Set a) where
  union = Set.union

instance Ord a => HasUnion0 (Set a) where
  empty = Set.empty

instance HasUnion IntSet where
  union = IntSet.union

instance HasUnion0 IntSet where
  empty = IntSet.empty

instance (Eq a, Hashable a) => HasUnion (HashSet a) where
  union = HashSet.union

instance (Eq a, Hashable a) => HasUnion0 (HashSet a) where
  empty = HashSet.empty


-- | The 'Monoid' @('union','empty')@
newtype Union f = Union { getUnion :: f }
  deriving (Eq,Ord,Show,Read)

instance HasUnion f => Semigroup (Union f) where
  Union a <> Union b = Union (a `union` b)

instance HasUnion0 f => Monoid (Union f) where
  Union a `mappend` Union b = Union (a `union` b)
  mempty = Union empty

instance HasUnion f => Reducer f (Union f) where
  unit = Union

instance Functor Union where
  fmap f (Union a) = Union (f a)

instance Foldable Union where
 foldMap f (Union a) = f a

instance Traversable Union where
  traverse f (Union a) = Union <$> f a

instance Foldable1 Union where
  foldMap1 f (Union a) = f a

instance Traversable1 Union where
  traverse1 f (Union a) = Union <$> f a

-- | Polymorphic containers that we can supply an operation to handle unions with
class Functor f => HasUnionWith f where
  unionWith :: (a -> a -> a) -> f a -> f a -> f a

{-# SPECIALIZE unionWith :: (a -> a -> a) -> IntMap a -> IntMap a -> IntMap a #-}
{-# SPECIALIZE unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a #-}
{-# SPECIALIZE unionWith :: Eq k => (a -> a -> a) -> HashMap k a -> HashMap k a -> HashMap k a #-}

class HasUnionWith f => HasUnionWith0 f where
  emptyWith :: f a

instance HasUnionWith IntMap where
  unionWith = IntMap.unionWith

instance HasUnionWith0 IntMap where
  emptyWith = IntMap.empty

instance Ord k => HasUnionWith (Map k) where
  unionWith = Map.unionWith

instance Ord k => HasUnionWith0 (Map k) where
  emptyWith = Map.empty

instance (Eq k, Hashable k) => HasUnionWith (HashMap k) where
  unionWith = HashMap.unionWith

instance (Eq k, Hashable k) => HasUnionWith0 (HashMap k) where
  emptyWith = HashMap.empty

-- | The 'Monoid' @('unionWith mappend','empty')@ for containers full of monoids.
newtype UnionWith f m = UnionWith { getUnionWith :: f m }

instance (HasUnionWith f, Semigroup m) => Semigroup (UnionWith f m) where
    UnionWith a <> UnionWith b = UnionWith (unionWith (<>) a b)

instance (HasUnionWith0 f, Monoid m) => Monoid (UnionWith f m) where
    mempty = UnionWith emptyWith
    UnionWith a `mappend` UnionWith b = UnionWith (unionWith mappend a b)

instance (HasUnionWith f, Semigroup m, Monoid m) => Reducer (f m) (UnionWith f m) where
    unit = UnionWith

