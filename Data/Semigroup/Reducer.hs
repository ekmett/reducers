{-# LANGUAGE UndecidableInstances , FlexibleContexts , MultiParamTypeClasses , FlexibleInstances , GeneralizedNewtypeDeriving, TypeOperators, ScopedTypeVariables, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Reducer
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A @c@-'Reducer' is a 'Semigroup' with a canonical mapping from @c@ to the Semigroup.
--
-----------------------------------------------------------------------------

module Data.Semigroup.Reducer
    ( Reducer(..)
    , foldMapReduce, foldMapReduce1
    , foldReduce, foldReduce1
    , pureUnit
    , returnUnit
    , Count(..)
    ) where

import Control.Applicative

import Data.Monoid as Monoid
import Data.Semigroup as Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Instances ()
import Data.Foldable
import Data.FingerTree

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import qualified Data.Map as Map
import Data.Map (Map)

--import Text.Parsec.Prim

-- | This type may be best read infix. A @c `Reducer` m@ is a 'Semigroup' @m@ that maps
-- values of type @c@ through @unit@ to values of type @m@. A @c@-'Reducer' may also
-- supply operations which tack-on another @c@ to an existing 'Monoid' @m@ on the left
-- or right. These specialized reductions may be more efficient in some scenarios
-- and are used when appropriate by a 'Generator'. The names 'cons' and 'snoc' work
-- by analogy to the synonymous operations in the list monoid.
--
-- This class deliberately avoids functional-dependencies, so that () can be a @c@-Reducer
-- for all @c@, and so many common reducers can work over multiple types, for instance,
-- First and Last may reduce both @a@ and 'Maybe' @a@. Since a 'Generator' has a fixed element
-- type, the input to the reducer is generally known and extracting from the monoid usually
-- is sufficient to fix the result type. Combinators are available for most scenarios where
-- this is not the case, and the few remaining cases can be handled by using an explicit 
-- type annotation.
--
-- Minimal definition: 'unit' or 'snoc'
class Semigroup m => Reducer c m where
  -- | Convert a value into a 'Semigroup'
  unit :: c -> m 
  -- | Append a value to a 'Semigroup' for use in left-to-right reduction
  snoc :: m -> c -> m
  -- | Prepend a value onto a 'Semigroup' for use during right-to-left reduction
  cons :: c -> m -> m 

  snoc m = (<>) m . unit
  cons = (<>) . unit

-- | Apply a 'Reducer' to a 'Foldable' container, after mapping the contents into a suitable form for reduction.
foldMapReduce :: (Foldable f, Monoid m, Reducer e m) => (a -> e) -> f a -> m
foldMapReduce f = foldMap (unit . f)

foldMapReduce1 :: (Foldable1 f, Reducer e m) => (a -> e) -> f a -> m
foldMapReduce1 f = foldMap1 (unit . f)

-- | Apply a 'Reducer' to a 'Foldable' mapping each element through 'unit'
foldReduce :: (Foldable f, Monoid m, Reducer e m) => f e -> m
foldReduce = foldMap unit

-- | Apply a 'Reducer' to a 'Foldable1' mapping each element through 'unit'
foldReduce1 :: (Foldable1 f, Reducer e m) => f e -> m
foldReduce1 = foldMap1 unit

returnUnit :: (Monad m, Reducer c n) => c -> m n 
returnUnit = return . unit

pureUnit :: (Applicative f, Reducer c n) => c -> f n
pureUnit = pure . unit

newtype Count = Count { getCount :: Int } deriving (Eq,Ord,Show,Read)

instance Semigroup Count where
  Count a <> Count b = Count (a + b)
  replicate1p n (Count a) = Count $ (fromIntegral n + 1) * a

instance Monoid Count where
  mempty = Count 0
  Count a `mappend` Count b = Count (a + b)

instance Reducer a Count where
  unit _ = Count 1
  Count n `snoc` _ = Count (n + 1)
  _ `cons` Count n = Count (n + 1)
  
instance (Reducer c m, Reducer c n) => Reducer c (m,n) where
  unit x = (unit x,unit x)
  (m,n) `snoc` x = (m `snoc` x, n `snoc` x)
  x `cons` (m,n) = (x `cons` m, x `cons` n)

instance (Reducer c m, Reducer c n, Reducer c o) => Reducer c (m,n,o) where
  unit x = (unit x,unit x, unit x)
  (m,n,o) `snoc` x = (m `snoc` x, n `snoc` x, o `snoc` x)
  x `cons` (m,n,o) = (x `cons` m, x `cons` n, x `cons` o)

instance (Reducer c m, Reducer c n, Reducer c o, Reducer c p) => Reducer c (m,n,o,p) where
  unit x = (unit x,unit x, unit x, unit x)
  (m,n,o,p) `snoc` x = (m `snoc` x, n `snoc` x, o `snoc` x, p `snoc` x)
  x `cons` (m,n,o,p) = (x `cons` m, x `cons` n, x `cons` o, x `cons` p)

instance Reducer c [c] where
  unit = return
  cons = (:)
  xs `snoc` x = xs ++ [x]

instance Reducer c () where
  unit _ = ()
  _ `snoc` _ = ()
  _ `cons` _ = ()

instance Reducer Bool Any where
  unit = Any

instance Reducer Bool All where
  unit = All

instance Reducer (a -> a) (Endo a) where
  unit = Endo

instance Semigroup a => Reducer a (Dual a) where
  unit = Dual
    
instance Num a => Reducer a (Sum a) where
  unit = Sum

instance Num a => Reducer a (Product a) where
  unit = Product

instance Ord a => Reducer a (Min a) where
  unit = Min

instance Ord a => Reducer a (Max a) where
  unit = Max

instance Reducer (Maybe a) (Monoid.First a) where
  unit = Monoid.First

instance Reducer a (Semigroup.First a) where
  unit = Semigroup.First

instance Reducer (Maybe a) (Monoid.Last a) where
  unit = Monoid.Last

instance Reducer a (Semigroup.Last a) where
  unit = Semigroup.Last

instance Measured v a => Reducer a (FingerTree v a) where
  unit = singleton
  cons = (<|)
  snoc = (|>) 

--instance (Stream s m t, Reducer c a) => Reducer c (ParsecT s u m a) where
--    unit = return . unit

instance Reducer a (Seq a) where
  unit = Seq.singleton
  cons = (Seq.<|)
  snoc = (Seq.|>)

instance Reducer Int IntSet where
  unit = IntSet.singleton
  cons = IntSet.insert
  snoc = flip IntSet.insert -- left bias irrelevant

instance Ord a => Reducer a (Set a) where
  unit = Set.singleton
  cons = Set.insert
  -- pedantic about order in case 'Eq' doesn't implement structural equality
  snoc s m | Set.member m s = s 
           | otherwise = Set.insert m s

instance Reducer (Int, v) (IntMap v) where
  unit = uncurry IntMap.singleton
  cons = uncurry IntMap.insert
  snoc = flip . uncurry . IntMap.insertWith $ const id

instance Ord k => Reducer (k, v) (Map k v) where
  unit = uncurry Map.singleton
  cons = uncurry Map.insert
  snoc = flip . uncurry . Map.insertWith $ const id

instance Monoid m => Reducer m (WrappedMonoid m) where
  unit = WrapMonoid
