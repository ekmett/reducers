{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Self
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- A simple 'Monoid' transformer that takes a 'Monoid' m and produces a new @m@-Reducer named 'Self' @m@
-- 
-- This is useful when you have a generator that already contains monoidal values or someone supplies
-- the map to the monoid in the form of a function rather than as a "Reducer" instance. You can just
-- @'getSelf' . `reduce`@ or @'getSelf' . 'mapReduce' f@ in those scenarios. These behaviors are encapsulated 
-- into the 'fold' and 'foldMap' combinators in "Data.Monoid.Combinators" respectively.
--
-----------------------------------------------------------------------------

module Data.Semigroup.Self
    ( Self(..)
    )  where

import Control.Applicative
import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Semigroup.Reducer (Reducer(..))

newtype Self m = Self { getSelf :: m } deriving (Semigroup, Monoid)

instance Semigroup m => Reducer m (Self m) where
  unit = Self

instance Functor Self where
  fmap f (Self x) = Self (f x)

instance Foldable Self where
  foldMap f (Self x) = f x

instance Traversable Self where
  traverse f (Self x) = Self <$> f x

instance Foldable1 Self where
  foldMap1 f (Self x) = f x

instance Traversable1 Self where
  traverse1 f (Self x) = Self <$> f x
