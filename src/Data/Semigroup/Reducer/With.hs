{-# LANGUAGE UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Reducer.With
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-----------------------------------------------------------------------------

module Data.Semigroup.Reducer.With
  ( WithReducer(..)
  ) where

import Data.FingerTree
import Data.Hashable
import Data.Semigroup.Reducer
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

-- | If @m@ is a @c@-"Reducer", then m is @(c `WithReducer` m)@-"Reducer"
--   This can be used to quickly select a "Reducer" for use as a 'FingerTree'
--   'measure'.

newtype WithReducer m c = WithReducer { withoutReducer :: c }
  deriving (Eq, Ord, Show, Read)

instance Hashable c => Hashable (WithReducer m c) where
  hashWithSalt n = hashWithSalt n . withoutReducer

instance Functor (WithReducer m) where
  fmap f = WithReducer . f . withoutReducer

instance Foldable (WithReducer m) where
  foldMap f = f . withoutReducer

instance Traversable (WithReducer m) where
  traverse f (WithReducer a) = WithReducer <$> f a

instance Foldable1 (WithReducer m) where
  foldMap1 f = f . withoutReducer

instance Traversable1 (WithReducer m) where
  traverse1 f (WithReducer a) = WithReducer <$> f a

instance Reducer c m => Reducer (WithReducer m c) m where
  unit = unit . withoutReducer

instance (Monoid m, Reducer c m) => Measured m (WithReducer m c) where
  measure = unit . withoutReducer
