{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.MonadPlus
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A semigroup for working with instances of 'MonadPlus'
--
-----------------------------------------------------------------------------

module Data.Semigroup.MonadPlus
    ( MonadSum(..)
    ) where

import Control.Monad (MonadPlus(..))
import Control.Applicative (Alternative(..))
#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))
#endif
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Reducer (Reducer(..))

-- | A 'MonadSum' turns any 'MonadPlus' instance into a 'Monoid'.

newtype MonadSum f a = MonadSum { getMonadSum :: f a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus)

instance MonadPlus f => Semigroup (MonadSum f a) where
  MonadSum a <> MonadSum b = MonadSum (mplus a b)

instance MonadPlus f => Monoid (MonadSum f a) where
  mempty = mzero
  MonadSum a `mappend` MonadSum b = MonadSum (mplus a b)

instance MonadPlus f => Reducer (f a) (MonadSum f a) where
  unit = MonadSum

