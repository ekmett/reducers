{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
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

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Data.Semigroup.Reducer (Reducer(..))

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

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

