{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Alt
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A semigroup for working 'Alt' or 'Plus'
--
-----------------------------------------------------------------------------

module Data.Semigroup.Alt
    ( Alter(..)
    ) where

import Data.Functor.Plus
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Reducer (Reducer(..))

-- | A 'Alter' turns any 'Alt' instance into a 'Semigroup'.

newtype Alter f a = Alter { getAlter :: f a }
    deriving (Functor,Alt,Plus)

instance Alt f => Semigroup (Alter f a) where
    Alter a <> Alter b = Alter (a <!> b)

instance Plus f => Monoid (Alter f a) where
    mempty = zero
    Alter a `mappend` Alter b = Alter (a <!> b)

instance Alt f => Reducer (f a) (Alter f a) where
    unit = Alter
