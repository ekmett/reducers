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
-- Module      :  Data.Semigroup.Alternative
-- Copyright   :  (c) Edward Kmett 2009-2011
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- A semigroup for working with 'Alternative' 'Functor's.
--
-----------------------------------------------------------------------------

module Data.Semigroup.Alternative
    ( Alternate(..)
    ) where

import Control.Applicative
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Reducer (Reducer(..))

-- | A 'Alternate' turns any 'Alternative' instance into a 'Monoid'.

newtype Alternate f a = Alternate { getAlternate :: f a }
  deriving (Functor,Applicative,Alternative)

instance Alternative f => Semigroup (Alternate f a) where
  Alternate a <> Alternate b = Alternate (a <|> b)

instance Alternative f => Monoid (Alternate f a) where
  mempty = empty
  Alternate a `mappend` Alternate b = Alternate (a <|> b)

instance Alternative f => Reducer (f a) (Alternate f a) where
  unit = Alternate

