{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}

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
import Data.Semigroup.Reducer (Reducer(..))

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | A 'Alternate' turns any 'Alternative' instance into a 'Monoid'.

newtype Alternate f a = Alternate { getAlternate :: f a }
  deriving (Functor,Applicative,Alternative)

instance Alternative f => Semigroup (Alternate f a) where
  Alternate a <> Alternate b = Alternate (a <|> b)

instance Alternative f => Monoid (Alternate f a) where
  mempty = empty
#if !(MIN_VERSION_base(4,11,0))
  Alternate a `mappend` Alternate b = Alternate (a <|> b)
#endif

instance Alternative f => Reducer (f a) (Alternate f a) where
  unit = Alternate

