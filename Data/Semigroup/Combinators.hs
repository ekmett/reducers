{-# LANGUAGE UndecidableInstances, TypeOperators, FlexibleContexts, MultiParamTypeClasses, FlexibleInstances, TypeFamilies, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Combinators
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (type families, MPTCs)
--
-- Utilities for working with Semigroups that conflict with names from the "Prelude",
-- "Data.Foldable", "Control.Monad" or elsewhere. Intended to be imported qualified.
--
-- > import Data.Semigroup.Combinators as Semigroup
--
-----------------------------------------------------------------------------

module Data.Semigroup.Combinators
    ( repeat
    , replicate1p
    , cycle
    ) where

import Prelude hiding (replicate, cycle, repeat)
import Data.Semigroup
import Data.Semigroup.Reducer

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'. May fail to terminate for some values in some semigroups.
cycle :: Semigroup m => m -> m
cycle xs = xs' where xs' = xs <> xs'

-- | A generalization of 'Data.List.repeat' to an arbitrary 'Semigroup'. May fail to terminate for some values in some semigroups.
repeat :: Reducer e m => e -> m 
repeat x = xs where xs = cons x xs 

-- | A generalization of 'Data.List.replicate' to an arbitrary 'Semigroup'. Adapted from 
-- <http://augustss.blogspot.com/2008/07/lost-and-found-if-i-write-108-in.html>
replicate1p :: (Semigroup m, Integral n) => m -> n -> m
replicate1p x0 y0 
    | y0 < 0 = error "Data.Semigroup.Combinators.replicate1p: negative length"
    | otherwise = f x0 $! y0 + 1
    where
        f x y 
            | even y = f (x `mappend` x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) x
        g x y z 
            | even y = g (x `mappend` x) (y `quot` 2) z
            | y == 1 = x `mappend` z
            | otherwise = g (x `mappend` x) ((y - 1) `quot` 2) (x `mappend` z)
{-# INLINE replicate1p #-}
