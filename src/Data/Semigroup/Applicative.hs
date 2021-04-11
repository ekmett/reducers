{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Applicative
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- Semigroups for working with 'Applicative' 'Functor's.
--
-----------------------------------------------------------------------------

module Data.Semigroup.Applicative
    ( Traversal(..)
    , Ap(..)
    ) where

import Control.Applicative
import Data.Semigroup.Reducer (Reducer(..))

#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif

#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

-- | A 'Traversal' uses an glues together 'Applicative' actions with (*>)
--   in the manner of 'traverse_' from "Data.Foldable". Any values returned by
--   reduced actions are discarded.
newtype Traversal f = Traversal { getTraversal :: f () }

instance Applicative f => Semigroup (Traversal f) where
  Traversal a <> Traversal b = Traversal (a *> b)

instance Applicative f => Monoid (Traversal f) where
  mempty = Traversal (pure ())
#if !(MIN_VERSION_base(4,11,0))
  Traversal a `mappend` Traversal b = Traversal (a *> b)
#endif

instance Applicative f => Reducer (f a) (Traversal f) where
  unit = Traversal . (() <$)
  a `cons` Traversal b = Traversal (a *> b)
  Traversal a `snoc` b = Traversal (() <$ (a *> b))

-- | Efficiently avoid needlessly rebinding when using 'snoc' on an action that already returns ()
--   A rewrite rule automatically applies this when possible
snocTraversal :: Reducer (f ()) (Traversal f) => Traversal f -> f () -> Traversal f
snocTraversal a = (<>) a . Traversal
{-# RULES "unitTraversal" unit = Traversal #-}
{-# RULES "snocTraversal" snoc = snocTraversal #-}

newtype Ap f m = Ap { getAp :: f m }
  deriving (Functor,Applicative)

instance (Applicative f, Semigroup m) => Semigroup (Ap f m) where
  (<>) = liftA2 (<>)

instance (Applicative f, Monoid m) => Monoid (Ap f m) where
  mempty = pure mempty
#if !(MIN_VERSION_base(4,11,0))
  mappend = liftA2 mappend
#endif

instance (Applicative f, Reducer c m) => Reducer (f c) (Ap f m) where
  unit = fmap unit . Ap
