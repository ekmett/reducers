{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Monad
-- Copyright   :  (c) Edward Kmett 2009
-- License     :  BSD-style
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (MPTCs)
--
-- Semigroups for working with 'Monad's.
--
-----------------------------------------------------------------------------

module Data.Semigroup.Monad
    ( Action(..)
    , Mon(..)
    ) where

import Control.Monad (liftM, liftM2)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))
#endif
import Data.Semigroup (Semigroup(..))
import Data.Semigroup.Reducer (Reducer(..))

-- | A 'Action' uses an glues together monadic actions with (>>)
--   in the manner of 'mapM_' from "Data.Foldable". Any values returned by
--   reduced actions are discarded.
newtype Action f = Action { getAction :: f () }

instance Monad f => Semigroup (Action f) where
  Action a <> Action b = Action (a >> b)

instance Monad f => Monoid (Action f) where
  mempty = Action (return ())
  Action a `mappend` Action b = Action (a >> b)

instance Monad f => Reducer (f a) (Action f) where
  unit a            = Action (a >> return ())
  a `cons` Action b = Action (a >> b)
  Action a `snoc` b = Action (a >> b >> return ())

-- | Efficiently avoid needlessly rebinding when using 'snoc' on an action that already returns ()
--   A rewrite rule automatically applies this when possible
snocAction :: Reducer (f ()) (Action f) => Action f -> f () -> Action f
snocAction a = (<>) a . Action
{-# RULES "unitAction" unit = Action #-}
{-# RULES "snocAction" snoc = snocAction #-}

newtype Mon f m = Mon { getMon :: f m }
  deriving (Functor,Applicative,Monad)

instance (Monad f, Semigroup m) => Semigroup (Mon f m) where
  (<>) = liftM2 (<>)

instance (Monad f, Monoid m) => Monoid (Mon f m) where
  mempty = return mempty
  mappend = liftM2 mappend

instance (Monad f, Reducer c m) => Reducer (f c) (Mon f m) where
  unit = liftM unit . Mon
