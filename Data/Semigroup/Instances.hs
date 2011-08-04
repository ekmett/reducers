{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Semigroup.Instances where

import Data.FingerTree
import Data.Semigroup
import Data.Monoid

instance Measured v a => Semigroup (FingerTree v a) where
  (<>) = mappend
