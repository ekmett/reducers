-- | Historically, this module defined an orphan @Semigroup@ instance for
-- @FingerTree@ on old versions of @base@. Nowadays, the @reducers@ library
-- no longer supports these old versions of @base@, so this module no longer
-- defines anything. As such, you should not need to import this.
module Data.Semigroup.Instances {-# DEPRECATED "This module no longer defines any instances. You should not need to import it." #-} where
