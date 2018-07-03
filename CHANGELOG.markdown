next [????.??.??]
-----------------
* Fix some `RULES` warnings.
* Allow building with `containers-0.6`.

3.12.2
------
* Fix the build with `fingertree-0.1.2` or later. Since `fingertree-0.1.2` now
  provides a `Semigroup` instance for `FingerTree`, as a result
  `Data.Semigroup.Instances` no longer exports anything if building against
  `fingertree-0.1.2` or later with `base-4.9` or later.
* Removed a couple of redundant instance constraints
* GHC 8 compatibility
* `transformers` 0.5 compatibility

3.12.1
----
* Restored support for `semigroups` < 0.17.

3.12
----
* `semigroups` 0.17 support

3.11
----
* Fixed the name of the field accessor for Ap
* `semigroupoids` 5 support
* Builds without warnings on GHC 7.10

3.10
----
* Updated dependencies

3.0.2.1
-------
* Updated `array` dependency for compatibility with GHC 7.8

3.0.2
-----
* Claim to be Trustworthy

3.0.1
---
* Removed the upper bounds on my other package dependencies
