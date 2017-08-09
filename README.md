# hspec-multicheck

A testing framework for [Haskell](https://www.haskell.org) using [Hspec](http://hspec.github.io).
Basically the framework is identical to Hspec.
However, this framework is designed to execute all test cases using both, QuickCheck and Smallcheck.
Thus, instances for Arbitrary and Series might be required for variables used in test cases.

If you want to test all your properties using QuickCheck and smallcheck you may use this library to reduce code duplication.

Have a look at [ListSpec.hs](test/Data/ListSpec.hs) if you would like to see an example (e.g. for quickly getting started).

For further information you may have a look at the pages of [Hspec](http://hspec.github.io), [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/) and [smallcheck](https://www.cs.york.ac.uk/fp/smallcheck/).
