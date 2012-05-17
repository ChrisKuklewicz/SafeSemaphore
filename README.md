SafeSemaphore
=============

SafeSemaphore is a Haskell library replacing non-exception safe libraries like QSem, QSemN, SampleVar.

Tests are included to demonstrate the bugs that occur when blocked threads are stopped with killThread.  The [GHC ticket #3160](http://hackage.haskell.org/trac/ghc/ticket/3160) reports this bad behavior of the usual libraries.

As of May 2012: A new version using literate haskell is being prepared, as well as a second version that simply uses STM.
