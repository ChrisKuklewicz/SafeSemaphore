SafeSemaphore
=============

SafeSemaphore is a Haskell library replacing non-exception safe libraries like QSem, QSemN, SampleVar.

Tests (tests/TestKillSem.hs) are included to demonstrate the bugs that occur when blocked threads
are stopped with killThread.  The [GHC ticket
#3160](http://hackage.haskell.org/trac/ghc/ticket/3160) reports this bad behavior of the usual
QSem QSemN.

Guide to modules
================

Semaphore modules usually have a highly recommended 'with' combinator which takes an action to done
between 'wait' and 'signal'.  These are exception safe and vastly simpler that remembering to ensure
'signal' always gets called when needed.

Control.Concurrent.MSem
-----------------------

Replacement for QSem that handles killThread without breaking.  This also is nice in that only a few
MVars are allocated by 'new' and none are allocated while using the MSem.  Blocked waiters have a
FIFO guarantee.

Control.Concurrent.MSemN
------------------------

Fancy QSemN that handles killThread without breaking.  Like MSem only 'new' allocates MVars.  The
FIFO waiter gurantee means that a blocked waiter with a large value requested will prevent later
small requests from being succeeding even if they could be satisfied.  Internally this uses an
error handler with 'uninterruptibleMask_' to ensure interupting a blocked waiter does break the semaphore.

The code's comments carefully describes the invariants and gives justifications why they are
preserved.  If you think the source is still too difficult then I will try to improve it.

Control.Concurrent.MSemN2
-------------------------

Exactly like MSemN but instead of 'uninterruptibleMask_' for interrupted waiters this uses extra
checking by all waiters in case the preceding waiter was interrupted.  This adds a tryTakeMVar to
all waiters but avoids setting up an error handler for blocking ones.  Performance has not been
micro-benchmarked.

The code's comments carefully describes the invariants and gives justifications why they are
preserved.  If you think the source is still too difficult then I will try to improve it.


Control.Concurrent.MSampleVar
-----------------------------

Replacement for SampleVar that handles killThread without breaking.


Control.Concurrent.FairRWLock
-----------------------------

Provides a fair RWLock, similar to [one from Java](http://download.oracle.com/javase/7/docs/api/java/util/concurrent/locks/ReentrantReadWriteLock.html).

There are complicated policy choices that have to be made, see the haddock or source file for details.

This should be exception safe.

Control.Concurrent.SSem
-----------------------

This is a replacement for both QSem and QSemN that uses a very simple STM backend (see
Control.Concurrent.STM.SSem below).  It does not have a FIFO guarantee -- an unlucky waiter could
starve forever.  This does provide a sane way to tryWait or tryWaitN that will not block forever but
may not succeed.

This should be exception safe.

Control.Concurrent.STM.SSem
---------------------------

This is the very simple STM backend for Control.Concurrent.SSem.  This merely wraps a humble Int.
Exposing this might allow weird composition.

This should be exception safe.
