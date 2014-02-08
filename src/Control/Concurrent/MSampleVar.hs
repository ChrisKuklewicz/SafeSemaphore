{-# LANGUAGE DeriveDataTypeable, CPP #-}
--
-- Module      :  Control.Concurrent.MSampleVar
-- Copyright   :  (c) Chris Kuklewicz 2011
-- License     :  3 clause BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  haskell@list.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--

-- | 'MSampleVar' is a safer version of the "Control.Concurrent.SampleVar" in
-- base.  The same problem as QSem(N) is being fixed, that of handling waiters
-- that die before being woken normally.  For "Control.Concurrent.SampleVar" in
-- base this error can lead to thinking a full 'SampleVar' is really empty and
-- cause 'writeSampleVar' to hang.  The 'MSampleVar' in this module is immune
-- to this error, and has a simpler implementation.
--
module Control.Concurrent.MSampleVar
       ( -- * Sample Variables
         MSampleVar,
         newEmptySV, -- :: IO (MSampleVar a)
         newSV,      -- :: a -> IO (MSampleVar a)
         emptySV,    -- :: MSampleVar a -> IO ()
         readSV,     -- :: MSampleVar a -> IO a
         writeSV,    -- :: MSampleVar a -> a -> IO ()
         isEmptySV,  -- :: MSampleVar a -> IO Bool
       ) where

import Control.Monad(void,join)
import Control.Concurrent.MVar(MVar,newMVar,newEmptyMVar,tryTakeMVar,takeMVar,putMVar,withMVar,isEmptyMVar)
import Control.Exception(mask_)
import Data.Typeable

-- |
-- Sample variables are slightly different from a normal 'MVar':
-- 
--  * Reading an empty 'MSampleVar' causes the reader to block.
--    (same as 'takeMVar' on empty 'MVar')
-- 
--  * Reading a filled 'MSampleVar' empties it and returns value.
--    (same as 'takeMVar')
--
--  * Try reading a filled 'MSampleVar' returns a Maybe value.
--    (same as 'tryTakeMVar')
-- 
--  * Writing to an empty 'MSampleVar' fills it with a value, and
--    potentially, wakes up a blocked reader (same as for 'putMVar' on
--    empty 'MVar').
--
--  * Writing to a filled 'MSampleVar' overwrites the current value.
--    (different from 'putMVar' on full 'MVar'.)
--
-- The readers queue in FIFO order, with the lead reader joining the writers in
-- a second FIFO queue to access the stored value.  Thus writers can jump the
-- queue of non-leading waiting readers to update the value, but the lead
-- reader has to wait on all previous writes to finish before taking the value.
--
-- This design choice emphasises that each reader sees the most up-to-date
-- value possible while still guaranteeing progress.
data MSampleVar a = MSampleVar { readQueue :: MVar ()
                               , lockedStore :: MVar (MVar a) }
  deriving ( Eq
#if __GLASGOW_HASKELL__ >= 707
           , Typeable
#endif
           )

#if __GLASGOW_HASKELL__ < 707
instance Typeable1 MSampleVar where
  typeOf1 _ = mkTyConApp tc []
    where tc = mkTyCon "MSampleVar"
#endif


-- | 'newEmptySV' allocates a new MSampleVar in an empty state.  No futher
-- allocation is done when using the 'MSampleVar'.
newEmptySV :: IO (MSampleVar a)
newEmptySV = do
  newReadQueue <- newMVar ()
  newLockedStore <- newMVar =<< newEmptyMVar
  return (MSampleVar { readQueue = newReadQueue
                     , lockedStore = newLockedStore })

-- | 'newSV' allocates a new MSampleVar containing the passed value.  The value
-- is not evalated or forced, but stored lazily.  No futher allocation is done
-- when using the 'MSampleVar'.
newSV :: a -> IO (MSampleVar a)
newSV a = do
  newReadQueue <- newMVar ()
  newLockedStore <- newMVar =<< newMVar a
  return (MSampleVar { readQueue = newReadQueue
                     , lockedStore = newLockedStore })

-- | 'isEmptySV' can block and be interrupted, in which case it does nothing.
-- If 'isEmptySV' returns then it reports the momentary status the
-- 'MSampleVar'.  Using this value without producing unwanted race conditions
-- is left up to the programmer.
isEmptySV :: MSampleVar a -> IO Bool
isEmptySV (MSampleVar _ ls) = withMVar ls isEmptyMVar
  -- (withMVar ls) might block, interrupting is okay

-- | If the 'MSampleVar' is full, forget the value and leave it empty.
-- Otherwise, do nothing.  This avoids any the FIFO queue of blocked 'readSV'
-- threads.
--
-- 'emptySV' can block and be interrupted, in which case it does nothing.  If
-- 'emptySV' returns then it left the 'MSampleVar' in an empty state.
emptySV :: MSampleVar a -> IO ()
emptySV (MSampleVar _ ls) = withMVar ls (void . tryTakeMVar)
  -- (withMVar ls) might block, interrupting is okay

-- | Wait for a value to become available, then take it and return.  The queue
-- of blocked 'readSV' threads is a fair FIFO queue.
--
-- 'readSV' can block and be interrupted, in which case it takes nothing.  If
-- 'readSV returns normally then it has taken a value.
readSV :: MSampleVar a -> IO a
readSV (MSampleVar rq ls) =  mask_ $ withMVar rq $ \ () ->
  join $ withMVar ls (return . takeMVar)
  -- (withMVar rq) might block, interrupting is okay
  -- (withMVar ls) might block, interrupting is okay
  -- join (takeMVar _) will block if empty, interrupting is okay

-- | Write a value into the 'MSampleVar', overwriting any previous value that
-- was there.
--
-- 'writeSV' can block and be interrupted, in which case it does nothing.
writeSV :: MSampleVar a -> a -> IO ()
writeSV (MSampleVar _ ls) a = mask_ $ withMVar ls $ \ v -> do
  void (tryTakeMVar v)
  putMVar v a  -- cannot block
  -- (withMVar ls) might block, interrupting is okay

{-
 Design notes:

 1) The outer MVar of lockedStore is employed in 'writeSV'.  If two 'writeSV' are
 racing in different threads then without the "withMVar ls" they can each
 execute "void (tryTakeMVar v)" and then both execute "putMVar v a", causing
 the second to block.  Change putMVar to tryPutMVar lets the first 'writeSV'
 win which arguably contradicts the specification, though this race makes it a
 weak contradiction.

 Thus the lockedStore outer MVar is used as a FIFO queue for writeSV/emptySV
 that gives the "previous" in the specification a precise meaning.

 2) There is no 'tryReadSV' because the desired semantics are unclear. With
 'tryTakeMVar' one is guaranteed to block and a value (Just a) if and only if
 'takeMVar' would have suceeded without blocking. Also, if you know there are
 no other readers then a Nothing return from 'tryTakeMVar' means that it is
 empty, which is the handiest property.

 3) An alternate design would queue the writers separately and let only
 lead-reader and lead-writer access the stored value.  Imagine several queued
 writers and no readers are waiting and then a reader arrives, this reader can
 see a value from the middle of the queue of writers.  This would no longer
 guarantees the most up-to-date value is read.

 The current design has a very orderly priority of readers and writers.  Design
 (3) makes the ordering between readers and writers choatic.  Design (1) goes
 further and also makes ordering between different writers chaotic.

-}

