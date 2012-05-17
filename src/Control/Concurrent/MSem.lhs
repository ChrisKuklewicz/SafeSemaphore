{-# LANGUAGE DeriveDataTypeable #-}
-- | 
-- Module      :  Control.Concurrent.MSem
-- Copyright   :  (c) Chris Kuklewicz 2011
-- License     :  3 clause BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  haskell@list.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- A semaphore in which operations may 'wait' for or 'signal' single units of value.  This modules
-- is intended to improve on "Control.Concurrent.QSem".
-- 
-- This semaphore gracefully handles threads which die while blocked waiting.  The fairness
-- guarantee is that blocked threads are FIFO.
--
-- If 'with' is used to guard a critical section then no quantity of the semaphore will be lost if
-- the activity throws an exception. 'new' can initialize the semaphore to negative, zero, or
-- positive quantity. 'wait' always leaves the 'MSem' with non-negative quantity.
--
-- The functions below are generic in (Integral i) with specialization to Int and Integer.
--
-- Overflow warning: These operations do not check for overflow errors.  If the Integral type is too
-- small to accept the new total then the behavior of these operations is undefined.  Using (MSem
-- Integer) prevents the possibility of an overflow error.
module Control.Concurrent.MSem
    (MSem
    ,new
    ,with
    ,wait
    ,signal
    ,peekAvail
    ) where

import Control.Monad(join)
import Control.Concurrent.MVar(MVar,withMVar,modifyMVar,modifyMVar_,newMVar,newEmptyMVar,putMVar,takeMVar,tryTakeMVar,tryPutMVar)
import Control.Exception(bracket_,uninterruptibleMask_,mask_)
import Data.Typeable(Typeable)

{- design notes are in MSemN.hs -}

-- | A 'MSem' is a semaphore in which the available quantity can be added and removed in single
--  units, and which can start with positive, zero, or negative value.
data MSem i = MSem { mSem :: !(MVar i)  -- ^ Used to lock access to state of semaphore quantity. Never updated.
                   , queueWait :: !(MVar ()) -- ^ Used as FIFO queue for waiter, held by head of queue.  Never updated.
                   , headWait :: !(MVar ())  -- ^ The head of the waiter queue blocks on headWait. Never updated.
                   }
  deriving (Eq,Typeable)

-- |'new' allows positive, zero, and negative initial values.  The initial value is forced here to
-- better localize errors.
--
-- The only way to acheive a negative value with MSem is to start negative with 'new'.  Once the quantity
new :: Integral i => i -> IO (MSem i)
{-# SPECIALIZE new :: Int -> IO (MSem Int) #-}
{-# SPECIALIZE new :: Integer -> IO (MSem Integer) #-}
new initial = do
  newMS <- newMVar $! initial
  newQueueWait <- newMVar ()
  newHeadWait <- newEmptyMVar
  return (MSem { mSem = newMS
               , queueWait = newQueueWait
               , headWait = newHeadWait })

-- | 'with' takes a unit of value from the semaphore to hold while performing the provided
-- operation.  'with' ensures the quantity of the sempahore cannot be lost if there are exceptions.
--
-- 'with' uses 'bracket_' to ensure 'wait' and 'signal' get called correctly.
with :: Integral i => MSem i -> IO a -> IO a
{-# SPECIALIZE with :: MSem Int -> IO a -> IO a #-}
{-# SPECIALIZE with :: MSem Integer -> IO a -> IO a #-}
with m = bracket_ (wait m)  (signal m)

-- |'wait' will take one unit of value from the sempahore, but will block if the quantity available
-- is not positive.
--
-- If 'wait' returns without interruption then it left the 'MSem' with a remaining quantity that was
-- greater than or equal to zero.  If 'wait' is interrupted then no quantity is lost.  If 'wait'
-- returns without interruption then it is known that each earlier waiter has definitely either been
-- interrupted or has retured without interruption.
wait :: Integral i => MSem i -> IO ()
{-# SPECIALIZE wait :: MSem Int -> IO () #-}
{-# SPECIALIZE wait :: MSem Integer -> IO () #-}
wait m = mask_ . withMVar (queueWait m) $ \ () -> do
  join . modifyMVar (mSem m) $ \ ms -> do
    mayGrab <- tryTakeMVar (headWait m)
    case mayGrab of
      Just () -> return (ms,return ())
      Nothing -> if 1 <= ms
                   then let ms' = pred ms -- ms' is never negative
                        in seq ms' $ return (ms', return ())
                   else return (ms, takeMVar (headWait m))
  -- mask_ is needed above because we may have just decremented 'avail' and we must finished 'wait'
  -- without being interrupted so that a 'bracket' can ensure a matching 'signal' can be ensured.
  --
  -- join (takeMVar ..) actually may or may not block, a 'signal' could have already arrived or this
  -- thread might have an pending throwTo/killThread exception.

-- | 'signal' adds one unit to the sempahore.
--
-- 'signal' may block, but it cannot be interrupted, which allows it to dependably restore value to
-- the 'MSem'.  All 'signal', 'peekAvail', and the head waiter may momentarily block in a fair FIFO
-- manner.
signal :: Integral i => MSem i -> IO ()
{-# SPECIALIZE signal :: MSem Int -> IO () #-}
{-# SPECIALIZE signal :: MSem Integer -> IO () #-}
signal m = uninterruptibleMask_ . modifyMVar_ (mSem m) $ \ ms -> do
  -- mask_ might be as good as uninterruptibleMask_ since nothing below can block
  if ms < 0
    then return $! succ ms
    else do
      didPlace <- tryPutMVar (headWait m) ()  -- ms is never negative
      if didPlace
        then return ms
        else return $! succ ms

-- | 'peekAvail' skips the queue of any blocked 'wait' threads, but may momentarily block on
-- 'signal', other 'peekAvail', and the head waiter. This returns the amount of value available to
-- be taken.  Using this value without producing unwanted race conditions is left up to the
-- programmer.
--
-- Note that "Control.Concurrent.MSemN" offers a more powerful API for making decisions based on the
-- available amount.
peekAvail :: Integral i => MSem i -> IO i
{-# SPECIALIZE peekAvail :: MSem Int -> IO Int #-}
{-# SPECIALIZE peekAvail :: MSem Integer -> IO Integer #-}
peekAvail m = mask_ $ withMVar (mSem m) $ \ ms -> do
  extraFlag <- tryTakeMVar (headWait m)
  case extraFlag of
    Nothing -> return ms
    Just () -> do putMVar (headWait m) () -- cannot block
                  return $! succ ms
