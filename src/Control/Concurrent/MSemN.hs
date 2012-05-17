{-# LANGUAGE DeriveDataTypeable #-}
-- | 
-- Module      :  Control.Concurrent.MSemN
-- Copyright   :  (c) Chris Kuklewicz 2011
-- License     :  3 clause BSD-style (see the file LICENSE)
-- 
-- Maintainer  :  haskell@list.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Quantity semaphores in which each thread may wait for an arbitrary amount.  This modules is
-- intended to improve on "Control.Concurrent.QSemN".
-- 
-- This semaphore gracefully handles threads which die while blocked waiting for quantity.  The
-- fairness guarantee is that blocked threads are FIFO.  An early thread waiting for a large
-- quantity will prevent a later thread waiting for a small quantity from jumping the queue.
--
-- If 'with' is used to guard a critical section then no quantity of the semaphore will be lost
-- if the activity throws an exception.
--
-- The functions below are generic in (Integral i) with specialization to Int and Integer.
--
-- Overflow warning: These operations do not check for overflow errors.  If the Integral type is too
-- small to accept the new total then the behavior of these operations is undefined.  Using (MSem
-- Integer) prevents the possibility of an overflow error.
module Control.Concurrent.MSemN
    (MSemN
    ,new
    ,with
    ,wait
    ,signal
    ,withF
    ,waitF
    ,signalF
    ,peekAvail
    ) where

import Prelude( Integral,Eq,IO,Int,Integer,Maybe(Just,Nothing),Num((+),(-)),Bool(False,True)
              , return,id,const,fmap,snd,maybe
              , (.),(<=),($),($!) )
import Control.Concurrent.MVar( MVar
                              , withMVar,modifyMVar,modifyMVar_,newMVar
                              , newEmptyMVar,putMVar,takeMVar,tryTakeMVar )
import Control.Exception(bracket,bracket_,uninterruptibleMask_,onException,evaluate,mask_)
import Control.Monad(when)
import Data.Typeable(Typeable)
import Data.Word(Word)

{- 

The only MVars allocated are the three created be 'new'.  Their three roles are
1) to have a FIFO queue of waiters
2) for the head waiter to block on
3) to protect the quantity state of the semaphore and the head waiter

subtle design notes:

with, wait, and signal pattern match the quantity against 0 which has two effect: it avoids locking
in the easy case and it ensures strict evaluation of the quantity before any locks are taken.

Originally withF, waitF, and signal did not strictly evalaute the function they are passed before
locks are taken because there is no real point since the function may throw an error when computing
the size.  But then I realized forcing 'f' might run forever with the locks held and I could move
this particular hang outside the locks by first evaluating 'f'.

-}

-- MS has an invariant that "maybe True (> avail) headWants" is always True.
data MS i = MS { avail :: !i             -- ^ This is the quantity available to be taken from the semaphore.
               , headWants :: !(Maybe i) -- ^ If there is waiter then this is Just the amount being waited for.
               }
  deriving (Eq,Typeable)

-- | A 'MSemN' is a quantity semaphore, in which the available quantity may be signalled or
-- waited for in arbitrary amounts.
data MSemN i = MSemN { mSem :: !(MVar (MS i))  -- ^ Used to lock access to state of semaphore quantity.
                     , queueWait :: !(MVar ()) -- ^ Used as FIFO queue for waiter, held by head of queue.
                     , headWait :: !(MVar ())  -- ^ The head of the waiter queue blocks on headWait.
                     }
  deriving (Eq,Typeable)

-- |'new' allows positive, zero, and negative initial values.  The initial value is forced here to
-- better localize errors.
new :: Integral i => i -> IO (MSemN i)
{-# SPECIALIZE new :: Int -> IO (MSemN Int) #-}
{-# SPECIALIZE new :: Word -> IO (MSemN Word) #-}
{-# SPECIALIZE new :: Integer -> IO (MSemN Integer) #-}
new initial = do
  newMS <- newMVar $! (MS { avail = initial  -- this forces initial
                          , headWants = Nothing })
  newQueueWait <- newMVar ()
  newHeadWait <- newEmptyMVar
  return (MSemN { mSem = newMS
                , queueWait = newQueueWait
                , headWait = newHeadWait })

-- | 'with' takes a quantity of the semaphore to take and hold while performing the provided
-- operation.  'with' ensures the quantity of the sempahore cannot be lost if there are exceptions.
-- This uses 'bracket' to ensure 'wait' and 'signal' get called correctly.
with :: Integral i => MSemN i -> i -> IO a -> IO a
{-# SPECIALIZE with :: MSemN Int -> Int -> IO a -> IO a #-}
{-# SPECIALIZE with :: MSemN Word -> Word -> IO a -> IO a #-}
{-# SPECIALIZE with :: MSemN Integer -> Integer -> IO a -> IO a #-}
with _ 0 = id -- this also forces with to be strict in wanted
with m wanted = bracket_ (wait m wanted)  (signal m wanted)

-- | 'withF' takes a pure function and an operation.  The pure function converts the available
-- quantity to a pair of the wanted quantity and a returned value.  The operation takes the result
-- of the pure function.  'withF' ensures the quantity of the sempahore cannot be lost if there
-- are exceptions.  This uses 'bracket' to ensure 'waitF' and 'signal' get called correctly.
--
-- Note: A long running pure function will block all other access to the 'MSemN' while it is
-- evaluated.
withF :: Integral i 
      => MSemN i
      -> (i -> (i,b))
      -> ((i,b) -> IO a)
      -> IO a
{-# SPECIALIZE withF :: MSemN Int -> (Int -> (Int,b)) -> ((Int,b) -> IO a) -> IO a #-}
{-# SPECIALIZE withF :: MSemN Word -> (Word -> (Word,b)) -> ((Word,b) -> IO a) -> IO a #-}
{-# SPECIALIZE withF :: MSemN Integer -> (Integer -> (Integer,b)) -> ((Integer,b) -> IO a) -> IO a #-}
withF m f = bracket (waitF m f)  (\(wanted,_) -> signal m wanted)

-- |'wait' allow positive, zero, and negative wanted values.  Waiters may block, and will be handled
-- fairly in FIFO order.
--
-- If 'wait' returns without interruption then it left the 'MSemN' with a remaining quantity that was
-- greater than or equal to zero.  If 'wait' is interrupted then no quantity is lost.  If 'wait'
-- returns without interruption then it is known that each earlier waiter has definitely either been
-- interrupted or has retured without interruption.
wait :: Integral i => MSemN i -> i -> IO ()
{-# SPECIALIZE wait :: MSemN Int -> Int -> IO () #-}
{-# SPECIALIZE wait :: MSemN Word -> Word -> IO () #-}
{-# SPECIALIZE wait :: MSemN Integer -> Integer -> IO () #-}
wait _ 0 = return () -- this also forces wait to be strict in wanted
wait m wanted = fmap snd $ waitF m (const (wanted,()))

-- | 'waitWith' takes the 'MSemN' and a pure function that takes the available quantity and computes the
-- amount wanted and a second value.  The value wanted is stricly evaluated but the second value is
-- returned lazily.
--
-- 'waitF' allow positive, zero, and negative wanted values.  Waiters may block, and will be handled
-- fairly in FIFO order.
--
-- If 'waitF' returns without interruption then it left the 'MSemN' with a remaining quantity that was
-- greater than or equal to zero.  If 'waitF' or the provided function are interrupted then no
-- quantity is lost.  If 'waitF' returns without interruption then it is known that each previous
-- waiter has each definitely either been interrupted or has retured without interruption.
--
-- Note: A long running pure function will block all other access to the 'MSemN' while it is
-- evaluated.
waitF :: Integral i => MSemN i -> (i -> (i,b)) -> IO (i,b)
{-# SPECIALIZE waitF :: MSemN Int -> (Int -> (Int,b)) -> IO (Int,b) #-}
{-# SPECIALIZE waitF :: MSemN Word -> (Word -> (Word,b)) -> IO (Word,b) #-}
{-# SPECIALIZE waitF :: MSemN Integer -> (Integer -> (Integer,b)) -> IO (Integer,b) #-}
waitF m f = mask_ . withMVar (queueWait m) $ \ () -> do
  (out@(wanted,_),mustWait) <- modifyMVar (mSem m) $ \ ms -> do
    let outVal@(wantedVal,_) = f (avail ms)
    -- assert that headDown is Nothing (from prior 'new' or 'signal' or 'cleanup')
    -- wantedVal gets forced by the (<=) condition here:
    if wantedVal <= avail ms
      then do
        let avail'down = avail ms - wantedVal -- avail'down is never negative, barring overflow
        ms' <- evaluate ms { avail = avail'down }
        return (ms', (outVal,False))
      else do
        ms' <- evaluate ms { headWants = Just wantedVal }
        return (ms', (outVal,True))
  -- mask_ is needed above because either (Just wantedVal) may be set here and this means we need to
  -- get the `onException` setup without being interrupted, or avail'down was set and we must finish
  -- 'waitF' without being interrupted so that a 'bracket' can ensure a matching 'signal' can
  -- protect the returned quantity.
  when mustWait $ do
    let cleanup = uninterruptibleMask_ $ modifyMVar_ (mSem m) $ \ms -> do
          mStale <- tryTakeMVar (headWait m)
          let avail' = avail ms + maybe 0 (const wanted) mStale
          evaluate ms {avail = avail', headWants = Nothing}
    takeMVar (headWait m) `onException` cleanup -- may not block if a 'signal' or exception has already arrived.
  return out

-- |'signal' allows positive, zero, and negative values, thus this is also way to remove quantity
-- that skips any threads in the 'wait'/'waitF' queue.  If the new total is greater than the next
-- value being waited for (if present) then the first waiter is woken.  If there are queued waiters
-- then the next one will wake after a waiter has proceeded and notice the remaining value; thus a
-- single 'signal' may result in several waiters obtaining values.  Waking waiting threads is
-- asynchronous.
--
-- 'signal' may block, but it cannot be interrupted, which allows it to dependably restore value to
-- the 'MSemN'.  All 'signal', 'signalF', 'peekAvail', and the head waiter may momentarily block in a
-- fair FIFO manner.
signal :: Integral i => MSemN i -> i -> IO ()
{-# SPECIALIZE signal :: MSemN Int -> Int -> IO () #-}
{-# SPECIALIZE signal :: MSemN Word -> Word -> IO () #-}
{-# SPECIALIZE signal :: MSemN Integer -> Integer -> IO () #-}
signal _ 0 = return () -- this also forces signal to be strict in 'size'
signal m size = uninterruptibleMask_ $ fmap snd $ signalF m (const (size,()))

-- | Instead of providing a fixed change to the available quantity, 'signalF' applies a provided
-- pure function to the available quantity to compute the change and a second value.  The
-- requested change is stricly evaluated but the second value is returned lazily.  If the new total is
-- greater than the next value being waited for then the first waiter is woken.  If there are queued
-- waiters then the next one will wake after a waiter has proceeded and notice the remaining value;
-- thus a single 'signalF' may result in several waiters obtaining values.  Waking waiting threads
-- is asynchronous.
--
-- 'signalF' may block, and it can be safely interrupted.  If the provided function throws an error
-- or is interrupted then it leaves the 'MSemN' unchanged.  All 'signal', 'signalF', 'peekAvail', and
-- the head waiter may momentarily block in a fair FIFO manner.
--
-- Note: A long running pure function will block all other access to the 'MSemN' while it is
-- evaluated.
signalF :: Integral i
        => MSemN i
        -> (i -> (i,b))
        -> IO (i,b)
{-# SPECIALIZE signalF :: MSemN Int -> (Int -> (Int,b)) -> IO (Int,b) #-}
{-# SPECIALIZE signalF :: MSemN Word -> (Word -> (Word,b)) -> IO (Word,b) #-}
{-# SPECIALIZE signalF :: MSemN Integer -> (Integer -> (Integer,b)) -> IO (Integer,b) #-}
signalF m f = mask_ . modifyMVar (mSem m) $ \ ms -> do
  -- Nothing below blocks, not even the putMVar
  let out@(size,_) = f (avail ms)
  avail' <- evaluate $ avail ms + size -- this forces 'size'
  ms' <- case headWants ms of
           Just wanted | wanted <= avail' -> do
             putMVar (headWait m) ()
             let avail'down = avail' - wanted -- avail'down is never negative, barring overflow
             evaluate ms { avail = avail'down, headWants = Nothing }
           _ -> evaluate ms { avail = avail' }
  return (ms',out)

-- | 'peekAvail' skips the queue of any blocked 'wait' and 'waitF' threads, but may momentarily
-- block on 'signal', 'signalF', other 'peekAvail', and the head waiter. This returns the amount of
-- value available to be taken.  Using this value without producing unwanted race conditions is left
-- up to the programmer.
--
-- 'peekAvail' is an optimized form of \"signalF m (\x -> (0,x))\".
--
-- A version of 'peekAvail' that joins the FIFO queue of 'wait' and 'waitF' can be acheived by
-- \"waitF m (\x -> (0,x))\"
peekAvail :: Integral i => MSemN i -> IO i
{-# SPECIALIZE peekAvail :: MSemN Int -> IO Int #-}
{-# SPECIALIZE peekAvail :: MSemN Word -> IO Word #-}
{-# SPECIALIZE peekAvail :: MSemN Integer -> IO Integer #-}
peekAvail m = withMVar (mSem m) (return . avail)
