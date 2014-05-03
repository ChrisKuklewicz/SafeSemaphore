{-# LANGUAGE DeriveDataTypeable #-}
-- | 
-- Module      :  Control.Concurrent.MSemN2
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
module Control.Concurrent.MSemN2
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
              , return,const,fmap,snd,seq
              , (.),(<=),($),($!) )
import Control.Concurrent.MVar( MVar
                              , withMVar,modifyMVar,newMVar
                              , newEmptyMVar,tryPutMVar,takeMVar,tryTakeMVar )
import Control.Exception(bracket,bracket_,uninterruptibleMask_,evaluate,mask_)
import Control.Monad(when,void)
import Data.Maybe(fromMaybe)
import Data.Typeable(Typeable)
import Data.Word(Word)

{- 

The only MVars allocated are the three created be 'new'.  Their three roles are
1) to have a FIFO queue of waiters
2) for the head waiter to block on
3) to protect the quantity state of the semaphore and the head waiter

-}

-- MS has an invariant that "maybe True (> avail) headWants" is always True.
data MS i = MS { avail :: !i             -- ^ This is the quantity available to be taken from the semaphore.
               , headWants :: !(Maybe i) -- ^ If there is waiter then this is Just the amount being waited for.
               }
  deriving (Eq,Typeable)

-- | A 'MSemN' is a quantity semaphore, in which the available quantity may be signalled or
-- waited for in arbitrary amounts.
data MSemN i = MSemN { quantityStore :: !(MVar (MS i))  -- ^ Used to lock access to state of semaphore quantity.
                     , queueWait :: !(MVar ()) -- ^ Used as FIFO queue for waiter, held by head of queue.
                     , headWait :: !(MVar i)  -- ^ The head of the waiter queue blocks on headWait.
                     }
  deriving (Eq,Typeable)

-- |'new' allows positive, zero, and negative initial values.  The initial value is forced here to
-- better localize errors.
new :: Integral i => i -> IO (MSemN i)
{-# SPECIALIZE new :: Int -> IO (MSemN Int) #-}
{-# SPECIALIZE new :: Word -> IO (MSemN Word) #-}
{-# SPECIALIZE new :: Integer -> IO (MSemN Integer) #-}
new initial = do
  newMS <- newMVar $! (MS { avail = initial  -- this forces 'initial'
                          , headWants = Nothing })
  newQueueWait <- newMVar ()
  newHeadWait <- newEmptyMVar
  return (MSemN { quantityStore = newMS
                , queueWait = newQueueWait
                , headWait = newHeadWait })

-- | 'with' takes a quantity of the semaphore to take and hold while performing the provided
-- operation.  'with' ensures the quantity of the sempahore cannot be lost if there are exceptions.
-- This uses 'bracket' to ensure 'wait' and 'signal' get called correctly.
with :: Integral i => MSemN i -> i -> IO a -> IO a
{-# SPECIALIZE with :: MSemN Int -> Int -> IO a -> IO a #-}
{-# SPECIALIZE with :: MSemN Word -> Word -> IO a -> IO a #-}
{-# SPECIALIZE with :: MSemN Integer -> Integer -> IO a -> IO a #-}
with m wanted = seq wanted $ bracket_ (wait m wanted)  (uninterruptibleMask_ $ signal m wanted)

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
withF m f = bracket (waitF m f)  (\(wanted,_) -> uninterruptibleMask_ $ signal m wanted)

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
wait m wanted = seq wanted $ fmap snd $ waitF m (const (wanted,()))

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
waitF m f = seq f $ mask_ . withMVar (queueWait m) $ \ () -> do
  (out,mustWait) <- modifyMVar (quantityStore m) $ \ ms -> do
    -- Assume: ((headWait is empty) OR (headWants is Nothing))
    -- Nothing in this scope can block
    --
    -- headWait might be full here if the predecessor waitF blocked and died and signal (tried to)
    -- feed it.
    recovered <- fmap (fromMaybe 0) (tryTakeMVar (headWait m))
    let total = avail ms + recovered
        outVal@(wantedVal,_) = f total
    if wantedVal <= total  -- forces wantedVal
      then do
        ms' <- evaluate MS { avail = total - wantedVal, headWants = Nothing }
        return (ms', (outVal,False))
      else do
        ms' <- evaluate MS { avail = total, headWants = Just wantedVal }
        return (ms', (outVal,True))
  -- quantityStore is now released, queueWait is still held, race with signal now possible
  -- Assert: (headWait is empty) AND (mustWait == (headWants is Just)) at release
  -- Proof: tryTakeMVar forced (headWait is empty), and
  --        the if-then-else branches ensured (mustWait == (headWants is Just))
  -- This assertion implies ((headWait is empty) OR (headWants is Nothing)) invariant holds (point X)
  when mustWait (void (takeMVar (headWait m)))
  return out
  -- Invariant: ((headWait is empty) OR (headWants is Nothing))
  -- Proof: 1) mustWait was false
  --           nothing happened since (point X) except perhaps race with signal
  --           signal maintained invariant
  --   2) mustWait was true
  --   2a) takeMVar succeeded so headWait became full since (point X)
  --         this implies signal filled headWait and thus signal ended with (headWait is full)
  --         signal invariant ((headWait is empty) OR (headWants is Nothing)) implies (headWants is Nothing) was set
  --          (headWait is empty) by takeMVar and (headWants is Nothing) by implication
  --   2b) takeMVar was interrupted and thus did nothing
  --         nothing happened since (point X) except perhaps race with signal
  --         signal maintained invariant

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
signal _ 0 = return () -- this case forces 'size'
signal m size = fmap snd $ signalF m (const (size,()))

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
signalF m f = seq f $ mask_ . modifyMVar (quantityStore m) $ \ ms -> do
  -- Assume: ((headWait is empty) OR (headWants is Nothing))
  -- Nothing in this scope can block
  let out@(size,_) = f (avail ms)
  ms' <- case headWants ms of
           Nothing -> evaluate ms { avail = avail ms + size }
           Just wantedVal -> do
             -- Because headWants is Just _ the assumption implies headWait is empty
             let total = avail ms + size
             if wantedVal <= total
                then do
                  _didPlace <- tryPutMVar (headWait m) wantedVal -- _didPlace is always True
                  evaluate MS { avail = total - wantedVal, headWants = Nothing }
                else do
                  evaluate ms { avail = total }
  return (ms',out)
  -- Invariant: ((headWait is empty) OR (headWants is Nothing))
  -- Proof: Assume invariant originally holds when taking quantityStore
  --   1) headWants originally Nothing, headWants and headWait unchanged, invariant still holds
  --   2) headWants originally Just _ implies, by assumption, that (headWait is empty)
  --      if-then-branch: headWants changed to Nothing and headWait changed to filled, invariant satisfied
  --      if-else-branch: headWants and headWait unchanged, invariant still holds

-- | 'peekAvail' skips the queue of any blocked 'wait' and 'waitF' threads, but may momentarily
-- block on 'signal', 'signalF', other 'peekAvail', and the head waiter. This returns the amount of
-- value available to be taken.  Using this value without producing unwanted race conditions is left
-- up to the programmer.
--
-- 'peekAvail' is an optimized form of \"signalF m (\x -> (0,x))\".
--
-- Quantity that has been passed to a blocked waiter but not picked up is not counted.  If the
-- blocked waiter is killed before picking it up then the passed quantity will be recovered by the
-- next waiter.  In this exceptional case this next waiter may see an available total that is
-- different than returned by peekAvail.
--
-- A version of 'peekAvail' that joins the FIFO queue of 'wait' and 'waitF' can be acheived by
-- \"waitF m (\x -> (0,x))\" but this will block if x is negative.  On the other hand this method
-- will see the total including any recovered quantity.
peekAvail :: Integral i => MSemN i -> IO i
{-# SPECIALIZE peekAvail :: MSemN Int -> IO Int #-}
{-# SPECIALIZE peekAvail :: MSemN Word -> IO Word #-}
{-# SPECIALIZE peekAvail :: MSemN Integer -> IO Integer #-}
peekAvail m = withMVar (quantityStore m) (return . avail)
