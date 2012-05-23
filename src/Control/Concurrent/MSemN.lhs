> {-# LANGUAGE DeriveDataTypeable #-}
> -- | 
> -- Module      :  Control.Concurrent.MSemN
> -- Copyright   :  (c) Chris Kuklewicz 2011
> -- License     :  3 clause BSD-style (see the file LICENSE)
> -- 
> -- Maintainer  :  haskell@list.mightyreason.com
> -- Stability   :  experimental
> -- Portability :  non-portable (concurrency)
> --
> -- Quantity semaphores in which each thread may wait for an arbitrary amount.  This modules is
> -- intended to improve on "Control.Concurrent.QSemN".
> -- 
> -- This semaphore gracefully handles threads which die while blocked waiting for quantity.  The
> -- fairness guarantee is that blocked threads are FIFO.  An early thread waiting for a large
> -- quantity will prevent a later thread waiting for a small quantity from jumping the queue.
> --
> -- If 'with' is used to guard a critical section then no quantity of the semaphore will be lost
> -- if the activity throws an exception.
> --
> -- The functions below are generic in (Integral i) with specialization to Int and Integer.
> --
> -- Overflow warning: These operations do not check for overflow errors.  If the Integral type is too
> -- small to accept the new total then the behavior of these operations is undefined.  Using (MSem
> -- Integer) prevents the possibility of an overflow error.
>
> module Control.Concurrent.MSemN
>     (MSemN
>     ,new
>     ,with
>     ,wait
>     ,signal
>     ,withF
>     ,waitF
>     ,signalF
>     ,peekAvail
>     ) where
> 
> import Prelude( Integral,Eq,IO,Int,Integer,Maybe(Just,Nothing),Num((+),(-)),Bool(False,True)
>               , return,id,const,fmap,snd,maybe,seq
>               , (.),(<=),($),($!) )
> import Control.Concurrent.MVar( MVar
>                               , withMVar,modifyMVar,modifyMVar_,newMVar
>                               , newEmptyMVar,tryPutMVar,takeMVar,tryTakeMVar )
> import Control.Exception(bracket,bracket_,uninterruptibleMask_,onException,evaluate,mask_)
> import Control.Monad(when)
> import Data.Typeable(Typeable)
> import Data.Word(Word)
> 
 
The only MVars allocated are the three created be 'new'.  Their three roles are
1) to have a FIFO queue of waiters (queueWait)
2) for the head waiter to block on, if necessary (headWait)
3) to protect the actual state of the semaphore (quantityStore)

> -- MS has an invariant that "maybe True (> avail) headWants" is always True.
> data MS i = MS { avail :: !i             -- ^ This is the quantity available to be taken from the semaphore.
>                , headWants :: !(Maybe i) -- ^ If there is waiter then this is Just the amount being waited for.
>                }
>   deriving (Eq,Typeable)

> -- | A 'MSemN' is a quantity semaphore, in which the available quantity may be signalled or
> -- waited for in arbitrary amounts.
> data MSemN i = MSemN { quantityStore :: !(MVar (MS i))  -- ^ Used to lock access to state of semaphore quantity.
>                      , queueWait :: !(MVar ()) -- ^ Used as FIFO queue for waiter, held by head of queue.
>                      , headWait :: !(MVar ())  -- ^ The head of the waiter queue blocks on headWait.
>                      }
>   deriving (Eq,Typeable)

The data structure for 'MSemN' is slightly more complicated than the one in 'MSem'.  Here the
quantityStore holds not just a value of type 'i' but also a 'Maybe i' called 'headWants'.

'headWants' is Nothing when there are no blocked threads waiting on quantity.  'headWants' is (Just
x) when there is at least one blocked thread and the head of the queue needs positive quantity x to
proceed.

There are two possible lifecycles of a wait request.  Like in MSem, all waiters do all work while
holding queueWait.  This is what forces the waiters into a FIFO order.

The first is when the waiter gets to head of the queue and finds that the quantityStore has enough
in 'avail' to be satisfied.  This waiter subtracts its wanted value from 'avail' and returns.

The second is when the waiter does not find a larger enough value in 'avail' must block.  It sets
headWants from Nothing to 'Just wanted' and then releases quantityStore, followed by blocked in
headWait.  When a signal arrives that puts the available quantity above the value in 'headWants'
then it puts () into 'headWait' to wake the blocked waiting thread.  Here the subtraction of the
value in 'Just wanted' from the available quantity is handled by the signalling thread.

The difficulty is maintaining the desired invariants in the face of exceptions.  If a frustrated
waiter dies before the 'takeMVar' on 'headWait' succeeds then the waiter's changes to
'quantityStore' must be undone!  This requires the 'uninterruptibleMask_' around the onException
action in 'waitF'.

When the head waiter releases the queueWait MVar, either by succeeding or being interrupted, there
are three invariants:

(wait invariant 1) The headWait MVar must be empty.

(wait invariant 2) The headWants value is Nothing.

This means that when a waiter first acquires the queueWait MVar both the above hold. If the waiter
succeeded then there is a progress invariant:

(wait progress invariant) The value of 'avail' is non-negative when wait succeeds.

When the signal operation release the quantityStore MVar then one of three situations holds:

(signal possibility 1) headWants was Nothing and it and headWait are unchanged, or

(signal possibility 2) headWants was (Just x) and it and headWait are unchanged, or

(signal possibility 3) headWants was (Just x) and is changed to Nothing and headWait has () put into it.

If headWait had () put into it then headWants is Nothing. The only way headWants can change back to
(Just x) is if a new waiter does it.  This requires the original waiter to hand over the queueWait
MVar, and we can be certain that (wait invariant 1) means that the () put into headWait is taken out
before this handoff.

Thus when a signal first acquires the quantityStore MVar there is a dynamically maintained invariant:

(signal invariant 1) A signal that finds headWants of (Just x) also finds headWait empty.

Note that a () put into headWait signifies amount: it is worth the quantity x in the (Just x) in
headWants that was just changed to Nothing.  After (signal possibility 3) only the receiving waiting
thread knows the amount that this () in headWait represents, and only this thread can fix the MSemN
if an exception occurs.  The waitF function below is careful to fix MSemN.

> -- |'new' allows positive, zero, and negative initial values.  The initial value is forced here to
> -- better localize errors.
> new :: Integral i => i -> IO (MSemN i)
> {-# SPECIALIZE new :: Int -> IO (MSemN Int) #-}
> {-# SPECIALIZE new :: Word -> IO (MSemN Word) #-}
> {-# SPECIALIZE new :: Integer -> IO (MSemN Integer) #-}
> new initial = do
>   newMS <- newMVar $! (MS { avail = initial  -- this forces initial
>                           , headWants = Nothing })
>   newQueueWait <- newMVar ()
>   newHeadWait <- newEmptyMVar
>   return (MSemN { quantityStore = newMS
>                 , queueWait = newQueueWait
>                 , headWait = newHeadWait })
> 
> -- | 'with' takes a quantity of the semaphore to take and hold while performing the provided
> -- operation.  'with' ensures the quantity of the sempahore cannot be lost if there are exceptions.
> -- This uses 'bracket' to ensure 'wait' and 'signal' get called correctly.
> with :: Integral i => MSemN i -> i -> IO a -> IO a
> {-# SPECIALIZE with :: MSemN Int -> Int -> IO a -> IO a #-}
> {-# SPECIALIZE with :: MSemN Word -> Word -> IO a -> IO a #-}
> {-# SPECIALIZE with :: MSemN Integer -> Integer -> IO a -> IO a #-}
> with m wanted = seq wanted $ bracket_ (wait m wanted)  (signal m wanted)
> 
> -- | 'withF' takes a pure function and an operation.  The pure function converts the available
> -- quantity to a pair of the wanted quantity and a returned value.  The operation takes the result
> -- of the pure function.  'withF' ensures the quantity of the sempahore cannot be lost if there
> -- are exceptions.  This uses 'bracket' to ensure 'waitF' and 'signal' get called correctly.
> --
> -- Note: A long running pure function will block all other access to the 'MSemN' while it is
> -- evaluated.
> withF :: Integral i 
>       => MSemN i
>       -> (i -> (i,b))
>       -> ((i,b) -> IO a)
>       -> IO a
> {-# SPECIALIZE withF :: MSemN Int -> (Int -> (Int,b)) -> ((Int,b) -> IO a) -> IO a #-}
> {-# SPECIALIZE withF :: MSemN Word -> (Word -> (Word,b)) -> ((Word,b) -> IO a) -> IO a #-}
> {-# SPECIALIZE withF :: MSemN Integer -> (Integer -> (Integer,b)) -> ((Integer,b) -> IO a) -> IO a #-}
> withF m f = bracket (waitF m f)  (\(wanted,_) -> signal m wanted)
> 
> -- |'wait' allow positive, zero, and negative wanted values.  Waiters may block, and will be handled
> -- fairly in FIFO order.  Waiters will succeed when the wanted value is less than or equal to the
> -- available value.  The FIFO order means that a 'wait' for a large quantity that blocks will prevent later
> -- requests from being considered even if the later requests would be for a small quantity that could be fulfilled.
> --
> -- If 'wait' returns without interruption then it left the 'MSemN' with a remaining quantity that was
> -- greater than or equal to zero.  If 'wait' is interrupted then no quantity is lost.  If 'wait'
> -- returns without interruption then it is known that each earlier waiter has definitely either been
> -- interrupted or has retured without interruption.
> wait :: Integral i => MSemN i -> i -> IO ()
> {-# SPECIALIZE wait :: MSemN Int -> Int -> IO () #-}
> {-# SPECIALIZE wait :: MSemN Word -> Word -> IO () #-}
> {-# SPECIALIZE wait :: MSemN Integer -> Integer -> IO () #-}
> wait m wanted = seq wanted $ fmap snd $ waitF m (const (wanted,()))
> 
> -- | 'waitWith' takes the 'MSemN' and a pure function that takes the available quantity and computes the
> -- amount wanted and a second value.  The value wanted is stricly evaluated but the second value is
> -- returned lazily.
> --
> -- 'waitF' allow positive, zero, and negative wanted values.  Waiters may block, and will be handled
> -- fairly in FIFO order.  Waiters will succeed when the wanted value is less than or equal to the
> -- available value.  The FIFO order means that a 'waitF' for a large quantity that blocks will prevent later
> -- requests from being considered even if the later requests would be for a small quantity that could be fulfilled.
> --
> -- If 'waitF' returns without interruption then it left the 'MSemN' with a remaining quantity that was
> -- greater than or equal to zero.  If 'waitF' or the provided function are interrupted then no
> -- quantity is lost.  If 'waitF' returns without interruption then it is known that each previous
> -- waiter has each definitely either been interrupted or has retured without interruption.
> --
> -- Note: A long running pure function will block all other access to the 'MSemN' while it is
> -- evaluated.
> waitF :: Integral i => MSemN i -> (i -> (i,b)) -> IO (i,b)
> {-# SPECIALIZE waitF :: MSemN Int -> (Int -> (Int,b)) -> IO (Int,b) #-}
> {-# SPECIALIZE waitF :: MSemN Word -> (Word -> (Word,b)) -> IO (Word,b) #-}
> {-# SPECIALIZE waitF :: MSemN Integer -> (Integer -> (Integer,b)) -> IO (Integer,b) #-}
> waitF m f = seq f $ mask_ . withMVar (queueWait m) $ \ () -> do
>   -- Assume when queueWait taken: (headWait is empty) AND (headWants is Nothing)
>   (out@(wanted,_),mustWait) <- modifyMVar (quantityStore m) $ \ ms -> do
>     -- Nothing in this scope can block
>     let outVal@(wantedVal,_) = f (avail ms)
>     -- assert that headDown is Nothing (from prior 'new' or 'signal' or 'cleanup')
>     -- wantedVal gets forced by the (<=) condition here:
>     if wantedVal <= avail ms
>       then do
>         let avail'down = avail ms - wantedVal -- avail'down is never negative, barring overflow
>         ms' <- evaluate ms { avail = avail'down }
>         return (ms', (outVal,False))
>       else do
>         ms' <- evaluate ms { headWants = Just wantedVal }
>         return (ms', (outVal,True))
>   -- quantityStore is now released, queueWait is still held, race with signal now possible
>   -- Assert: (headWait is empty) AND (mustWait == (headWants is Just)) at release (point X)
>   -- Proof: (headWait is empty) was assumed and is unchanged, and
>   --        either mustWait is False and assumed (headWants is Nothing) is unchanged,
>   --        or mustWait is True and headWants was set to Just wantedVal
>   when mustWait $ do
>     let cleanup = uninterruptibleMask_ $ modifyMVar_ (quantityStore m) $ \ms -> do
>           recovered <- tryTakeMVar (headWait m)
>           let total = avail ms + maybe 0 (const wanted) recovered
>           evaluate MS {avail = total, headWants = Nothing}
>     takeMVar (headWait m) `onException` cleanup -- takeMVar might not block if a 'signal' or exception has already arrived.
>   return out
>   -- Invariant when queueWait released: (headWait is empty) AND (headWants is Nothing)
>   -- Proof: 1) mustWait is false, so (headWants is Just) was false
>   --           so (headWait is empty) AND (headWants is Nothing) was true at (point X)
>   --           by LEMMA under signalF this is unchanged by signalF; there has been no race condition
>   --   2) mustWait is true, so (headWants is Just) was true
>   --   2a) takeMVar succeeded so headWait became full since (point X)
>   --         this implies signal filled headWait and thus signal ended with (headWait is full)
>   --         signal invariant ((headWait is empty) OR (headWants is Nothing)) implies (headWants is Nothing) was set
>   --          (headWait is empty) by takeMVar and (headWants is Nothing) by implication
>   --   2b) takeMVar was interrupted, then onException ran cleanup, by uninterruptibleMask_ it succeeded
>   --         cleanup's tryTakeMVar ensured (headWait is empty), and
>   --         cleanup's modifyMVar_ ensured (headWants is Nothing)

> 
> -- |'signal' allows positive, zero, and negative values, thus this is also way to remove quantity
> -- that skips any threads in the 'wait'/'waitF' queue.  If the new total is greater than the next
> -- value being waited for (if present) then the first waiter is woken.  If there are queued waiters
> -- then the next one will wake after a waiter has proceeded and notice the remaining value; thus a
> -- single 'signal' may result in several waiters obtaining values.  Waking waiting threads is
> -- asynchronous.
> --
> -- 'signal' may block, but it cannot be interrupted, which allows it to dependably restore value to
> -- the 'MSemN'.  All 'signal', 'signalF', 'peekAvail', and the head waiter may momentarily block in a
> -- fair FIFO manner.
> signal :: Integral i => MSemN i -> i -> IO ()
> {-# SPECIALIZE signal :: MSemN Int -> Int -> IO () #-}
> {-# SPECIALIZE signal :: MSemN Word -> Word -> IO () #-}
> {-# SPECIALIZE signal :: MSemN Integer -> Integer -> IO () #-}
> signal _ 0 = return () -- this also forces 'size'
> signal m size = uninterruptibleMask_ $ fmap snd $ signalF m (const (size,()))
> 
> -- | Instead of providing a fixed change to the available quantity, 'signalF' applies a provided
> -- pure function to the available quantity to compute the change and a second value.  The
> -- requested change is stricly evaluated but the second value is returned lazily.  If the new total is
> -- greater than the next value being waited for then the first waiter is woken.  If there are queued
> -- waiters then the next one will wake after a waiter has proceeded and notice the remaining value;
> -- thus a single 'signalF' may result in several waiters obtaining values.  Waking waiting threads
> -- is asynchronous.
> --
> -- 'signalF' may block, and it can be safely interrupted.  If the provided function throws an error
> -- or is interrupted then it leaves the 'MSemN' unchanged.  All 'signal', 'signalF', 'peekAvail', and
> -- the head waiter may momentarily block in a fair FIFO manner.
> --
> -- Note: A long running pure function will block all other access to the 'MSemN' while it is
> -- evaluated.
> signalF :: Integral i
>         => MSemN i
>         -> (i -> (i,b))
>         -> IO (i,b)
> {-# SPECIALIZE signalF :: MSemN Int -> (Int -> (Int,b)) -> IO (Int,b) #-}
> {-# SPECIALIZE signalF :: MSemN Word -> (Word -> (Word,b)) -> IO (Word,b) #-}
> {-# SPECIALIZE signalF :: MSemN Integer -> (Integer -> (Integer,b)) -> IO (Integer,b) #-}
> signalF m f = seq f $ mask_ . modifyMVar (quantityStore m) $ \ ms -> do
>   -- Assume: ((headWait is empty) OR (headWants is Nothing))
>   -- Nothing below can block
>   let out@(size,_) = f (avail ms)
>   total <- evaluate $ avail ms + size -- this forces 'size'
>   ms' <- case headWants ms of
>            Just wanted | wanted <= total -> do
>              -- Assumption implies headWait is empty, using putMVar below would never block
>              didPlace <- tryPutMVar (headWait m) () 
>              evaluate $ if didPlace
>                           then MS { avail = total - wanted, headWants = Nothing }  -- always this case
>                           else MS { avail = total, headWants = Nothing }  -- impossible case
>            _ -> evaluate ms { avail = total }
>   return (ms',out)
>   -- Invariant: ((headWait is empty) OR (headWants is Nothing))
>   -- Proof: 1) originally (headWants is Nothing), headWait and headWants unchanged, invariant still holds
>   --   2) orignal (Just wanted)
>   --   2a) wanted <= total, headWait becomes filled and headWants becomes Nothing, invariant holds
>   --   2b) wanted > total, headWait and headWants unchanged, invariant still holds
>
> -- LEMMA: if (headWait is empty) AND (headWants is Nothing) holds before signalF then it holds after signalF
> -- Proof: When (headWants is Nothing) both headWait and headWants are unchanged (proof case 1 above)

> -- | 'peekAvail' skips the queue of any blocked 'wait' and 'waitF' threads, but may momentarily
> -- block on 'signal', 'signalF', other 'peekAvail', and the head waiter. This returns the amount of
> -- value available to be taken.  Using this value without producing unwanted race conditions is left
> -- up to the programmer.
> --
> -- 'peekAvail' is an optimized form of \"signalF m (\x -> (0,x))\".
> --
> -- A version of 'peekAvail' that joins the FIFO queue of 'wait' and 'waitF' can be acheived by
> -- \"waitF m (\x -> (0,x))\"
> peekAvail :: Integral i => MSemN i -> IO i
> {-# SPECIALIZE peekAvail :: MSemN Int -> IO Int #-}
> {-# SPECIALIZE peekAvail :: MSemN Word -> IO Word #-}
> {-# SPECIALIZE peekAvail :: MSemN Integer -> IO Integer #-}
> peekAvail m = withMVar (quantityStore m) (return . avail)
