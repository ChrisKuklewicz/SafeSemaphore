-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.SSem
-- Copyright   :  (c) Chris Kuklewicz, 2012
-- License     :  BSD-style
-- 
-- Maintainer  :  haskell@list.mightyreason.com
-- Stability   :  experimental
-- Portability :  non-portable (concurrency)
--
-- Very simple quantity semaphore.
--
-----------------------------------------------------------------------------
module Control.Concurrent.SSem( SSem,new
                              , withSem,wait,signal,tryWait
                              , withSemN,waitN,signalN,tryWaitN
                              , getValue) where

import Control.Concurrent.STM.SSemInternals(SSem(SSem))
import qualified Control.Concurrent.STM.SSem as S(wait,signal,tryWait,waitN,signalN,tryWaitN,getValue)
import Control.Concurrent.STM.TVar(newTVarIO)
import Control.Exception(bracket_)
import Control.Monad.STM(atomically)

-- | Create a new semaphore with the given argument as the initially available quantity.  This
-- allows new semaphores to start with a negative, zero, or positive quantity.
new :: Int -> IO SSem
new = fmap SSem . newTVarIO

-- | It is recommended that all paired uses of 'wait' and 'signal' use the 'with' bracketed form
-- to ensure exceptions safety.
withSem :: SSem -> IO a -> IO a
withSem s = bracket_ (wait s) (signal s)

-- | It is recommended that all paired uses of 'waitN' and 'signalN' use the 'withN'
-- bracketed form to ensure exceptions safety.
withSemN :: SSem -> Int -> IO a -> IO a
withSemN s i = bracket_ (waitN s i) (signalN s i)

-- | Try to take a unit of value from the semaphore.  This succeeds when the current quantity is
-- positive, and then reduces the quantity by one.  Otherwise this will block and 'retry' until it
-- succeeds or is killed.  This will never result in a negative quantity.  If several threads are
-- retying then which one succeeds next is undefined -- an unlucky thread might starve.
wait :: SSem -> IO ()
wait = atomically . S.wait

-- | Try to take the given value from the semaphore.  This succeeds when the quantity is greater or
-- equal to the given value, and then subtracts the given value from the quantity.  Otherwise this
-- will block and 'retry' until it succeeds or is killed.  This will never result in a negative
-- quantity.  If several threads are retrying then which one succeeds next is undefined -- an
-- unlucky thread might starve.
waitN :: SSem -> Int-> IO ()
waitN s i = atomically (S.waitN s i)

-- | Signal that single unit of the semaphore is available.  This increases the available quantity
-- by one.
signal :: SSem -> IO ()
signal = atomically . S.signal

-- | Signal that many units of the semaphore are available.  This changes the available quantity by
-- adding the passed size.
signalN :: SSem-> Int -> IO ()
signalN s i = atomically (S.signalN s i)

-- | Non-waiting version of wait.  `tryWait s` is defined as `tryWaitN s 1`
tryWait :: SSem -> IO (Maybe Int)
tryWait = atomically . S.tryWait

-- | Non-waiting version of waitN.  It either takes the quantity from the semaphore like
-- waitN and returns `Just value taken` or finds insufficient quantity to take and returns
-- Nothing
tryWaitN :: SSem -> Int -> IO (Maybe Int)
tryWaitN s i = atomically (S.tryWaitN s i)

-- | This returns the current quantity in the semaphore.  This is diffucult to use due to race conditions.
getValue :: SSem -> IO Int
getValue = atomically . S.getValue
