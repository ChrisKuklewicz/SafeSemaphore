-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.STM.SSem
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
module Control.Concurrent.STM.SSem(SSem, new, wait, signal, tryWait
                                  , waitN, signalN, tryWaitN
                                  , getValue) where

import Control.Monad.STM(STM,retry)
import Control.Concurrent.STM.TVar(newTVar,readTVar,writeTVar)
import Control.Concurrent.STM.SSemInternals(SSem(SSem))

-- | Create a new semaphore with the given argument as the initially available quantity.  This
-- allows new semaphores to start with a negative, zero, or positive quantity.
new :: Int -> STM SSem
new = fmap SSem . newTVar

-- | Try to take a unit of value from the semaphore.  This succeeds when the current quantity is
-- positive, and then reduces the quantity by one.  Otherwise this will 'retry'.  This will never
-- result in a negative quantity.  If several threads are retying then which one succeeds next is
-- undefined -- an unlucky thread might starve.
wait :: SSem -> STM ()
wait = flip waitN 1

-- | Try to take the given value from the semaphore.  This succeeds when the quantity is greater or
-- equal to the given value, and then subtracts the given value from the quantity.  Otherwise this
-- will 'retry'.  This will never result in a negative quantity.  If several threads are retrying
-- then which one succeeds next is undefined -- an unlucky thread might starve.
waitN :: SSem -> Int -> STM ()
waitN (SSem s) i = do
  v <- readTVar s
  if v >= i
    then writeTVar s $! v-i
    else retry

-- | Signal that single unit of the semaphore is available.  This increases the available quantity
-- by one.
signal :: SSem -> STM ()
signal = flip signalN 1

-- | Signal that many units of the semaphore are available.  This changes the available quantity by
-- adding the passed size.
signalN :: SSem -> Int -> STM ()
signalN (SSem s) i = do
  v <- readTVar s
  writeTVar s $! v+i

-- | Non-retrying version of 'wait'.  `tryWait s` is defined as `tryN s 1`
tryWait :: SSem -> STM (Maybe Int)
tryWait = flip tryWaitN 1

-- | Non-retrying version of waitN.  It either takes the quantity from the semaphore like
-- waitN and returns `Just value taken` or finds insufficient quantity to take and returns
-- Nothing
tryWaitN :: SSem -> Int -> STM (Maybe Int)
tryWaitN (SSem s) i = do
  v <- readTVar s
  if v >= i
    then do writeTVar s $! v-i
            return (Just i)
    else return Nothing

-- | Return the current quantity in the semaphore.  This is potentially useful in a larger STM
-- transaciton and less useful as `atomically getValueSem :: IO Int` due to race conditions.
getValue :: SSem -> STM Int
getValue (SSem s) = readTVar s
