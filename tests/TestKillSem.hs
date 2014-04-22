{-# LANGUAGE CPP #-}
{- some output from log of "cabal test", three old modules fail, three new modules pass:

Test SampleVar
0: forkIO read thread 1
0: stop thread 1
1: read interrupted
0: write sv #1
0: write sv #2 with timeout
0: timeout triggered, write sv #2 blocked, FAIL


Test QSem
0: forkIO wait thread 1
0: stop thread 1
1: wait interrupted
0: signal q #1
0: forkIO wait thread 2
0: forkIO wait thread 3
0: signal q #2
2: wait done
0: stop thread 2
0: stop thread 3
3: wait interrupted (QUANTITY LOST) FAIL
False


Test QSemN
0: forkIO wait thread 1
0: stop thread 1
1: wait interrupted
0: signal q #1
0: forkIO wait thread 2
0: forkIO wait thread 3
0: signal q #2
2: wait done
0: stop thread 2
0: stop thread 3
3: wait interrupted (QUANTITY LOST) FAIL
False
Expected 3 Failures for above code



Test MSampleVar
0: forkIO read thread 1
0: stop thread 1
1: read interrupted
0: write sv #1
0: write sv #2 with timeout
0: write sv #2 returned, PASS


Test MSem
0: forkIO wait thread 1
0: stop thread 1
1: wait interrupted
0: signal q #1
0: forkIO wait thread 2
2: wait done
0: forkIO wait thread 3
0: signal q #2
3: wait done (QUANTITY CONSERVED) PASS
0: stop thread 2
0: stop thread 3
True


Test MSemN
0: forkIO wait thread 1
0: stop thread 1
1: wait interrupted
0: signal q #1
0: forkIO wait thread 2
2: wait done
0: forkIO wait thread 3
0: signal q #2
3: wait done (QUANTITY CONSERVED) PASS
0: stop thread 2
0: stop thread 3
True
Test suite TestSafeSemaphore: PASS
Test suite logged to: dist/test/SafeSemaphore-0.8.0-TestSafeSemaphore.log

-}
module Main where

import Prelude hiding (read)
import Control.Concurrent
import Control.Exception
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
import qualified Control.Concurrent.MSem as MSem
import qualified Control.Concurrent.MSemN as MSemN
import qualified Control.Concurrent.MSemN2 as MSemN2
import qualified Control.Concurrent.SSem as SSem
import Control.Concurrent.MVar
import Test.HUnit
import System.Exit
#if !MIN_VERSION_base(4,7,0)
import Control.Concurrent.SampleVar
#endif
import Control.Concurrent.MSampleVar as MSV
import System.Timeout

delay = threadDelay (1000*100)
--delay = yield -- now causes tests to fail in ghc 7.4

fork x = do m <- newEmptyMVar
            t <- forkIO (finally x (putMVar m ()))
            delay
            return (t,m)

stop (t,m) = do killThread t
                delay
                takeMVar m

-- True if test passed, False if test failed
-- This expects FIFO semantics for the waiters
testSem :: Integral n
        => String
        -> (n -> IO a)
        -> (a->IO ())
        -> (a -> IO ())
        -> IO Bool
testSem name new wait signal = do
  putStrLn ("\n\nTest "++ name)
  q <- new 0

  putStrLn "0: forkIO wait thread 1"
  (t1,m1) <- fork $ do
    wait q `onException` (putStrLn "1: wait interrupted")
    putStrLn "1: wait done UNEXPECTED"
  putStrLn "0: stop thread 1"
  stop (t1,m1)
  putStrLn "0: signal q #1"
  signal q
  delay

  putStrLn "0: forkIO wait thread 2"
  (t2,m2) <- fork $ do
    wait q `onException` (putStrLn "2: wait interrupted UNEXPECTED")
    putStrLn "2: wait done"
  delay

  result <- newEmptyMVar
  putStrLn "0: forkIO wait thread 3"
  (t3,m3) <- fork $ do
    wait q `onException` (putStrLn "3: wait interrupted (QUANTITY LOST) FAIL" >> putMVar result False)
    putStrLn "3: wait done (QUANTITY CONSERVED) PASS"
    putMVar result True
  putStrLn "0: signal q #2"
  signal q
  delay

  putStrLn "0: stop thread 2"
  stop (t2,m2)
  putStrLn "0: stop thread 3"
  stop (t3,m3)
  r <- takeMVar result
  print r
  return r

testSV name newEmpty read write = do
  putStrLn ("\n\nTest "++ name)
  sv <- newEmpty
  putStrLn "0: forkIO read thread 1"
  (t1,m1) <- fork $ do
    read sv `onException` (putStrLn "1: read interrupted")
    putStrLn "1: read done UNEXPECTED"
  putStrLn "0: stop thread 1"
  stop (t1,m1)
  putStrLn "0: write sv #1"
  write sv 1
  putStrLn "0: write sv #2 with timeout"
  m <- timeout (1000*100) (write sv 2)
  case m of
    Nothing -> do
      putStrLn "0: timeout triggered, write sv #2 blocked, FAIL"
      return False
    Just () -> do
      putStrLn "0: write sv #2 returned, PASS"
      return True

-- True if test passed, False if test failed
-- This does not expect FIFO semantics for the waiters, uses getValue instead
testSSem :: Integral n
        => String
        -> (n -> IO a)
        -> (a->IO ())
        -> (a -> IO ())
        -> (a -> IO Int)
        -> IO Bool
testSSem name new wait signal getValue = do
  putStrLn ("\n\nTest "++ name)
  q <- new 0

  putStrLn "0: forkIO wait thread 1"
  (t1,m1) <- fork $ do
    wait q `onException` (putStrLn "1: wait interrupted")
    putStrLn "1: wait done UNEXPECTED"
  putStrLn "0: stop thread 1"
  stop (t1,m1)
  putStrLn "0: signal q #1"
  signal q
  delay

  putStrLn "0: forkIO wait thread 2"
  (t2,m2) <- fork $ do
    wait q `onException` (putStrLn "2: wait interrupted")
    putStrLn "2: wait done"
  delay

  putStrLn "0: forkIO wait thread 3"
  (t3,m3) <- fork $ do
    wait q `onException` (putStrLn "3: wait interrupted")
    putStrLn "3: wait done"
  delay

  putStrLn "0: signal q #2"
  signal q
  delay

  putStrLn "0: stop thread 2"
  stop (t2,m2)
  putStrLn "0: stop thread 3"
  stop (t3,m3)
  r <- getValue q
  putStrLn $ "Final Value "++show r
  return (r==0)

#if !MIN_VERSION_base(4,7,0)
testOldSV = test $ testSV "SampleVar" newEmptySampleVar readSampleVar writeSampleVar
#else
testOldSV = test $ putStrLn "Cannot test SampleVar on GHC 7.8 because it was removed" >> return False
#endif
testNewSV = test $ testSV "MSampleVar" newEmptySV readSV writeSV

testsQ = TestList . (testOldSV:) . map test $
  [ testSem "QSem" newQSem waitQSem signalQSem
  , testSem "QSemN" newQSemN (flip waitQSemN 1) (flip signalQSemN 1)
  ]

testsM = TestList . (testNewSV:) . map test $
  [ testSem "MSem" MSem.new MSem.wait MSem.signal
  , testSem "MSemN" MSemN.new (flip MSemN.wait 1) (flip MSemN.signal 1)
  , testSem "MSemN2" MSemN2.new (flip MSemN2.wait 1) (flip MSemN2.signal 1)
  , testSSem "SSem" SSem.new SSem.wait SSem.signal SSem.getValue
  ]

-- This is run by "cabal test"
main = do
  runTestTT testsQ
  putStrLn "Expected 3 Failures for above code\n"
  c <- runTestTT testsM
  if failures c == 0 then exitSuccess else exitFailure
