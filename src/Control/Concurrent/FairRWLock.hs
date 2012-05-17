{-# LANGUAGE DeriveDataTypeable, PatternGuards #-}
{-| Provides a fair RWLock, similar to one from Java.

 <http://download.oracle.com/javase/7/docs/api/java/util/concurrent/locks/ReentrantReadWriteLock.html>

 There are complicated policy choices that have to be made.  This policy choices here are different
 from the ones for the RWLock in concurrent-extras.

 The preferred way to use this API is sticking to 'new', 'withRead', and 'withWrite'.

 The readers and writers are always identified by their ThreadId.  Each thread that calls
 acquireRead must later call releaseRead from the same thread.  Each thread that calls acquireWrite
 must later call releaseWrite from the same thread.

 The main way to misuse a FairRWLock is to call a release without having called an acquire.  This is
 reported in the (Left error) outcomes from releaseRead and releaseWrite.  If the FairRWLock has a
 bug and finds itself in an impossible state then it will throw an error.

 The FairRWLock may be in a free unlocked state, it may be in a read locked state, and it may be a
 write locked state.  Many running threads may hold the read lock and execute concurrently.  Only
 one running thread may hold the write lock.  The scheduling is a fair FIFO queue that avoids
 starvation.

 When in the read lock state the first acquireWrite will block, and subsequent acquireRead and
 acquireWrite will queue in order.  When in the write locked state all other threads trying to
 acquireWrite or acquireRead will queue in order.

 FairRWLock allows recursive write locks, and it allows recursive read locks, and it allows the
 write lock holding thread to acquire read locks.  When the current writer also holds read locks and
 then releases its last write lock it will immediately convert to the read locked state (and other
 waiting readers may join it).  When a reader acquires a write lock it will (1) release all its read
 locks, (2) wait to acquire the write lock, (3) retake the same number of read locks released in
 (1).

 No sequence of calling acquire on a single RWLock should lead to deadlock.

-}
module Control.Concurrent.FairRWLock
  ( RWLock, RWLockException(..), RWLockExceptionKind(..),FRW(..),LockKind(..),TMap,TSet
  , new, peekLock, checkLock
  , acquireRead, acquireWrite
  , releaseRead, releaseWrite
  , withRead, withWrite
  ) where

import Control.Applicative(liftA2)
import Control.Concurrent
import Control.Exception
import Control.Monad((>=>),join,forM_)
import Data.Sequence((<|),(|>),(><),Seq,ViewL(..),ViewR(..))
import qualified Data.Sequence as Seq(empty,viewl,viewr,breakl,spanl)
import qualified Data.Foldable as F(toList)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Typeable(Typeable)

-- Try to make most impossible data states unrepresentable
type TMap = Map ThreadId Int -- nonempty, all values > 0
type TSet = Set ThreadId     -- nonempty

data LockKind = ReaderKind { unRK :: TSet }
              | WriterKind { unWK :: ThreadId }
  deriving (Eq,Ord,Show)

-- LockQ may be empty
-- No duplicate ThreadIds in LockKinds
-- MVar () will be created empty, released once with putMVar
type LockQ = Seq (LockKind,MVar ())

data LockUser = 
    FreeLock
  | Readers { readerCounts :: TMap -- re-entrant count of reader locks held be each thread
            , queueR :: Maybe ( (ThreadId,MVar ())    -- empty or queue with leading Writer
                              , LockQ )
            }
  | Writer { writerID :: ThreadId
           , writerCount           -- re-entrant writer locks held by writerID, at least 1
           , readerCount :: !Int   -- re-entrant reader locks held by writerID, at least 0
           , queue :: LockQ }
  deriving (Eq,Typeable)


-- | Opaque type of the fair RWLock.
newtype RWLock = RWL (MVar LockUser)

-- | Exception type thrown or returned by this module.  \"Impossible\" conditions get the error thrown
--  and usage problems get the error returned.
data RWLockException = RWLockException ThreadId RWLockExceptionKind String
  deriving (Show,Typeable)

-- | Operation in which error arose
data RWLockExceptionKind = RWLock'acquireWrite | RWLock'releaseWrite
                         | RWLock'acquireRead  | RWLock'releaseRead
  deriving (Show,Typeable)

instance Exception RWLockException

-- | Observable state of holders of lock.  The W returns a pair of Ints where the first is number of
-- read locks (at least 0) and the second is the number of write locks held (at least 1).  The R
-- returns a map from thread id to the positive number of read locks held.
data FRW = F | R TMap | W (ThreadId,(Int,Int)) deriving (Show)

-- | Create a new RWLock which starts in a free and unlocked state.
new :: IO RWLock
new = fmap RWL (newMVar FreeLock)

-- | This is by far the preferred way to acquire a read lock.  This uses bracket_ to ensure
-- acquireRead and releaseRead are called correctly around the passed command.
--
-- This ought to ensure releaseRead will not return a (Left error), but if it does then this error
-- will be thrown.
--
-- This can block and be safely interrupted.
withRead :: RWLock -> IO a -> IO a
withRead = liftA2 bracket_ acquireRead (releaseRead >=> either throw return)

-- | This is by far the preferred way to acquire a write lock.  This uses bracket_ to ensure
-- acquireWrite and releaseWrite are called correctly around the passed command.
--
-- This ought to ensure releaseWrite will not return a (Left error), but if it does then this error
-- will be thrown.
--
-- This can block and be safely interrupted.
withWrite :: RWLock -> IO a -> IO a
withWrite = liftA2 bracket_ acquireWrite (releaseWrite >=> either throw return)

-- | Observe which threads are holding the lock and which threads are waiting (in order).  This is
-- particularly useful for writing tests.
peekLock :: RWLock -> IO (FRW,[LockKind])
peekLock (RWL rwlVar) = withMVar rwlVar $ \ rwd -> return $
  case rwd of
    FreeLock -> (F,[])
    Readers { readerCounts=rcs, queueR=qr } -> (R rcs,maybe [] (\((t,_),q) -> WriterKind t : map fst (F.toList q)) qr)
    Writer { writerID=it, writerCount=wc, readerCount=rc, queue=q } -> (W (it,(rc,wc)), map fst (F.toList q))

-- | checkLocks return a pair of numbers, the first is the count of read locks this thread holds,
-- the second is the number of write locks that this thread holds.  This may be useful for sanity
-- checking complex usage of RWLocks.
--
-- This may block and be safely interrupted.
checkLock :: RWLock -> IO (Int,Int)
checkLock (RWL rwlVar) = do
  me <- myThreadId
  withMVar rwlVar $ \ rwd -> return $
    case rwd of
      FreeLock -> (0,0)
      Readers { readerCounts=rcs } ->
        case Map.lookup me rcs of
          Nothing -> (0,0)
          Just rc -> (rc,0)
      Writer { writerID=it, writerCount=wc, readerCount=rc } ->
        if it==me then (rc,wc) else (0,0)

-- | A thread that calls acquireRead must later call releaseRead once for each call to acquireRead.
--
-- If this thread has not previous called acquireRead then releaseRead will do nothing and return a
-- (Left error).
--
-- This can block but cannot be interrupted.
releaseRead :: RWLock -> IO (Either RWLockException ())
releaseRead (RWL rwlVar) = uninterruptibleMask_ $ do
  me <- myThreadId
  releaseRead' False me rwlVar -- False to indicate called from releaseRead

-- Eleven non-impossible cases, plus one impossible case
-- Lock is Free, error or impossible
-- I have write lock, I have no read lock, error or impossible
--                  , I have at least one read lock, just decrement the counter
-- Someone else has write lock, abandoning my acquireWrite
--                            , releaseWrite called in error
-- Read lock held, I have 1 read lock, no other readers, change to FreeLock
--                                                     , change to next Writer
--                                   , remove and leave to other readers
--               , I have more than one read lock, just decrement the counter
--               , I have no read lock, abandoning with no queue is IMPOSSIBLE
--                                    , abandoning from queue past next writer
--                                    , releaseRead called in error
releaseRead' :: Bool -> ThreadId -> MVar LockUser -> IO (Either RWLockException ())
releaseRead' abandon me rwlVar = uninterruptibleMask_ . modifyMVar rwlVar $ \ rwd -> do
  let impossible :: Show x => String -> x -> IO a
      impossible s x = throw
        (RWLockException me (if abandon then RWLock'acquireRead else RWLock'releaseRead) (imp s x))
      err :: Show x => String -> x -> IO (LockUser,Either RWLockException ())
      err s x = return . ((,) rwd) . Left $
        (RWLockException me (if abandon then RWLock'acquireRead else RWLock'releaseRead) (s++" : "++show x))
      ret :: LockUser -> IO (LockUser,Either RWLockException ())
      ret x = return (x,Right ())

      -- if there is a bug then dropReader may find an impossible situation when abandoning a thread, and throw an error
      dropReader :: LockQ -> IO LockQ
      dropReader q = do
        let inR (ReaderKind rcs,_) = Set.member me rcs
            inR _ = False
            (pre,myselfPost) = Seq.breakl inR q
        case Seq.viewl myselfPost of
          EmptyL ->
            impossible "failure to abandon acquireRead, RWLock locked by other thread(s) and this thread is not in queue" me
          (myself,mblock) :< post -> do
            let rcs' = Set.delete me (unRK myself) -- safe unRK call
            evaluate $ if Set.null rcs' then pre >< post else pre >< ((ReaderKind rcs',mblock) <| post)

  case rwd of
    FreeLock | abandon ->
      impossible "acquireRead interrupted with unlocked RWLock" me
             | otherwise ->
      err "cannot releaseRead lock from unlocked RWLock" me

    w@(Writer { writerID=it, readerCount=rc, queue=q }) | it==me -> do
      case rc of
        0 | abandon -> impossible "acquireRead interrupted with write lock but not read lock" (me,it)
          | otherwise -> err "releaseRead when holding write lock but not read lock" (me,it)
        _ -> do
          rc' <- evaluate $ pred rc
          ret (w { readerCount=rc' })

    {-ditto-}                                           | abandon -> do
      q' <- dropReader q
      ret (w { queue=q' })

    {-ditto-}                                           | otherwise ->
      err "releaseRead called when not read locked " me

    r@(Readers { readerCounts=rcs,queueR=qR }) ->
      case Map.lookup me rcs of
        Just 1 -> do
          let rcs' = Map.delete me rcs
          if Map.null rcs'
            then case qR of
                   Nothing ->
                     ret FreeLock
                   Just ((wid,mblock),q) -> do
                     putMVar mblock ()
                     ret (Writer { writerID=wid, writerCount=1, readerCount=0, queue=q })
            else ret (r { readerCounts=rcs' })

        Just rc -> do
          rc' <- evaluate $ pred rc
          rcs' <- evaluate $ Map.insert me rc' rcs
          ret (r { readerCounts=rcs' })

        Nothing   | abandon ->
          case qR of
            Nothing ->
              impossible "acquireRead interrupted not holding lock and with no queue" (me,rcs)
            Just (w,q) -> do
              q' <- dropReader q
              ret (r { queueR = Just (w,q') })

        {-ditto-} | otherwise -> 
          err "releaseRead called with read lock held by others" (me,rcs)

-- | A thread that calls acquireWrite must later call releaseWrite once for each call to acquireWrite.
--
-- If this thread has not previous called acquireWrite then releaseWrite will do nothing and return
-- a (Left error).
--
-- This can block but cannot be interrupted.
releaseWrite :: RWLock -> IO (Either RWLockException ())
releaseWrite (RWL rwlVar) = uninterruptibleMask_ $ do
  me <- myThreadId
  releaseWrite' False me rwlVar  -- False to indicate called from releaseWrite

-- Nine non-impossible cases, plus one impossible case
-- Lock is Free
-- I have write lock, I only had 1 write lock and no read locks, promote from LockQ
--                  , I only had 1 write lock and some read locks, convert me to reader and promote leading readers
--                  , I have many write locks, just decrement the counter
-- Someone else has write lock, abandoning my acquireWrite
--                            , releaseWrite called in error
-- Read lock held, releaseWrite called in error
--               , with no queue, abandoning acquireWrite is IMPOSSIBLE
--               , abandoning my leading acquireWrite
--               , abandoning my non-leading acquireWrite
releaseWrite' :: Bool -> ThreadId -> MVar LockUser -> IO (Either RWLockException ())
releaseWrite' abandon me rwlVar = uninterruptibleMask_ . modifyMVar rwlVar $ \ rwd -> do
  let impossible :: Show x => String -> x -> IO a
      impossible s x = throw
        (RWLockException me (if abandon then RWLock'acquireWrite else RWLock'releaseWrite) (imp s x))
      err :: Show x => String -> x -> IO (LockUser,Either RWLockException ())
      err s x = return . ((,) rwd) . Left $
        (RWLockException me (if abandon then RWLock'acquireWrite else RWLock'releaseWrite) (s++" : "++show x))
      ret :: LockUser -> IO (LockUser,Either RWLockException ())
      ret x = return (x,Right ())

      dropWriter :: LockQ -> IO LockQ
      dropWriter q = do
        let inW (WriterKind it,_) = me==it
            inW _ = False
            (pre,myselfPost) = Seq.breakl inW q
        case Seq.viewl myselfPost of
          EmptyL -> 
            impossible "failure to abandon acquireWrite, RWLock locked by other and not in queue" me
          _ :< post ->
            evaluate $ pre><post

  case rwd of
    FreeLock | abandon ->
     impossible "acquireWrite interrupted with unlocked RWLock" me
             | otherwise ->
     err "cannot releaseWrite lock from unlocked RWLock" me

    w@(Writer { writerID=it, writerCount=wc, readerCount=rc, queue=q }) | it==me -> do
      case (wc,rc) of
        (1,0) -> ret =<< promote q  -- if abandon then this is the only valid case
        _ | abandon -> impossible "acquireWrite interrupted with write lock and bad RWLock state" (me,it,wc,rc)
        (1,_) -> ret =<< promoteReader rc q
        (_,_) -> ret (w { writerCount=(pred wc) })

    {-ditto-}                                                          | abandon -> do
      q' <- dropWriter q
      ret (w { queue=q' })

    {-ditto-}                                                          | otherwise  -> do
      err "cannot releaseWrite when not not holding the write lock" (me,it)

    Readers { readerCounts=rcs} | not abandon ->
      err "cannot releaseWrite when RWLock is read locked" (me,rcs)
          
    Readers { readerCounts=rcs, queueR=Nothing } ->
      impossible "failure to abandon acquireWrite, RWLock read locked and no queue" (me,rcs)

    r@(Readers { readerCounts=rcs, queueR=Just (w@(it,_),q) }) | it==me -> do
      (rcs'new,qr) <- splitReaders q
      ret (r { readerCounts=Map.union rcs rcs'new, queueR=qr })

    {- ditto -}                                                | otherwise -> do
      q' <- dropWriter q
      ret (r { queueR=Just (w,q') })

 where
  -- | promote when converting from write lock straight to read lock
  promoteReader :: Int -> LockQ -> IO LockUser
  promoteReader rc q = do
    (rcs'new, qr) <- splitReaders q
    let rcs = Map.insert me rc rcs'new
    return (Readers { readerCounts=rcs, queueR=qr })

  -- | promote from releasing write lock
  promote :: LockQ -> IO LockUser
  promote qIn = do
    case Seq.viewl qIn of
      EmptyL -> return FreeLock

      (WriterKind it,mblock) :< qOut -> do
        putMVar mblock ()
        return (Writer { writerID=it, writerCount=1, readerCount=0, queue=qOut })

      _ -> do
        (rcs,qr) <- splitReaders qIn
        return (Readers { readerCounts=rcs, queueR=qr })

  -- | Merge (and wake) any and all readers on left end of LockQ, and return queueR value
  splitReaders :: LockQ -> IO (TMap,Maybe ((ThreadId,MVar ()),LockQ))
  splitReaders qIn = do
    let (more'Readers,qTail) = Seq.spanl isReader qIn
        (rks,mblocks) = unzip (F.toList more'Readers)
        rcs = Map.fromDistinctAscList . map (\k -> (k,1)) . F.toList . Set.unions . map unRK $ rks
        qr = case Seq.viewl qTail of
              EmptyL -> Nothing
              (wk,mblock) :< qOut -> Just ((unWK wk,mblock),qOut) -- unWK safe
    forM_ mblocks (\mblock -> putMVar mblock ())
    return (rcs,qr)
   where
    isReader (ReaderKind {},_) = True
    isReader _ = False

-- Six cases:
-- Lock is Free
-- I already have write lock
-- Someone else has write lock, mblock
-- I alread have read lock
-- Someone else has read lock, no pending write lock
-- Someone else has read lock, there is a pending write lock, mblock

-- | Any thread may call acquireRead (even ones holding write locks).  This read lock may be
-- acquired multiple times, requiring an identical number of releaseRead calls.
--
-- All previous calls to acquireWrite by other threads will have succeeded and been released (or
-- interrupted) before this acquireRead will return.
--
-- The best way to use acquireRead is to use withRead instead to ensure releaseRead will be called
-- exactly once.
--
-- This may block and be safely interrupted.  If interrupted then the RWLock will be left unchanged.
acquireRead :: RWLock -> IO ()
acquireRead (RWL rwlVar) = mask_ . join . modifyMVar rwlVar $ \ rwd -> do
  me <- myThreadId
  let safeBlock mblock = (readMVar mblock) `onException` (releaseRead' True me rwlVar)
  case rwd of
    FreeLock -> 
      return ( Readers { readerCounts=Map.singleton me 1, queueR=Nothing }
             , return () )

    w@(Writer { writerID=it, readerCount=rc, queue=q }) | it == me -> do
      rc' <- evaluate $ succ rc
      return ( w { readerCount=rc' }
             , return () )
                                                        | otherwise -> do
      (q',mblock) <- enterQueueR q me
      return ( w { queue = q' }
             , safeBlock mblock )

    r@(Readers { readerCounts=rcs }) | Just rc <- Map.lookup me rcs -> do
      rc' <- evaluate $ succ rc
      rcs' <- evaluate $ Map.insert me rc' rcs
      return ( r { readerCounts=rcs' }
             , return () )

    r@(Readers { readerCounts=rcs, queueR=Nothing }) -> do
      rcs' <- evaluate $ Map.insert me 1 rcs
      return ( r { readerCounts=rcs' }
             , return () )

    r@(Readers { queueR=Just (w,q) }) -> do
      (q',mblock) <- enterQueueR q me
      return ( r { queueR=Just (w,q') }
             , safeBlock mblock )
 where
  -- Merge adjacent readers when adding to right end of LockQ
  enterQueueR :: LockQ -> ThreadId -> IO (LockQ,MVar ())
  enterQueueR qIn me = do
    case Seq.viewr qIn of
      pre :> (ReaderKind rcs,mblock) -> do
        rcs' <- addMe rcs
        return (pre |> (ReaderKind rcs', mblock),mblock)
      _ -> do
        mblock <- newEmptyMVar
        return (qIn |> (ReaderKind (Set.singleton me),mblock), mblock)
   where
    -- Paranoid check of design assertion, TODO: remove check
    addMe :: TSet -> IO TSet
    addMe rcs | Set.member me rcs = error (imp "enterQueueR.addMe when already in set" me)
              | otherwise = return (Set.insert me rcs)

-- This is not exported.  This has uninterruptibleMask_.  It is used to restore read locks released
-- during acquireWrite when acquireWrite is called while holding read locks.  If this acquireWrite
-- upgrade is going well then this thread holds the Writer lock and acquireReadPriority is identical
-- to acquireRead.  If this acquireWrite gets interrupted then acquireReadPriority will to obtain
-- the read lock or put itself at the front of the queue if another thread holds the write lock.
acquireReadPriority :: RWLock -> IO ()
acquireReadPriority (RWL rwlVar) = uninterruptibleMask_ . join . modifyMVar rwlVar $ \ rwd -> do
  me <- myThreadId
  let safeBlock mblock = (readMVar mblock) `onException` (releaseRead' True me rwlVar)
  case rwd of
    FreeLock -> 
      return ( Readers { readerCounts=Map.singleton me 1, queueR=Nothing }
             , return () )

    w@(Writer { writerID=it, readerCount=rc, queue=q }) | it == me -> do
      rc' <- evaluate $ succ rc
      return ( w { readerCount=rc' }
             , return () )
                                                        | otherwise -> do
      (q',mblock) <- enterQueueL me q
      return ( w { queue = q' }
             , safeBlock mblock )

    r@(Readers { readerCounts=rcs }) -> do
      case Map.lookup me rcs of
        Just rc -> do
          rc' <- evaluate $ succ rc
          rcs' <- evaluate $ Map.insert me rc' rcs
          return ( r { readerCounts=rcs' }
                 , return () )

        Nothing -> do
          rcs' <- evaluate $ Map.insert me 1 rcs
          return ( r { readerCounts=rcs' }
                 , return () )
 where
  -- Merge adjacent readers when adding to right end of LockQ
  enterQueueL :: ThreadId -> LockQ -> IO (LockQ,MVar ())
  enterQueueL me qIn = do
    case Seq.viewl qIn of
      (ReaderKind rcs,mblock) :< post -> do
        rcs' <- addMe rcs
        return ((ReaderKind rcs', mblock) <| post,mblock)
      _ -> do
        mblock <- newEmptyMVar
        return ((ReaderKind (Set.singleton me),mblock) <| qIn , mblock)
   where
    -- Paranoid check of design assertion, TODO: remove check
    addMe :: TSet -> IO TSet
    addMe rcs | Set.member me rcs = error (imp "enterQueueL.addMe when already in set" me)
              | otherwise = return (Set.insert me rcs)

-- Six cases:
-- Lock is Free
-- I already have write lock
-- Someone else has write lock
-- I already have read lock
-- Someone else has read lock, there is no pending write lock
-- Someone else has read lock, there is a pending write lock

-- | Any thread may call acquireWrite (even ones holding read locks, but see below for interrupted
-- behavior).  This write lock may be acquired multiple times, requiring an identical number of
-- releaseWrite calls.
--
-- All previous calls to acquireRead by other threads will have succeeded and been released (or
-- interrupted) before this acquireWrite will return.
--
-- The best way to use acquireWrite is to use withWrite instead to ensure releaseWrite will be
-- called exactly once.
--
-- This may block and usually be safely interrupted.  If interrupted then the RWLock will be left
-- unchanged.  The exception to being able to interrupted when this blocks is very subtle: if this
-- thread holds read locks and calls acquireWrite then it will release those read locks and go to
-- the back of the queue to acquire the write lock (it does not get to skip the queue).  While
-- blocking waiting for the write lock to be available this thread may be interrupted.  If not
-- interrupted then the write lock will eventually be acquired, followed by re-acquiring the
-- original number of read locks.  But if acquireWrite is interrupted after releasing read locks
-- then it MUST restore those read locks on the way out.  To do this the internal error handler will
-- use 'uninterruptibleMask_' and a special version of acquireRead that skips to the front of the
-- queue; when the current lock state is a reader this works instantly but when the current lock
-- state is a writer this thread will block in an UNINTERRUPTIBLE state until the current writer is
-- finished.  Once this other writer is finished the error handler will obtain the read locks it
-- needs to allow the error propagation to continue.
acquireWrite :: RWLock -> IO ()
acquireWrite rwl@(RWL rwlVar) = mask_ . join . modifyMVar rwlVar $ \ rwd -> do
  me <- myThreadId
  let safeBlock mblock = (takeMVar mblock) `onException` (releaseWrite' True me rwlVar)
  case rwd of
    FreeLock ->
      return ( Writer { writerID=me, writerCount=1, readerCount=0, queue=Seq.empty }
             , return () )

    w@(Writer { writerID=it, writerCount=wc, queue=q }) | it==me ->
      return ( w { writerCount=(succ wc) }
             , return () )

    {-ditto-}                                           | otherwise -> do
      mblock <- newEmptyMVar
      q' <- evaluate $ q |> (WriterKind me,mblock)
      return ( w { queue=q' }
             , safeBlock mblock )

    Readers { readerCounts=rcs } | Just rc <- Map.lookup me rcs -> do
      return ( rwd
             , withoutReads rc (acquireWrite rwl) )

    r@(Readers { queueR=Nothing }) -> do
      mblock <- newEmptyMVar
      let qr = Just ((me,mblock),Seq.empty)
      return ( r { queueR=qr }
             , safeBlock mblock )

    r@(Readers { queueR=Just (w,q) }) -> do
      mblock <- newEmptyMVar
      q' <- evaluate $ q |> (WriterKind me,mblock)
      return ( r { queueR=Just (w,q') }
             , safeBlock mblock )
 where
  withoutReads :: Int -> IO a -> IO a
  withoutReads n x = foldr (.) id (replicate n withoutRead) $ x
  withoutRead :: IO a -> IO a
  withoutRead = bracket_ (releaseRead rwl >>= either throw return) (acquireReadPriority rwl)

-- format impossible error strings to include standard description prefix
imp :: Show x => String -> x -> String
imp s x = "FairRWLock impossible error: "++s++" : "++show x

{-

subtle bug #1:

When converting from a read lock holding 'rc' read locks to a also holding a write lock, I first wrote:

replicateM_ rc (releaseRead rwl >>= either throw return)
acquireWrite rwl
replicateM_ rc (acquireRead rwl)

Imagine there are rc copies of withRead wrapped around the above:
withRead = liftA2 bracket_ acquireRead (releaseRead >=> either throw return)

Then the acquireWrite blocks and gets interrupted.
The releaseReads in the withRead will see a strange situation (not locked!) and call throw.

What is the answer? reverse the bracket for the release/acquire? Hmm..

-}