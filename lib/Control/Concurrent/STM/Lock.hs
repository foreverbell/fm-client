module Control.Concurrent.STM.Lock (
  Lock
, LockState (..)
, newLock
, newLockIO
, newAcquiredLock
, newAcquiredLockIO
, acquireLock
, releaseLock
, waitLock
, viewLock
) where

import Control.Concurrent.STM
import Control.Monad (void)

newtype Lock = Lock { un :: TMVar () }
data LockState = Acquired | Released deriving (Eq)

newLock :: STM Lock
newLock = Lock <$> newTMVar ()

newLockIO :: IO Lock
newLockIO = Lock <$> newTMVarIO ()

newAcquiredLock :: STM Lock
newAcquiredLock = Lock <$> newEmptyTMVar

newAcquiredLockIO :: IO Lock
newAcquiredLockIO = Lock <$> newEmptyTMVarIO

acquireLock :: Lock -> STM ()
acquireLock = takeTMVar . un

releaseLock :: Lock -> STM ()
releaseLock (Lock lock) = void $ tryPutTMVar lock ()

waitLock :: Lock -> LockState -> STM ()
waitLock (Lock lock) state = case state of
  Acquired -> putTMVar lock () >> takeTMVar lock
  Released -> takeTMVar lock >> putTMVar lock ()

viewLock :: Lock -> STM LockState
viewLock (Lock lock) = do
  locked <- isEmptyTMVar lock
  return $ if locked then Acquired else Released
