module Control.Concurrent.STM.Lock ( 
  Lock
, newLock
, newLockIO
, newAcquiredLock
, newAcquiredLockIO
, acquireLock
, tryAcquireLock
, releaseLock
, waitLock
, isLocked
) where

import Control.Concurrent.STM
import Control.Monad (void)
import Data.Maybe (isJust)

newtype Lock = Lock { un :: TMVar () }

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

tryAcquireLock :: Lock -> STM Bool
tryAcquireLock = fmap isJust . tryTakeTMVar . un

releaseLock :: Lock -> STM ()
releaseLock (Lock lock) = void $ tryPutTMVar lock ()

waitLock :: Lock -> STM ()
waitLock (Lock lock) = takeTMVar lock >> putTMVar lock ()

isLocked :: Lock -> STM Bool
isLocked = isEmptyTMVar . un
