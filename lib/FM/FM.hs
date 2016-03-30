{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FM.FM ( 
  runCacheOnly, runSessionOnly, runPlayerOnly
, CacheOnly, SessionOnly, PlayerOnly

, initPlayer
, PlayerState (..)
, Player, playerState, playerVolume, playerMuted
, isStopped
, play, pause, resume, stop, updateVolume

, IsSession
, SomeSession (..)
, fromSession

, Cache
, initCache
, cacheSong
, waitCaching
) where

import Control.Monad.Reader

import FM.CacheManager
import FM.Player
import FM.Session

newtype CacheOnly a = CacheOnly (ReaderT Cache IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cache)
 
newtype SessionOnly s a = SessionOnly (ReaderT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s)

newtype PlayerOnly a = PlayerOnly (ReaderT Player IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Player)

runCacheOnly :: Cache -> CacheOnly a -> IO a
runCacheOnly cache (CacheOnly m) = runReaderT m cache

runSessionOnly :: (IsSession s) => SomeSession -> SessionOnly s a -> IO a
runSessionOnly session (SessionOnly m) = runReaderT m (fromSession session)

runPlayerOnly :: Player -> PlayerOnly a -> IO a
runPlayerOnly player (PlayerOnly m) = runReaderT m player
