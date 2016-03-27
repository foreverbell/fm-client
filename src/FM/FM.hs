{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FM.FM ( 
  runSessionOnly
, runPlayerOnly
, SessionOnly, PlayerOnly
, initPlayer
, PlayerState (..)
, Player
, playerState, playerVolume, playerMuted
, isPlaying, isPaused, isStopped
) where

import Control.Monad.Reader

import FM.Player
import FM.Session

newtype SessionOnly s a = SessionOnly (ReaderT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s)

newtype PlayerOnly a = PlayerOnly (ReaderT Player IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Player)

runSessionOnly :: (IsSession s) => SomeSession -> SessionOnly s a -> IO a
runSessionOnly session (SessionOnly m) = runReaderT m (fromSession session)

runPlayerOnly :: Player -> PlayerOnly a -> IO a
runPlayerOnly player (PlayerOnly m) = runReaderT m player
