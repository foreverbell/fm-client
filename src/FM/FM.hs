{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module FM.FM ( 
  runSessionOnly
, runPlayerOnly
, SessionOnly, PlayerOnly
, initialPlayer
, PlayerState (..)
, Player
, playerState
, isPlaying, isPaused, isStopped
) where

import Control.Concurrent.STM.TMVar (newEmptyTMVarIO)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Reader

import FM.Player
import FM.Session

newtype SessionOnly s a = SessionOnly (ReaderT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s)

newtype PlayerOnly a = PlayerOnly (ReaderT Player IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Player)

initialPlayer :: IO Player
initialPlayer = do
  playerContext <- newEmptyTMVarIO
  playerState <- newTVarIO Stopped
  currentLyrics <- newEmptyTMVarIO
  return Player {..}

runSessionOnly :: (IsSession s) => s -> SessionOnly s a -> IO a
runSessionOnly session (SessionOnly m) = runReaderT m session

runPlayerOnly :: Player -> PlayerOnly a -> IO a
runPlayerOnly player (PlayerOnly m) = runReaderT m player
