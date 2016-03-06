{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards #-}

module FM.FM ( 
  runSessionOnly
, runStateOnly
, runBoth
, SessionOnly, StateOnly, Both
, initialState
, FMState (..)
, MusicLocation
, PlayerState (..)
) where

import Control.Concurrent.MVar (newEmptyMVar)
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef (newIORef)

import FM.FMState
import FM.Session

newtype SessionOnly s a = SessionOnly (ReaderT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s)

newtype StateOnly a = StateOnly (StateT FMState IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadState FMState)

newtype Both s a = Both (ReaderT s (StateT FMState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s, MonadState FMState)

initialState :: IO FMState
initialState = do
  playerContext <- newEmptyMVar
  playerState <- newIORef Stop
  totalLength <- newEmptyMVar
  currentLocation <- newEmptyMVar
  currentLyrics <- newEmptyMVar
  let currentVolume = 100
  return $ FMState {..}

runSessionOnly :: (IsSession s) => s -> SessionOnly s a -> IO a
runSessionOnly session (SessionOnly m) = runReaderT m session

runStateOnly :: FMState -> StateOnly a -> IO (a, FMState)
runStateOnly state (StateOnly m) = runStateT m state

runBoth :: (IsSession s) => s -> FMState -> Both s a -> IO (a, FMState)
runBoth session state (Both m) = runStateT (runReaderT m session) state
