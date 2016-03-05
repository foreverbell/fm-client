{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module FM.FM ( 
  FM
, runFM
, FMState (..)
, SongLocation
, PlayerHandle (..)
, PlayingState (..)
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.IORef (IORef, newIORef)
import System.Process (ProcessHandle)
import System.IO (Handle)

import qualified FM.Song as Song

data PlayingState = Playing Song.Song 
                  | Paused Song.Song
                  | Stop

data PlayerHandle = PlayerHandle {
  forkThreadId :: ThreadId
, inHandle     :: Handle
, outHandle    :: Handle
, processId    :: ProcessHandle
}

type SongLocation = (Int, Double)

data FMState = FMState {
  playerHandle    :: MVar PlayerHandle
, playingState    :: IORef PlayingState
, playingLength   :: MVar SongLocation
, currentLocation :: MVar SongLocation
, currentLyrics   :: MVar Song.Lyrics
, currentVolume   :: Int
}

newtype FM s a = FM (ReaderT s (StateT FMState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s, MonadState FMState)

runFM :: s -> Maybe FMState -> FM s a -> IO (a, FMState)
runFM session state (FM fm) = case state of
  Just state -> runStateT (runReaderT fm session) state
  Nothing -> do
    playerHandle <- newEmptyMVar
    playingState <- newIORef Stop
    playingLength <- newEmptyMVar
    currentLocation <- newEmptyMVar
    currentLyrics <- newEmptyMVar
    let currentVolume = 100
    runStateT (runReaderT fm session) FMState {..}
