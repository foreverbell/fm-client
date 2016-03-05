{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module FM.FM ( 
  FM
, runFM
, FMState (..)
, SongLocation
, PlayerContext (..)
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

data PlayerContext = PlayerContext {
  inHandle       :: Handle
, outHandle      :: Handle
, processHandle  :: ProcessHandle
, parentThreadId :: ThreadId
, childThreadId  :: ThreadId
}

type SongLocation = (Int, Double)

data FMState = FMState {
  playerContext   :: MVar PlayerContext
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
    playerContext <- newEmptyMVar
    playingState <- newIORef Stop
    playingLength <- newEmptyMVar
    currentLocation <- newEmptyMVar
    currentLyrics <- newEmptyMVar
    let currentVolume = 100
    runStateT (runReaderT fm session) FMState {..}
