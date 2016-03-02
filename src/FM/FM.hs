{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FunctionalDependencies #-}

module FM.FM ( 
  FM
, runFM
, FMState (..)
, SongLocation
, PlayerHandle (..)
, PlayingState (..)
, module MonadTransformer
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad.IO.Class as MonadTransformer
import Control.Monad.Reader as MonadTransformer
import Control.Monad.State as MonadTransformer
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
, currentLyrics   :: MVar (Maybe Song.Lyrics)
, currentVolume   :: Int
}

newtype FM s a = FM (ReaderT s (StateT FMState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s, MonadState FMState)

runFM :: s -> FM s a -> IO a
runFM session (FM fm) = do
  playerHandle <- newEmptyMVar
  playingState <- newIORef Stop
  playingLength <- newEmptyMVar
  currentLocation <- newEmptyMVar
  currentLyrics <- newEmptyMVar
  let currentVolume = 100
  evalStateT (runReaderT fm session) FMState {..}
