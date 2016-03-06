module FM.FMState ( 
  FMState (..)
, MusicLocation
, PlayerContext (..)
, PlayerState (..)
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.MVar (MVar)
import Data.IORef (IORef)
import System.Process (ProcessHandle)
import System.IO (Handle)

import qualified FM.Song as Song

data PlayerState = Playing Song.Song 
                 | Paused Song.Song
                 | Stop

data PlayerContext = PlayerContext {
  inHandle       :: Handle
, outHandle      :: Handle
, processHandle  :: ProcessHandle
, parentThreadId :: ThreadId
, childThreadId  :: ThreadId
}

type MusicLocation = (Int, Double)

data FMState = FMState {
  playerContext   :: MVar PlayerContext
, playerState     :: IORef PlayerState
, totalLength     :: MVar MusicLocation
, currentLocation :: MVar MusicLocation
, currentLyrics   :: MVar Song.Lyrics
, currentVolume   :: Int
}
