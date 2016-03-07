module FM.FMState ( 
  FMState (..)
, MusicLocation
, PlayerContext (..)
, PlayerState (..)
) where

import Control.Concurrent (ThreadId)
import Control.Concurrent.STM.TMVar (TMVar)
import Control.Concurrent.STM.TVar (TVar)
import System.Process (ProcessHandle)
import System.IO (Handle)

import qualified FM.Song as Song

data PlayerState = Playing Song.Song 
                 | Paused Song.Song
                 | Stopped

data PlayerContext = PlayerContext {
  inHandle       :: Handle
, outHandle      :: Handle
, processHandle  :: ProcessHandle
, parentThreadId :: ThreadId
, childThreadId  :: ThreadId
}

type MusicLocation = (Int, Double)

data FMState = FMState {
  playerContext   :: TMVar PlayerContext
, playerState     :: TVar PlayerState
, totalLength     :: TMVar MusicLocation
, currentLocation :: TMVar MusicLocation
, currentLyrics   :: TMVar Song.Lyrics
, currentVolume   :: Int
}
