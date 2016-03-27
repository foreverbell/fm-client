{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FM.Player (
  PlayerState (..)
, isPlaying, isPaused, isStopped
, Player (..)
, initPlayer
, play
, pause
, resume
, stop
, setVolume
, mute
) where

import           Control.Concurrent (ThreadId, forkFinally, killThread, myThreadId, throwTo)
import           Control.Concurrent.Async (async, cancel, poll)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception (asyncExceptionFromException, AsyncException (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM (atomically)
import           System.IO
import           System.Process (ProcessHandle, runInteractiveProcess, waitForProcess, terminateProcess)

import qualified FM.Song as Song

data PlayerState = Playing Song.Song
                 | Paused Song.Song
                 | Stopped

isPlaying :: PlayerState -> Bool
isPlaying (Playing _) = True
isPlaying _ = False

isPaused :: PlayerState -> Bool
isPaused (Paused _) = True
isPaused _ = False

isStopped :: PlayerState -> Bool
isStopped Stopped = True
isStopped _ = False

data PlayerContext = PlayerContext {
  inHandle       :: Handle
, outHandle      :: Handle
, processHandle  :: ProcessHandle
, parentThreadId :: ThreadId
, childThreadId  :: ThreadId
}

data Player = Player {
  playerContext :: TMVar PlayerContext
, playerState   :: TVar PlayerState
, stoppedSignal :: TMVar ()
}

initPlayer :: (MonadIO m) => m Player
initPlayer = liftIO $ do
  playerContext <- newEmptyTMVarIO
  playerState <- newTVarIO Stopped
  stoppedSignal <- newTMVarIO ()
  return Player {..}

type FetchLyrics = Song.Song -> IO Song.Lyrics
type Notify a = a -> IO ()

play :: (MonadIO m, MonadReader Player m) => Song.Song -> Int -> FetchLyrics -> Notify Bool -> Notify (Double, Double) -> Notify String -> m ()
play song@Song.Song {..} volume fetchLyrics onTerminate onProgress onLyrics = do
  Player {..} <- ask
  (inHandle, outHandle, _, processHandle) <- liftIO $ runInteractiveProcess "mpg123" ["-R"] Nothing Nothing
  let initHandle h = liftIO $ do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering
  initHandle inHandle
  initHandle outHandle
  exitLock <- liftIO $ atomically $ newTMVar ()
  lyricsAsync <- liftIO $ async $ fetchLyrics song

  let
    notifyLyrics :: Song.Lyrics -> Double -> Notify String -> IO Song.Lyrics
    notifyLyrics (Song.Lyrics lyrics) time notify = Song.Lyrics <$> 
      case span (\l -> fst l <= time) lyrics of
        ([], restLyrics) -> return restLyrics
        (curLyrics, restLyrics) -> do
          notify $ snd (last curLyrics)
          return restLyrics

    loop :: Bool -> (Double, Double, Maybe Song.Lyrics) -> String -> IO ()
    loop _ (len, cur, _) "@P 0" = onProgress (len, cur)

    loop True _ out@('@':'F':_) = do
      let len = read (words out !! 4)
      onProgress (len, 0)
      loop False (len, 0, Nothing) =<< hGetLine outHandle

    loop False (len, cur, lyrics) out@('@':'F':_) = do
      let cur' = read (words out !! 3)
      when (floor cur' /= floor cur) $ onProgress (len, cur')
      lyrics' <- case lyrics of
        Just lyrics -> Just <$> notifyLyrics lyrics cur' onLyrics
        Nothing -> do
          lyrics <- poll lyricsAsync
          case lyrics of
            Just (Right lyrics) -> Just <$> notifyLyrics lyrics cur' onLyrics
            _ -> return Nothing
      loop False (len, cur', lyrics') =<< hGetLine outHandle

    loop b ctx _ = loop b ctx =<< hGetLine outHandle

    playerThread = do
      atomically $ putTMVar exitLock ()
      hPutStrLn inHandle $ "V " ++ show volume
      hPutStrLn inHandle $ "L " ++ url
      loop True (0, 0, Nothing) =<< hGetLine outHandle

    cleanUp e = do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      terminateProcess processHandle
      waitForProcess processHandle
      cancel lyricsAsync
      atomically $ do
        writeTVar playerState Stopped
        takeTMVar playerContext
      case e of
        Right _ -> onTerminate True
        Left e -> case asyncExceptionFromException e of
                    Just ThreadKilled -> onTerminate False
                    _ -> throwTo parentThreadId e
      atomically $ putTMVar stoppedSignal ()

  parentThreadId <- liftIO myThreadId
  childThreadId <- liftIO $ forkFinally playerThread cleanUp
  void $ liftIO $ atomically $ do
    putTMVar playerContext PlayerContext {..}
    writeTVar playerState (Playing song)
    takeTMVar stoppedSignal
    takeTMVar exitLock

-- | pause, resume and stop are idempotent.
pause :: (MonadIO m, MonadReader Player m) => m ()
pause = do
  Player {..} <- ask
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Playing s -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      atomically $ writeTVar playerState (Paused s)
      hPutStrLn inHandle "P"
    _ -> return ()

resume :: (MonadIO m, MonadReader Player m) => m ()
resume = do
  Player {..} <- ask
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Paused s -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      atomically $ writeTVar playerState (Playing s)
      hPutStrLn inHandle "P"
    _ -> return ()

stop :: (MonadIO m, MonadReader Player m) => m ()
stop = do
  Player {..} <- ask
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Stopped -> return ()
    _ -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      killThread childThreadId
      atomically $ readTMVar stoppedSignal

setVolume :: (MonadIO m, MonadReader Player m) => Int -> m ()
setVolume volume = do
  Player {..} <- ask
  h <- fmap inHandle <$> liftIO (atomically $ tryReadTMVar playerContext)
  maybe (return ()) (\h -> liftIO (hPutStrLn h $ "V " ++ show volume)) h

mute :: (MonadIO m, MonadReader Player m) => m ()
mute = setVolume 0
