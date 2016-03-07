{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FM.Player ( 
  PlayerState (..)
, isPlaying, isPaused, isStopped
, Player (..)
, play
, pause
, resume
, stop
, setVolume
) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Concurrent (ThreadId, forkFinally, killThread, myThreadId, throwTo)
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar

import           Control.Exception (asyncExceptionFromException, AsyncException (..))
import           Control.Monad.STM (atomically)
import           Data.List (isPrefixOf)

import           System.IO
import           System.Process (ProcessHandle, runInteractiveProcess, terminateProcess)

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
, currentLyrics :: TMVar Song.Lyrics
}

type FetchLyrics = Song.Song -> IO Song.Lyrics
type Notify a = a -> IO ()

play :: (MonadIO m, MonadReader Player m) => Song.Song -> Int -> FetchLyrics -> Notify Bool -> Notify (Double, Double) -> m ()
play song@Song.Song {..} volume fetchLyrics onTerminate onUpdate = do
  Player {..} <- ask
  (inHandle, outHandle, _, processHandle) <- liftIO $ runInteractiveProcess "mpg123" ["-R"] Nothing Nothing
  let initHandle h = liftIO $ do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering
  initHandle inHandle
  initHandle outHandle
  exitLock <- liftIO $ atomically $ newTMVar ()
  let playerThread = do
        atomically $ putTMVar exitLock ()
        atomically . putTMVar currentLyrics =<< fetchLyrics song
        hPutStrLn inHandle $ "V " ++ show volume
        hPutStrLn inHandle $ "L " ++ url
        loop (outHandle, 0, 0) True =<< hGetLine outHandle
  let cleanUp e = do
        PlayerContext {..} <- atomically $ readTMVar playerContext
        terminateProcess processHandle
        atomically $ do
          writeTVar playerState Stopped
          tryTakeTMVar currentLyrics
          takeTMVar playerContext
        case e of
          Right _ -> onTerminate True
          Left e -> case asyncExceptionFromException e of
                      Just ThreadKilled -> onTerminate False
                      _ -> throwTo parentThreadId e
  parentThreadId <- liftIO myThreadId
  childThreadId <- liftIO $ forkFinally playerThread cleanUp
  void $ liftIO $ atomically $ do
    putTMVar playerContext PlayerContext {..}
    writeTVar playerState (Playing song)
    takeTMVar exitLock
    where
      loop :: (Handle, Double, Double) -> Bool -> String -> IO ()

      loop (h, _, _) True out@('@':'F':_) = do
        let l = read (words out !! 4)
        onUpdate (l, 0)
        loop (h, l, 0) False =<< hGetLine h

      loop ctx@(h, _, _) True _ = loop ctx True =<< hGetLine h
      
      loop (h, l, c) False out@('@':'F':_) = do
        let c' = read (words out !! 3)
        when (floor c' /= floor c) $ onUpdate (l, c')
        loop (h, l, c') False =<< hGetLine h

      loop ctx@(h, l, c) False out 
        | "@P 0" `isPrefixOf` out = onUpdate (l, c)
        | otherwise = loop ctx False =<< hGetLine h

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

setVolume :: (MonadIO m, MonadReader Player m) => Int -> m ()
setVolume volume = do
  Player {..} <- ask
  h <- fmap inHandle <$> liftIO (atomically $ tryReadTMVar playerContext)
  maybe (return ()) (\h -> liftIO (hPutStrLn h $ "V " ++ show volume)) h
