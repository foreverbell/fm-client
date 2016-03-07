{-# LANGUAGE RecordWildCards, FlexibleContexts #-}

module FM.Player ( 
  play
, pause
, resume
, stop
, setVolume
, increaseVolume, decreaseVolume
) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Concurrent (forkFinally, killThread, myThreadId, throwTo)
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar
import Control.Exception (asyncExceptionFromException, AsyncException (..))
import Control.Monad.STM (atomically)
import Data.List (isPrefixOf)

import System.IO
import System.Process (runInteractiveProcess, terminateProcess)

import qualified FM.Song as Song
import           FM.FMState

type FetchLyrics = Song.Song -> IO Song.Lyrics
type NotifyDone = Bool -> IO ()

play :: (MonadIO m, MonadState FMState m) => Song.Song -> FetchLyrics -> NotifyDone -> m ()
play song@Song.Song {..} fetchLyrics notify = do
  fm@FMState {..} <- get
  (inHandle, outHandle, _, processHandle) <- liftIO $ runInteractiveProcess "mpg123" ["-R"] Nothing Nothing
  let initHandle h = liftIO $ do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering
  initHandle inHandle
  initHandle outHandle
  exitLock <- liftIO $ atomically $ newTMVar ()
  let playerThread = do
        atomically $ putTMVar exitLock ()
        {- TODO: fetch lyrics asynchronously. -}
        lyrics <- fetchLyrics song
        atomically $ putTMVar currentLyrics lyrics
        hPutStrLn inHandle $ "V " ++ show currentVolume
        hPutStrLn inHandle $ "L " ++ url
        loop (outHandle, fm) True =<< hGetLine outHandle
  let cleanUp e = do
        PlayerContext {..} <- atomically $ readTMVar playerContext
        terminateProcess processHandle
        atomically $ do
          writeTVar playerState Stopped
          tryTakeTMVar totalLength
          tryTakeTMVar currentLocation
          tryTakeTMVar currentLyrics
          takeTMVar playerContext
        case e of
          Right _ -> notify True
          Left e -> case asyncExceptionFromException e of
                      Just ThreadKilled -> notify False
                      _ -> throwTo parentThreadId e
  parentThreadId <- liftIO myThreadId
  childThreadId <- liftIO $ forkFinally playerThread cleanUp
  void $ liftIO $ atomically $ do
    putTMVar playerContext PlayerContext {..}
    writeTVar playerState (Playing song)
    takeTMVar exitLock
    where
      loop :: (Handle, FMState) -> Bool -> String -> IO ()

      loop ctx@(h, FMState {..}) True out@('@':'F':_) = do
        let ws = words out
        let tracks = read (ws !! 2) :: Int
        let length = read (ws !! 4) :: Double
        atomically $ do
          putTMVar totalLength (tracks, length)
          putTMVar currentLocation (0, 0)
        loop ctx False =<< hGetLine h
      
      loop ctx@(h, FMState {..}) True _ = loop ctx True =<< hGetLine h
      
      loop ctx@(h, FMState {..}) False out@('@':'F':_) = do
        let ws = words out
        let curTrack = read (ws !! 1) :: Int
        let curLocation = read (ws !! 3) :: Double
        atomically $ swapTMVar currentLocation (curTrack, curLocation)
        loop ctx False =<< hGetLine h

      loop ctx@(h, _) False out 
        | "@P 0" `isPrefixOf` out = return ()
        | otherwise = loop ctx False =<< hGetLine h

-- | pause, resume and stop are idempotent.
pause :: (MonadIO m, MonadState FMState m) => m ()
pause = do
  FMState {..} <- get
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Playing s -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      atomically $ writeTVar playerState (Paused s)
      hPutStrLn inHandle "P"
    _ -> return ()

resume :: (MonadIO m, MonadState FMState m) => m ()
resume = do
  FMState {..} <- get
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Paused s -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      atomically $ writeTVar playerState (Playing s)
      hPutStrLn inHandle "P"
    _ -> return ()

stop :: (MonadIO m, MonadState FMState m) => m ()
stop = do
  FMState {..} <- get
  state <- liftIO $ atomically $ readTVar playerState
  case state of
    Stopped -> return ()
    _ -> liftIO $ do
      PlayerContext {..} <- atomically $ readTMVar playerContext
      killThread childThreadId

setVolume :: (MonadIO m, MonadState FMState m) => Int -> m ()
setVolume vol = do
  let vol' | vol < 0 = 0
           | vol > 100 = 100
           | otherwise = vol
  FMState {..} <- get
  h <- fmap inHandle <$> liftIO (atomically $ tryReadTMVar playerContext)
  maybe (return ()) (\h -> liftIO (hPutStrLn h $ "V " ++ show vol')) h
  modify $ \fm -> fm { currentVolume = vol' }

increaseVolume :: (MonadIO m, MonadState FMState m) => m ()
increaseVolume = setVolume . (\x -> x + 5) =<< gets currentVolume
 
decreaseVolume :: (MonadIO m, MonadState FMState m) => m ()
decreaseVolume = setVolume . (\x -> x - 5) =<< gets currentVolume
