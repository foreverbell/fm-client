{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Control.Concurrent (forkFinally, killThread)
import Control.Concurrent.MVar
import Data.IORef
import Data.List (isPrefixOf)

import System.IO
import System.Process (runInteractiveProcess, terminateProcess)

import qualified FM.Song as Song
import           FM.FM

play :: (MonadIO m, MonadState FMState m) => Song.Song -> (Song.Song -> IO Song.Lyrics) -> m ()
play song@Song.Song {..} fetchLyrics = do
  fm@FMState {..} <- get
  (inHandle, outHandle, _, processId) <- liftIO $ runInteractiveProcess "mpg123" ["-R"] Nothing Nothing
  let initHandle h = liftIO $ do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering
  initHandle inHandle
  initHandle outHandle
  exitLock <- liftIO $ newMVar ()
  let playerThread = do
        putMVar exitLock ()
        {- TODO: fetch lyrics asynchronously. -}
        putMVar currentLyrics =<< fetchLyrics song
        hPutStrLn inHandle $ "V " ++ show currentVolume
        hPutStrLn inHandle $ "L " ++ mp3URL
        loop (outHandle, fm) True =<< hGetLine outHandle
  let cleanUp = \_ -> void $ do
        PlayerHandle {..} <- readMVar playerHandle
        terminateProcess processId
        writeIORef playingState Stop
        tryTakeMVar playingLength
        tryTakeMVar currentLocation
        tryTakeMVar currentLyrics
        takeMVar playerHandle
  forkThreadId <- liftIO $ forkFinally playerThread cleanUp
  liftIO $ putMVar playerHandle PlayerHandle {..}
  liftIO $ writeIORef playingState (Playing song)
  liftIO $ void $ takeMVar exitLock
    where
      loop :: (Handle, FMState) -> Bool -> String -> IO ()

      loop ctx@(h, FMState {..}) True out@('@':'F':_) = do
        let ws = words out
        let tracks = read (ws !! 2) :: Int
        let length = read (ws !! 4) :: Double
        putMVar playingLength (tracks, length)
        putMVar currentLocation (0, 0)
        loop ctx False =<< hGetLine h
      
      loop ctx@(h, FMState {..}) True _ = loop ctx True =<< hGetLine h
      
      loop ctx@(h, FMState {..}) False out@('@':'F':_) = do
        let ws = words out
        let curTrack = read (ws !! 1) :: Int
        let curLocation = read (ws !! 3) :: Double
        modifyMVar_ currentLocation (const $ return (curTrack, curLocation))
        loop ctx False =<< hGetLine h

      loop ctx@(h, _) False out 
        | "@P 0" `isPrefixOf` out = return ()
        | otherwise = loop ctx False =<< hGetLine h

pause :: (MonadIO m, MonadState FMState m) => m ()
pause = do
  FMState {..} <- get
  state <- liftIO $ readIORef playingState
  case state of
    Playing s -> do
      PlayerHandle {..} <- liftIO $ readMVar playerHandle
      liftIO $ hPutStrLn inHandle "P"
      liftIO $ writeIORef playingState (Paused s)
    _ -> return ()

resume :: (MonadIO m, MonadState FMState m) => m ()
resume = do
  FMState {..} <- get
  state <- liftIO $ readIORef playingState
  case state of
    Paused s -> do
      PlayerHandle {..} <- liftIO $ readMVar playerHandle
      liftIO $ hPutStrLn inHandle "P"
      liftIO $ writeIORef playingState (Playing s)
    _ -> return ()

stop :: (MonadIO m, MonadState FMState m) => m ()
stop = do
  FMState {..} <- get
  state <- liftIO $ readIORef playingState
  case state of
    Stop -> return ()
    _ -> do
      PlayerHandle {..} <- liftIO $ readMVar playerHandle
      liftIO $ killThread forkThreadId

setVolume :: (MonadIO m, MonadState FMState m) => Int -> m ()
setVolume vol = do
  let vol' | vol < 0 = 0
           | vol > 100 = 100
           | otherwise = vol
  FMState {..} <- get
  h <- fmap inHandle <$> liftIO (tryReadMVar playerHandle)
  maybe (return ()) (\h -> liftIO (hPutStrLn h $ "V " ++ show vol')) h
  modify $ \fm -> fm { currentVolume = vol' }

increaseVolume :: (MonadIO m, MonadState FMState m) => m ()
increaseVolume = setVolume . (\x -> x + 5) =<< gets currentVolume
 
decreaseVolume :: (MonadIO m, MonadState FMState m) => m ()
decreaseVolume = setVolume . (\x -> x - 5) =<< gets currentVolume
