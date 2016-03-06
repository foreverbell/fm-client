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
import Control.Concurrent.MVar
import Control.Exception (asyncExceptionFromException, AsyncException (..))
import Data.IORef
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
  exitLock <- liftIO $ newMVar ()
  let playerThread = do
        putMVar exitLock ()
        {- TODO: fetch lyrics asynchronously. -}
        putMVar currentLyrics =<< fetchLyrics song
        hPutStrLn inHandle $ "V " ++ show currentVolume
        hPutStrLn inHandle $ "L " ++ mp3URL
        loop (outHandle, fm) True =<< hGetLine outHandle
  let cleanUp = \e -> do
        PlayerContext {..} <- readMVar playerContext
        terminateProcess processHandle
        writeIORef playerState Stop
        tryTakeMVar totalLength
        tryTakeMVar currentLocation
        tryTakeMVar currentLyrics
        takeMVar playerContext
        case e of
          Right _ -> notify True
          Left e -> case asyncExceptionFromException e of
                      Just ThreadKilled -> notify False
                      _ -> throwTo parentThreadId e
  parentThreadId <- liftIO myThreadId
  childThreadId <- liftIO $ forkFinally playerThread cleanUp
  liftIO $ putMVar playerContext PlayerContext {..}
  liftIO $ writeIORef playerState (Playing song)
  liftIO $ void $ takeMVar exitLock
    where
      loop :: (Handle, FMState) -> Bool -> String -> IO ()

      loop ctx@(h, FMState {..}) True out@('@':'F':_) = do
        let ws = words out
        let tracks = read (ws !! 2) :: Int
        let length = read (ws !! 4) :: Double
        putMVar totalLength (tracks, length)
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
  state <- liftIO $ readIORef playerState
  case state of
    Playing s -> do
      PlayerContext {..} <- liftIO $ readMVar playerContext
      liftIO $ hPutStrLn inHandle "P"
      liftIO $ writeIORef playerState (Paused s)
    _ -> return ()

resume :: (MonadIO m, MonadState FMState m) => m ()
resume = do
  FMState {..} <- get
  state <- liftIO $ readIORef playerState
  case state of
    Paused s -> do
      PlayerContext {..} <- liftIO $ readMVar playerContext
      liftIO $ hPutStrLn inHandle "P"
      liftIO $ writeIORef playerState (Playing s)
    _ -> return ()

stop :: (MonadIO m, MonadState FMState m) => m ()
stop = do
  FMState {..} <- get
  state <- liftIO $ readIORef playerState
  case state of
    Stop -> return ()
    _ -> do
      PlayerContext {..} <- liftIO $ readMVar playerContext
      liftIO $ killThread childThreadId

setVolume :: (MonadIO m, MonadState FMState m) => Int -> m ()
setVolume vol = do
  let vol' | vol < 0 = 0
           | vol > 100 = 100
           | otherwise = vol
  FMState {..} <- get
  h <- fmap inHandle <$> liftIO (tryReadMVar playerContext)
  maybe (return ()) (\h -> liftIO (hPutStrLn h $ "V " ++ show vol')) h
  modify $ \fm -> fm { currentVolume = vol' }

increaseVolume :: (MonadIO m, MonadState FMState m) => m ()
increaseVolume = setVolume . (\x -> x + 5) =<< gets currentVolume
 
decreaseVolume :: (MonadIO m, MonadState FMState m) => m ()
decreaseVolume = setVolume . (\x -> x - 5) =<< gets currentVolume
