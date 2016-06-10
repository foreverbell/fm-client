{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FM.Player (
  MonadPlayer, runPlayer
, PlayerState (..)
, isStopped
, Player (..)
, initPlayer
, play
, pause
, resume
, stop
, updateVolume
) where

import           Control.Concurrent (ThreadId, forkFinally, killThread, myThreadId, throwTo)
import           Control.Concurrent.Async (async, cancel, poll)
import           Control.Concurrent.STM.Lock
import           Control.Concurrent.STM.TMVar
import           Control.Concurrent.STM.TVar
import           Control.Exception (asyncExceptionFromException, AsyncException (..))
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.STM (atomically)
import           Data.Maybe (isJust, fromJust)
import           System.IO
import           System.Process (ProcessHandle, runInteractiveProcess, waitForProcess, terminateProcess)

import qualified FM.Song as Song

newtype MonadPlayer a = MonadPlayer (ReaderT Player IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Player)

runPlayer :: Player -> MonadPlayer a -> IO a
runPlayer player (MonadPlayer m) = runReaderT m player

data PlayerState = Playing Song.Song
                 | Paused Song.Song
                 | Stopped

isStopped :: PlayerState -> Bool
isStopped Stopped = True
isStopped _ = False

data PlayerContext = PlayerContext {
  inHandle       :: Handle
, outHandle      :: Handle
, errHandle      :: Handle
, processHandle  :: ProcessHandle
, parentThreadId :: ThreadId
, childThreadId  :: ThreadId
}

data Player = Player {
  mpg123Context :: TMVar PlayerContext
, playerState   :: TVar PlayerState
, playerVolume  :: Int
, playerMuted   :: Bool
, playerLock    :: Lock
}

initPlayer :: (MonadIO m) => m Player
initPlayer = liftIO $ do
  mpg123Context <- newEmptyTMVarIO
  playerState <- newTVarIO Stopped
  let playerVolume = 100
  let playerMuted = False
  playerLock <- newLockIO
  return Player {..}

type FetchUrl = Song.Song -> IO (Maybe String)
type FetchLyrics = Song.Song -> IO Song.Lyrics
type Notify a = a -> IO ()

play :: (MonadIO m, MonadReader Player m) => Song.Song -> FetchUrl -> FetchLyrics -> Notify Bool -> Notify (Double, Double) -> Notify String -> m ()
play song@Song.Song {..} fetchUrl fetchLyrics onTerminate onProgress onLyrics = do
  Player {..} <- ask
  (inHandle, outHandle, errHandle, processHandle) <- liftIO $ runInteractiveProcess "mpg123" ["-R"] Nothing Nothing
  let initHandle h = liftIO $ do
        hSetBinaryMode h False
        hSetBuffering h LineBuffering
  initHandle inHandle
  initHandle outHandle
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
      continue $ loop False (len, 0, Nothing)

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
      continue $ loop False (len, cur', lyrics')

    loop b ctx ('@':_) = continue $ loop b ctx

    loop _ _ _ = return () -- encounter mpg123 internal error.

    continue :: (String -> IO ()) -> IO ()
    continue cont = do
      isEOF <- hIsEOF outHandle
      unless isEOF $ cont =<< hGetLine outHandle

    playerThread = do
      atomically $ waitLock playerLock Acquired
      url <- fetchUrl song
      when (isJust url) $ do
        hPutStrLn inHandle $ "V " ++ show (if playerMuted then 0 else playerVolume)
        hPutStrLn inHandle $ "L " ++ fromJust url
        loop True (0, 0, Nothing) =<< hGetLine outHandle

    cleanUp e = do
      PlayerContext {..} <- atomically $ do
        writeTVar playerState Stopped
        takeTMVar mpg123Context
      cancel lyricsAsync
      forM_ [inHandle, outHandle, errHandle] $ \h -> hClose h
      terminateProcess processHandle
      waitForProcess processHandle
      case e of
        Right _ -> onTerminate True
        Left e -> case asyncExceptionFromException e of
                    Just ThreadKilled -> onTerminate False
                    _ -> throwTo parentThreadId e
      atomically $ releaseLock playerLock

  parentThreadId <- liftIO myThreadId
  childThreadId <- liftIO $ forkFinally playerThread cleanUp
  void $ liftIO $ atomically $ do
    putTMVar mpg123Context PlayerContext {..}
    writeTVar playerState (Playing song)
    acquireLock playerLock

-- | pause, resume and stop are idempotent.
pause :: (MonadIO m, MonadReader Player m) => m ()
pause = do
  Player {..} <- ask
  inHandle <- liftIO $ atomically $ do
    state <- readTVar playerState
    case state of
      Playing s -> do
        PlayerContext {..} <- readTMVar mpg123Context
        writeTVar playerState (Paused s)
        return $ Just inHandle
      _ -> return Nothing
  when (isJust inHandle) $ liftIO $ hPutStrLn (fromJust inHandle) "P"

resume :: (MonadIO m, MonadReader Player m) => m ()
resume = do
  Player {..} <- ask
  inHandle <- liftIO $ atomically $ do
    state <- readTVar playerState
    case state of
      Paused s -> do
        PlayerContext {..} <- readTMVar mpg123Context
        writeTVar playerState (Playing s)
        return $ Just inHandle
      _ -> return Nothing
  when (isJust inHandle) $ liftIO $ hPutStrLn (fromJust inHandle) "P"

stop :: (MonadIO m, MonadReader Player m) => m ()
stop = do
  Player {..} <- ask
  childThreadId <- liftIO $ atomically $ do
    state <- readTVar playerState
    case state of
      Stopped -> return Nothing
      _ -> do
        PlayerContext {..} <- readTMVar mpg123Context
        return $ Just childThreadId
  when (isJust childThreadId) $ liftIO $ killThread (fromJust childThreadId)
  liftIO $ atomically $ waitLock playerLock Released

updateVolume :: (MonadIO m, MonadReader Player m) => m ()
updateVolume = do
  Player {..} <- ask
  h <- fmap inHandle <$> liftIO (atomically $ tryReadTMVar mpg123Context)
  let v = if playerMuted then 0 else playerVolume
  liftIO $ maybe (return ()) (\h -> hPutStrLn h $ "V " ++ show v) h
