{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module FM.CacheManager (
  Cache
, initCache
, cacheSong
, waitCaching
, initSession
, fetchCache
) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.Lock
import           Control.Monad.Reader
import           Control.Monad.STM (atomically)
import qualified Crypto.Hash as C
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:), (.=))
import           Data.Aeson.Extra
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import           System.Directory (doesFileExist, getDirectoryContents)
import           System.Process (runInteractiveProcess, waitForProcess)

import           FM.Session
import qualified FM.Song as Song

hashSongId :: String -> String
hashSongId = show . C.hashWith C.MD5 . BS8.pack

instance JSON.FromJSON Song.Song where
  parseJSON = onObject $ \v -> do
    let parseArtists = onArray $ \v -> mapM JSON.parseJSON (V.toList v)
    uid <- v .: "uid"
    title <- v .: "title"
    album <- v .: "album"
    artists <- parseArtists =<< v .: "artists"
    let url = [ ]
    return Song.Song {..}

instance JSON.ToJSON Song.Song where
  toJSON Song.Song {..} = JSON.object [ "uid" .= uid
                                      , "title" .= title
                                      , "album" .= album
                                      , "artists" .= JSON.Array (V.fromList (JSON.toJSON <$> artists))
                                      ]

data Cache = Cache {
  cachePath      :: FilePath
, songQueue      :: TQueue (String, FilePath)
, queueLock      :: Lock
, workerThreadId :: ThreadId
}

initCache :: (MonadIO m) => FilePath -> m Cache
initCache cachePath = do
  songQueue <- liftIO newTQueueIO
  queueLock <- liftIO newLockIO
  workerThreadId <- liftIO $ forkIO $ forever $ do
    (url, path) <- atomically $ do
      result <- peekTQueue songQueue
      state <- viewLock queueLock
      when (state == Released) (acquireLock queueLock)
      return result
    (_, _, _, h) <- runInteractiveProcess "aria2c" [ "--auto-file-renaming=false", "-d", cachePath, "-o", path, url ] Nothing Nothing
    waitForProcess h
    atomically $ do
      readTQueue songQueue
      isEmpty <- isEmptyTQueue songQueue
      when isEmpty (releaseLock queueLock)
  return Cache {..}

waitCaching :: (MonadIO m) => Cache -> m ()
waitCaching Cache {..} = liftIO $ atomically $ waitLock queueLock Released

cacheSong :: (MonadIO m, MonadReader Cache m) => Song.Song -> m ()
cacheSong song@Song.Song {..} = do
  Cache {..} <- ask
  let hashPath = show uid ++ "-" ++ hashSongId (show uid)
  liftIO $ do
    BL.writeFile (cachePath ++ "/" ++ hashPath ++ ".json") (JSON.encode song)
    atomically $ writeTQueue songQueue (url, hashPath ++ ".mp3")

data Session = Session {
  sessionCachePath :: FilePath
}

instance IsSession Session

initSession :: (MonadIO m) => Cache -> m SomeSession
initSession Cache {..} = return $ SomeSession (Session cachePath)

fetchCache :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchCache = do
  Session {..} <- ask
  files <- liftIO $ filterM (isValid sessionCachePath) =<< getDirectoryContents sessionCachePath
  songs <- forM files $ \path -> do
    let fullPath = sessionCachePath ++ "/" ++ path
    song <- liftIO $ JSON.decode <$> BL.readFile fullPath
    case song of
      Just song -> do
        let url = take (length fullPath - 4) fullPath ++ "mp3"
        return $ Just song { Song.url = url }
      Nothing -> return Nothing
  return $ map fromJust $ filter isJust songs
  where
    isValid dir path = do
      let fullPath = dir ++ "/" ++ path
      isFile <- doesFileExist fullPath
      let id = takeWhile (/= '-') path
      let hashPath = id ++ "-" ++ hashSongId id
      let validFile = path == (hashPath ++ ".json")
      mp3Exists <- doesFileExist (dir ++ "/" ++ hashPath ++ ".mp3")
      return $ isFile && validFile && mp3Exists
