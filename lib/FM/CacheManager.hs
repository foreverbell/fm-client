{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FM.CacheManager (
  MonadCache, runCache
, Cache
, initCache
, cacheSong
, deleteSong
, waitAllCacheTasks
, initSession
, fetchCache
, fetchLyrics
) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.STM.TQueue
import           Control.Concurrent.STM.Lock
import           Control.Exception (try, SomeException)
import           Control.Monad.Reader
import           Control.Monad.STM (atomically)
import qualified Crypto.Hash as C
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:), (.=))
import           Data.Aeson.Extra
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Default.Class (def)
import           Data.Maybe (fromJust, isJust)
import qualified Data.Vector as V
import           System.Directory (doesFileExist, getDirectoryContents, removeFile)
import           System.Exit (ExitCode (..))
import           System.Process (runInteractiveProcess, waitForProcess)

import qualified FM.NetEase as NetEase
import           FM.Session
import qualified FM.Song as Song

newtype MonadCache a = MonadCache (ReaderT Cache IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Cache)

runCache :: Cache -> MonadCache a -> IO a
runCache cache (MonadCache m) = runReaderT m cache

hashSongId :: String -> String
hashSongId uid = uid ++ "-" ++ hash
  where hash = show $ C.hashWith C.MD5 $ BS8.pack uid

data SongWithLyrics = SongWithLyrics Song.Song Song.Lyrics

instance JSON.FromJSON SongWithLyrics where
  parseJSON = onObject $ \v -> do
    let
      parseArray :: (JSON.FromJSON a) => JSON.Value -> JSON.Parser [a]
      parseArray = onArray $ \v -> mapM JSON.parseJSON (V.toList v)
    uid <- v .: "uid"
    title <- v .: "title"
    album <- v .: "album"
    artists <- parseArray =<< v .: "artists"
    let url = [ ]
    let parseLyrics = onObject $ \v -> do
          time <- parseArray =<< v .: "time"
          body <- parseArray =<< v .: "body"
          return $ Song.Lyrics $ zip time body
    lyrics <- parseLyrics =<< v .: "lyrics"
    return $ SongWithLyrics Song.Song {..} lyrics

instance JSON.ToJSON SongWithLyrics where
  toJSON (SongWithLyrics Song.Song {..} (Song.Lyrics lyrics)) = 
    JSON.object [ "uid" .= uid
                , "title" .= title
                , "album" .= album
                , "artists" .= array artists
                , "lyrics" .= JSON.object [ "time" .= array (map fst lyrics), "body" .= array (map snd lyrics) ]
                ]
    where array xs = JSON.Array (V.fromList (JSON.toJSON <$> xs))

data Cache = Cache {
  cachePath :: FilePath
, songQueue :: TQueue Song.Song
, queueLock :: Lock
}

initCache :: (MonadIO m) => FilePath -> m Cache
initCache cachePath = do
  songQueue <- liftIO newTQueueIO
  queueLock <- liftIO newLockIO
  netEaseSession <- NetEase.initSession True
  liftIO $ forkIO $ forever $ do
    song@Song.Song {..} <- atomically $ do
      result <- peekTQueue songQueue
      state <- viewLock queueLock
      when (state == Released) (acquireLock queueLock)
      return result
    let hashPath = hashSongId (show uid)
    (_, _, _, h) <- runInteractiveProcess "aria2c" [ "--auto-file-renaming=false", "-d", cachePath, "-o", hashPath ++ ".mp3", url ] Nothing Nothing
    exitCode <- waitForProcess h
    if exitCode == ExitSuccess
      then do
        lyrics <- runSession netEaseSession (NetEase.fetchLyrics song)
        BL.writeFile (cachePath ++ "/" ++ hashPath ++ ".json") (JSON.encode $ SongWithLyrics song lyrics)
      else do
        let path = cachePath ++ "/" ++ hashPath ++ ".mp3"
        void (try (removeFile path) :: IO (Either SomeException ()))
    atomically $ do
      readTQueue songQueue
      isEmpty <- isEmptyTQueue songQueue
      when isEmpty (releaseLock queueLock)
  return Cache {..}

waitAllCacheTasks :: (MonadIO m) => Cache -> m ()
waitAllCacheTasks Cache {..} = liftIO $ atomically $ waitLock queueLock Released

cacheSong :: (MonadIO m, MonadReader Cache m) => Song.Song -> m ()
cacheSong song = do
  Cache {..} <- ask
  liftIO $ atomically $ writeTQueue songQueue song

deleteSong :: (MonadIO m, MonadReader Cache m) => Song.Song -> m ()
deleteSong Song.Song {..} = do
  Cache {..} <- ask
  let path = cachePath ++ "/" ++ hashSongId (show uid)
  void $ forM [".mp3", ".json"] $ \suffix -> do
    let fullPath = path ++ suffix
    liftIO (try $ removeFile fullPath :: IO (Either SomeException ()))

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
      Just (SongWithLyrics song _) -> do
        let url = take (length fullPath - 4) fullPath ++ "mp3"
        return $ Just song { Song.url = url }
      Nothing -> return Nothing
  return $ map fromJust $ filter isJust songs
  where
    isValid dir path = do
      let fullPath = dir ++ "/" ++ path
      isFile <- doesFileExist fullPath
      let uid = takeWhile (/= '-') path
      let hashPath = hashSongId uid
      let validFile = path == (hashPath ++ ".json")
      mp3Exists <- doesFileExist (dir ++ "/" ++ hashPath ++ ".mp3")
      return $ isFile && validFile && mp3Exists

fetchLyrics :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Lyrics
fetchLyrics Song.Song {..} = do
  song <- liftIO $ JSON.decode <$> BL.readFile (take (length url - 3) url ++ "json")
  return $ case song of
    Just (SongWithLyrics _ lyrics) -> lyrics
    Nothing -> def
