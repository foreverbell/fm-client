module Main where

import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

import FM.FM
import FM.NetEase
import FM.Player

main = void $ do
  session <- initSession
  [username, password] <- take 2 . lines <$> liftIO (readFile "passport")
  runFM session $ do
    login username password
    fm <- fetchFM
    liftIO $ mapM print fm
    lyrics <- liftIO $ fetchLyricsIO session (fm !! 0)
    liftIO $ print lyrics
    play (fm !! 0) (fetchLyricsIO session)
    liftIO $ threadDelay 5000000
    pause
    liftIO $ threadDelay 1000000
    resume
    liftIO $ threadDelay 1000000
    stop
