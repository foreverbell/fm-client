module Main where

import Control.Monad.IO.Class
import Control.Monad (void)
import Control.Concurrent (threadDelay)

import FM.FM
import FM.NetEase
import FM.Player

import qualified UI.Login as UI
import qualified UI.Menu as UI

delay :: MonadIO m => Int -> m ()
delay seconds = liftIO $ threadDelay (seconds * 1000000)

test :: IO ()
test = void $ do
  session <- initSession True
  [username, password] <- take 2 . lines <$> liftIO (readFile "passport")
  runFM session Nothing $ do
    login username password
    fm <- fetchFM
    liftIO $ mapM print fm
    lyrics <- liftIO $ fetchLyricsIO session (fm !! 0)
    liftIO $ print lyrics
    play (fm !! 0) (fetchLyricsIO session)
    delay 5
    pause
    delay 1
    resume
    delay 1
    stop

main = do
  UI.login
  UI.playerMenu =<< UI.sourceMenu
  test
