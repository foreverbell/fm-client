module Main where

import Control.Monad.Cont
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

import FM.FM
import FM.NetEase
import FM.Player

import qualified UI.Login as Login
import qualified UI.Menu.Source as Menu
import qualified UI.Menu.Player as Menu

delay :: MonadIO m => Int -> m ()
delay seconds = liftIO $ threadDelay (seconds * 1000000)

test :: IO ()
test = void $ do
  session <- initSession True
  player <- initPlayer
  [username, password] <- take 2 . lines <$> liftIO (readFile "passport")
  fm <- runSessionOnly session $ do
    login username password
    fetchFM
  mapM print fm
  lyrics <- runSessionOnly session $ fetchLyrics (fm !! 0)
  print lyrics
  signal <- newEmptyMVar
  let onTerminate b = do
        putStrLn $ if b then "normally exit" else "user interrupt"
        putMVar signal ()
  runPlayerOnly player $ do
    play (fm !! 0) 100 (runSessionOnly session . fetchLyrics) onTerminate print putStrLn
    liftIO $ delay 5
    pause
    liftIO $ delay 1
    resume
  takeMVar signal

{- FIXME: Use Cont(CPS) is a workaround, since brick doesn't support stacking windows well. -}
main :: IO ()
main = flip runCont id $ do
  source <- Menu.sourceMenu
  passport <- Login.login source
  Menu.playerMenu source passport
