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

fetchLyricsIO session song = runSessionOnly session (fetchLyrics song)

test :: IO ()
test = void $ do
  session <- initSession True
  state <- initialState
  [username, password] <- take 2 . lines <$> liftIO (readFile "passport")
  runBoth session state $ do
    login username password
    fm <- fetchFM
    liftIO $ mapM print fm
    lyrics <- liftIO $ fetchLyricsIO session (fm !! 0)
    liftIO $ print lyrics
    signal <- liftIO $ newEmptyMVar
    play (fm !! 0) (fetchLyricsIO session) $ \b -> do
      putStrLn $ if b then "normally exit" else "user interrupt"
      putMVar signal ()
    delay 5
    pause
    delay 1
    resume
    liftIO $ takeMVar signal

{- FIXME: Use Cont(CPS) is a workaround, since brick doesn't support stacking windows well. -}
main :: IO ()
main = flip runCont id $ do
  source <- Menu.sourceMenu
  passport <- Login.login source
  Menu.playerMenu source passport
