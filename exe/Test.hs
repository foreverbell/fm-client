module Test where

import Control.Monad.Cont
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar

import FM.FM
import FM.NetEase

delay :: MonadIO m => Int -> m ()
delay seconds = liftIO $ threadDelay (seconds * 1000000)

test :: IO ()
test = void $ do
  session <- initSession True
  player <- initPlayer
  [username, password] <- take 2 . lines <$> liftIO (readFile "passport")
  fm <- runSession session $ do
    login username (encryptPassword password)
    fetchFM
  mapM print fm
  lyrics <- runSession session $ fetchLyrics (fm !! 0)
  print lyrics

main :: IO ()
main = test
