module Main where

import Control.Monad.Cont

import           FM.FM
import qualified FM.NetEase as NetEase

import qualified UI.Black as Black
import qualified UI.Login as Login
import qualified UI.Menu.Source as Menu
import qualified UI.Menu.Player as Menu

import           SessionManager
import           Types

main :: IO ()
main = flip runContT id $ do
  manager <- liftIO newSessionManager
  source <- Menu.sourceMenu [ NetEaseFM, NetEasePublicFM, NetEaseDailyRecommendation, NetEasePlayLists ]
  session <- Login.login "NetEase Login" source manager
  case source of
    NetEasePlayLists -> do
      playLists <- liftIO $ Black.black (runSessionOnly session NetEase.fetchPlayLists) return
      source <- Menu.sourceMenu [ NetEasePlayList id title | (id, title) <- playLists ]
      Menu.playerMenu source session
    _ -> Menu.playerMenu source session
