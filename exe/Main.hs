module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Cont

import           FM.FM
import qualified FM.NetEase as NetEase

import qualified UI.Black as Black
import qualified UI.Login as Login
import qualified UI.Menu as Menu
import qualified UI.Player as Player

import           SessionManager
import           Types

main :: IO ()
main = evalContT $ do
  manager <- liftIO newSessionManager
  source <- Menu.menuSelection [ NetEaseFM, NetEasePublicFM, NetEaseDailyRecommendation, NetEasePlayLists ] Nothing "播放源"
  session <- Login.login "登陆" source manager
  case source of
    NetEasePlayLists -> do
      playLists <- liftIO $ Black.black (runSessionOnly session NetEase.fetchPlayLists) return
      source <- Menu.menuSelection [ NetEasePlayList id title | (id, title) <- playLists ] Nothing (show1 NetEasePlayLists)
      Player.musicPlayer source session
    _ -> Player.musicPlayer source session
