module Main where

import Control.Monad.Cont

import           FM.FM
import qualified FM.NetEase as NetEase

import qualified UI.Black as Black
import qualified UI.Login as Login
import qualified UI.Menu as Menu
import qualified UI.Player as Player

import           SessionManager
import           Types

main :: IO ()
main = flip runContT id $ do
  manager <- liftIO newSessionManager
  source <- Menu.menuSelection [ NetEaseFM, NetEasePublicFM, NetEaseDailyRecommendation, NetEasePlayLists ] "Select Source"
  session <- Login.login "NetEase Login" source manager
  case source of
    NetEasePlayLists -> do
      playLists <- liftIO $ Black.black (runSessionOnly session NetEase.fetchPlayLists) return
      source <- Menu.menuSelection [ NetEasePlayList id title | (id, title) <- playLists ] "Select Play List"
      Player.musicPlayer source session
    _ -> Player.musicPlayer source session
