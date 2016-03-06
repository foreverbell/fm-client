{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module UI.Menu.Player (
  playerMenu
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Attributes as UI

import           UI.Types

import           Control.Concurrent (throwTo, myThreadId, forkIO, forkFinally)
import           Control.Concurrent.Chan (Chan, writeChan, newChan)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Cont (Cont, cont)
import           Data.Default.Class
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Sequence as S
import           System.Exit (exitSuccess)

import           FM.FM
import qualified FM.Song as Song
import qualified FM.NetEase as NetEase

data State = State {
  source       :: MusicSource
, playSequence :: S.Seq Song.Song
, currentIndex :: Int
, focusedIndex :: Int
, session      :: SomeSession
, fmState      :: FMState
, eventChan    :: Chan Event
}

data Event = VtyEvent UI.Event
           | HelloWorld

liftSession :: (IsSession s) => State -> SessionOnly s a -> IO a
liftSession State {..} m = runSessionOnly (fromSession session) m

liftState :: State -> StateOnly a -> IO (a, State)
liftState state@State {..} m = do
  (r, s) <- runStateOnly fmState m
  return (r, state { fmState = s })

liftState_ :: State -> StateOnly a -> IO State
liftState_ state m = snd <$> liftState state m

fetch :: State -> IO [Song.Song]
fetch state@State {..} = do
  new <- case source of
    NetEaseFM -> liftSession state NetEase.fetchFM
    NetEasePublicFM -> liftSession state NetEase.fetchFM
    NetEaseDailyRecommendation -> liftSession state NetEase.fetchRListAsFM
  if null new
     then error "unable to fetch new songs"
     else return new

fetchLyrics :: State -> Song.Song -> IO Song.Lyrics
fetchLyrics state song = liftSession state (NetEase.fetchLyrics song)

fetchMore :: State -> IO State
fetchMore state@State {..} = do
  new <- S.fromList <$> fetch state
  return state { playSequence = playSequence S.>< new }

star :: State -> Song.Song -> IO Song.Song
star state song = do
  forkIO $ liftSession state (NetEase.star song)
  return song { Song.starred = True }

unstar :: State -> Song.Song -> IO Song.Song
unstar state song = do
  forkIO $ liftSession state (NetEase.unstar song)
  return song { Song.starred = False }

trash :: State -> Song.Song -> IO Song.Song
trash state song = do
  forkIO $ liftSession state (NetEase.trash song)
  return song { Song.starred = False }

playerMenuDraw :: State -> [UI.Widget]
playerMenuDraw State {..} = [ui]
  where
    ui = UI.center player
    vpSize = 15
    player = UI.vLimit vpSize $ UI.viewport "vp" UI.Vertical $ UI.vBox $ do
      let format Song.Song {..} = title ++ " - " ++ intercalate " / " artists ++ " - " ++ album
      (song, index) <- zip (toList playSequence) [1 .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkFocused
                 | otherwise = UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ format song)

playerMenuEvent :: State -> Event -> UI.EventM (UI.Next State)
playerMenuEvent state@State {..} event = case event of
  HelloWorld -> UI.continue =<< liftIO (fetchMore state)
  VtyEvent (UI.EvKey UI.KEsc []) -> liftIO exitSuccess
  VtyEvent (UI.EvKey UI.KUp []) -> UI.continue state { focusedIndex = max 1 (focusedIndex - 1) } 
  VtyEvent (UI.EvKey UI.KDown []) -> do
    state' <- if focusedIndex == S.length playSequence
                 then liftIO (fetchMore state)
                 else return state
    UI.continue state' { focusedIndex = min (focusedIndex + 1) (S.length playSequence) } 
  _ -> UI.continue state

playerMenuApp :: UI.App State Event
playerMenuApp = UI.App { UI.appDraw = playerMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = playerMenuEvent
                       , UI.appAttrMap = const UI.attributeMap
                       , UI.appLiftVtyEvent = VtyEvent
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

playerMenuCPS :: MusicSource -> SomeSession -> IO ()
playerMenuCPS source session = do
  fm <- initialState
  chan <- newChan
  let state = State { source = source
                    , playSequence = S.empty
                    , currentIndex = 1
                    , focusedIndex = 1
                    , session = session
                    , fmState = fm
                    , eventChan = chan
                    }
  writeChan chan HelloWorld
  void $ UI.customMain (UI.mkVty def) chan playerMenuApp state

playerMenu :: MusicSource -> SomeSession -> Cont (IO ()) (IO ())
playerMenu source session = cont (const $ playerMenuCPS source session)
