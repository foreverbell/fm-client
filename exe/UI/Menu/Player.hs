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

import           Control.Concurrent (throwTo, myThreadId, forkFinally)
import           Control.Concurrent.Chan
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
           | NewSongArrival [Song.Song]

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
    NetEaseDailyRecommendation -> liftSession state NetEase.fetchRListAsFM
  if null new
     then error "unable to fetch new songs"
     else return new

{- TODO: star / unstar / trash should be executed asynchronously. -}
star :: State -> Song.Song -> IO Song.Song
star state song = do
  liftSession state (NetEase.star song)
  return song { Song.starred = True }

unstar :: State -> Song.Song -> IO Song.Song
unstar state song = do
  liftSession state (NetEase.unstar song)
  return song { Song.starred = False }

trash :: State -> Song.Song -> IO Song.Song
trash state song = do
  liftSession state (NetEase.trash song)
  return song { Song.starred = False }

fetchLyrics :: State -> Song.Song -> IO Song.Lyrics
fetchLyrics state song = liftSession state (NetEase.fetchLyrics song)

fetchMore :: State -> IO ()
fetchMore state@State {..} = do
  this <- myThreadId
  let finally e = case e of
        Left e -> throwTo this e
        Right _ -> return ()
  let get = do
        new <- fetch state
        writeChan eventChan (NewSongArrival new)
  void $ forkFinally get finally

vpSize :: Int
vpSize = 10

vpName :: UI.Name
vpName = "vp"

playerMenuDraw :: State -> [UI.Widget]
playerMenuDraw State {..} = [ui]
  where
    ui = UI.center player
    player = UI.hLimit 60 $ UI.vLimit 10 $ UI.viewport vpName UI.Both $ UI.vBox $ do
      let slices = toList $ S.take (vpSize * 2 + 2) $ S.drop (focusedIndex - vpSize) playSequence
      let format Song.Song {..} = title ++ " - " ++ intercalate " / " artists ++ " - " ++ album
      let from = max 0 (focusedIndex - vpSize)
      (song, index) <- zip slices [from .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkFocused
                 | otherwise = UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ format song)

playerMenuEvent :: State -> Event -> UI.EventM (UI.Next State)
playerMenuEvent state event = case event of
  NewSongArrival new -> UI.continue state { playSequence = playSequence state S.>< S.fromList new  }
  VtyEvent (UI.EvKey UI.KEsc []) -> liftIO exitSuccess
  _ -> UI.continue state

playerMenuApp :: UI.App State Event
playerMenuApp = UI.App { UI.appDraw = playerMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = playerMenuEvent
                       , UI.appAttrMap = const UI.attributeMap
                       , UI.appLiftVtyEvent = VtyEvent
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

playerMenu_cps :: MusicSource -> SomeSession -> IO ()
playerMenu_cps source session = do
  fm <- initialState
  chan <- newChan
  let state = State { source = source
                    , playSequence = S.empty
                    , currentIndex = 0
                    , focusedIndex = 0
                    , session = session
                    , fmState = fm
                    , eventChan = chan
                    }
  fetchMore state
  void $ UI.customMain (UI.mkVty def) chan playerMenuApp state

playerMenu :: MusicSource -> SomeSession -> Cont (IO ()) (IO ())
playerMenu source session = cont (const $ playerMenu_cps source session)
