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

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan (Chan, writeChan, newChan)
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (Cont, cont)
import           Control.Monad.STM (atomically)
import           Data.Default.Class
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Sequence as S
import           System.Exit (exitSuccess)

import           FM.FM
import qualified FM.Song as Song
import qualified FM.Player as Player
import qualified FM.NetEase as NetEase

data State = State {
  source        :: MusicSource
, playSequence  :: S.Seq Song.Song
, currentIndex  :: Int
, focusedIndex  :: Int
, session       :: SomeSession
, fmState       :: FMState
, eventChan     :: Chan Event
, pendingMasked :: Bool
}

data Event = VtyEvent UI.Event
           | UserEventFetchMore
           | UserEventPending
           | UserEventTimer

liftSession :: (MonadIO m, IsSession s) => State -> SessionOnly s a -> m a
liftSession State {..} m = liftIO $ runSessionOnly (fromSession session) m

liftState :: (MonadIO m) => State -> StateOnly a -> m (a, State)
liftState state@State {..} m = do
  (r, s) <- liftIO $ runStateOnly fmState m
  return (r, state { fmState = s })

liftState_ :: (MonadIO m) => State -> StateOnly a -> m State
liftState_ state m = snd <$> liftState state m

fetch :: (MonadIO m) => State -> m [Song.Song]
fetch state@State {..} = do
  new <- case source of
    NetEaseFM -> liftSession state NetEase.fetchFM
    NetEasePublicFM -> liftSession state NetEase.fetchFM
    NetEaseDailyRecommendation -> liftSession state NetEase.fetchRListAsFM
  if null new
     then error "unable to fetch new songs"
     else return new

fetchLyrics :: (MonadIO m) => State -> Song.Song -> m Song.Lyrics
fetchLyrics state song = liftSession state (NetEase.fetchLyrics song)

fetchMore :: (MonadIO m) => State -> m State
fetchMore state@State {..} = do
  new <- S.fromList <$> fetch state
  return state { playSequence = playSequence S.>< new }

star :: (MonadIO m) => State -> Song.Song -> m Song.Song
star state song = do
  liftIO $ forkIO $ liftSession state (NetEase.star song)
  return song { Song.starred = True }

unstar :: (MonadIO m) => State -> Song.Song -> m Song.Song
unstar state song = do
  liftIO $ forkIO $ liftSession state (NetEase.unstar song)
  return song { Song.starred = False }

trash :: (MonadIO m) => State -> Song.Song -> m Song.Song
trash state song = do
  liftIO $ forkIO $ liftSession state (NetEase.trash song)
  return song { Song.starred = False }

play :: (MonadIO m) => State -> m State
play state@State {..} = do
  let onComplete e = when e (writeChan eventChan UserEventPending)
  state@State {..} <- liftState_ state $ Player.play (playSequence `S.index` (currentIndex - 1)) (fetchLyrics state) onComplete
  return state { focusedIndex = currentIndex, pendingMasked = False }

pause :: (MonadIO m) => State -> m State
pause state = do
  state <- liftState_ state Player.pause
  return state { pendingMasked = True }

resume :: (MonadIO m) => State -> m State
resume state = do
  state <- liftState_ state Player.resume
  return state { pendingMasked = False }

stop :: (MonadIO m) => State -> m State
stop state = do
  state <- liftState_ state Player.stop
  return state { pendingMasked = True }

vpSize :: Int
vpSize = 15

playerMenuDraw :: State -> [UI.Widget]
playerMenuDraw State {..} = [ui]
  where
    ui = UI.center player
    player = UI.vLimit vpSize $ UI.viewport "vp" UI.Vertical $ UI.vBox $ do
      let format Song.Song {..} = title ++ " - " ++ intercalate " / " artists ++ " - " ++ album
      (song, index) <- zip (toList playSequence) [1 .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkFocused
                 | otherwise = UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ format song)

playerMenuEvent :: State -> Event -> UI.EventM (UI.Next State)
playerMenuEvent state@State {..} event = case event of
  UserEventFetchMore -> UI.continue =<< fetchMore state

  UserEventPending -> do
    player <- liftIO $ atomically $ readTVar (playerState fmState)
    if pendingMasked && isPlaying player
      then UI.continue state
      else do
        state@State {..} <- if currentIndex == S.length playSequence
                               then fetchMore state
                               else return state
        UI.continue =<< play state { currentIndex = currentIndex + 1 }

  VtyEvent (UI.EvKey UI.KEsc []) -> do
    player <- liftIO $ atomically $ readTVar (playerState fmState)
    if isStopped player
       then liftIO exitSuccess
       else UI.continue =<< stop state

  VtyEvent (UI.EvKey (UI.KChar ' ') []) -> do
    player <- liftIO $ atomically $ readTVar (playerState fmState)
    UI.continue =<< case player of
      Playing _ -> do
        if currentIndex == focusedIndex
           then pause state
           else do
             state@State {..} <- stop state
             play state { currentIndex = focusedIndex }
      Paused _ -> do
        if currentIndex == focusedIndex
           then resume state
           else do
             state@State {..} <- stop state
             play state { currentIndex = focusedIndex }
      Stopped -> play state { currentIndex = focusedIndex }

  VtyEvent (UI.EvKey UI.KPageUp []) -> UI.continue state { focusedIndex = max 1 (focusedIndex - vpSize) }

  VtyEvent (UI.EvKey UI.KPageDown []) -> UI.continue state { focusedIndex = min (S.length playSequence) (focusedIndex + vpSize) }

  VtyEvent (UI.EvKey UI.KUp []) -> UI.continue state { focusedIndex = max 1 (focusedIndex - 1) } 

  VtyEvent (UI.EvKey UI.KDown []) -> do
    state@State {..} <- if focusedIndex == S.length playSequence
                          then liftIO (fetchMore state)
                          else return state
    UI.continue state { focusedIndex = min (S.length playSequence) (focusedIndex + 1) } 

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
                    , pendingMasked = True
                    }
  writeChan chan UserEventFetchMore
  void $ UI.customMain (UI.mkVty def) chan playerMenuApp state

playerMenu :: MusicSource -> SomeSession -> Cont (IO ()) (IO ())
playerMenu source session = cont (const $ playerMenuCPS source session)
