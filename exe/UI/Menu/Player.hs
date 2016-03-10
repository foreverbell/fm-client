{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module UI.Menu.Player (
  playerMenu
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           UI.Types

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan (writeChan, newChan)
import           Control.Concurrent.STM.TVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (Cont, cont)
import           Control.Monad.STM (atomically)
import           Data.Char (chr)
import           Data.Default.Class
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Sequence as S
import           Text.Printf (printf)
import           System.Exit (exitSuccess)

import           FM.FM
import qualified FM.Song as Song
import qualified FM.Player as Player
import qualified FM.NetEase as NetEase

data State = State {
  session       :: SomeSession
, player        :: Player
, source        :: MusicSource
, playSequence  :: S.Seq Song.Song
, onStopped     :: Bool
, currentIndex  :: Int
, focusedIndex  :: Int
, volume        :: Int
, progress      :: (Double, Double)
, lyrics        :: String 
, postEvent     :: Event -> IO ()
, pendingMasked :: Bool
}

data Event = VtyEvent UI.Event
           | UserEventFetchMore
           | UserEventPending
           | UserEventUpdateProgress (Double, Double)
           | UserEventUpdateLyrics String

liftSession :: (MonadIO m, IsSession s) => State -> SessionOnly s a -> m a
liftSession State {..} m = liftIO $ runSessionOnly (fromSession session) m

liftPlayer :: (MonadIO m) => State -> PlayerOnly a -> m a
liftPlayer State {..} m = liftIO $ runPlayerOnly player m

fetch :: (MonadIO m) => State -> m [Song.Song]
fetch state@State {..} = do
  new <- case source of
    NetEaseFM -> liftSession state NetEase.fetchFM
    NetEasePublicFM -> liftSession state NetEase.fetchFM
    NetEaseDailyRecommendation -> liftSession state NetEase.fetchRecommend
  if null new
     then error "unable to fetch new songs"
     else return new

fetchLyrics :: (MonadIO m) => State -> Song.Song -> m Song.Lyrics
fetchLyrics state song = liftSession state (NetEase.fetchLyrics song)

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

fetchMore :: (MonadIO m) => State -> m State
fetchMore state@State {..} = do
  new <- S.fromList <$> fetch state
  return state { playSequence = playSequence S.>< new }

play :: (MonadIO m) => State -> m State
play state@State {..} = do
  let onTerminate e = when e (postEvent UserEventPending)
  let onProgress p = postEvent (UserEventUpdateProgress p)
  let onLyrics l = postEvent (UserEventUpdateLyrics l)
  liftPlayer state $ Player.play (playSequence `S.index` (currentIndex - 1)) volume (fetchLyrics state) onTerminate onProgress onLyrics
  return state { focusedIndex = currentIndex
               , onStopped = False
               , progress = (0, 0)
               , lyrics = [] 
               , pendingMasked = False }

pause :: (MonadIO m) => State -> m State
pause state = do
  liftPlayer state Player.pause
  return state { pendingMasked = True }

resume :: (MonadIO m) => State -> m State
resume state = do
  liftPlayer state Player.resume
  return state { pendingMasked = False }

stop :: (MonadIO m) => State -> m State
stop state = do
  liftPlayer state Player.stop
  return state { onStopped = True
               , progress = (0, 0)
               , lyrics = []
               , pendingMasked = True }

increaseVolume :: (MonadIO m) => State -> Int -> m State
increaseVolume state@State {..} d = do
  let vol = min 100 (volume + d)
  liftPlayer state (Player.setVolume vol)
  return state { volume = vol }

decreaseVolume :: (MonadIO m) => State -> Int -> m State
decreaseVolume state@State {..} d = do
  let vol = max 0 (volume - d)
  liftPlayer state (Player.setVolume vol)
  return state { volume = vol }

playerMenuDraw :: State -> [UI.Widget]
playerMenuDraw State {..} = [ui]
  where
    formatSong Song.Song {..} = printf "%s - %s - %s" title (intercalate " / " artists) album :: String

    formatTime time = printf "%02d:%02d" minute second :: String
      where (minute, second) = floor time `quotRem` 60 :: (Int, Int)

    ui = UI.vBox [UI.separator, banner, UI.separator, progressBar, UI.separator, lyricsBar, UI.separator, player]

    banner = UI.hCenter $ UI.hBox [UI.mkRed $ UI.str star, UI.mkYellow $ UI.str body]
      where 
        (body, star) | onStopped = ("[Stopped]", [])
                     | otherwise = (formatSong song, if Song.starred song then star else [])
          where 
            song = playSequence `S.index` (currentIndex - 1)
            star = [chr 9829] ++ "  "

    progressBar | onStopped = UI.separator
                | otherwise = UI.mkGreen $ UI.hCenter $ UI.str $ 
                    printf "[%s%s] (%s/%s)" (replicate blocks '>') (replicate (bar - blocks) ' ') (formatTime cur) (formatTime len)
      where 
        (len, cur) = progress
        ratio = if len == 0 then 0 else cur / len
        bar = 25 :: Int
        blocks = floor $ fromIntegral bar * ratio

    lyricsBar = UI.mkRed $ UI.hCenter $ UI.str $ if null lyrics then " " else lyrics

    player = UI.viewport "vp" UI.Vertical $ UI.hCenter $ UI.vBox $ do
      (song, index) <- zip (toList playSequence) [1 .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ formatSong song)

playerMenuEvent :: State -> Event -> UI.EventM (UI.Next State)
playerMenuEvent state@State {..} event = case event of
  UserEventFetchMore -> UI.continue =<< fetchMore state

  UserEventPending -> do
    pstate <- liftIO $ atomically $ readTVar (playerState player)
    if pendingMasked && isPlaying pstate
      then UI.continue state
      else do
        state@State {..} <- if currentIndex == S.length playSequence
                               then fetchMore state
                               else return state
        UI.continue =<< play state { currentIndex = currentIndex + 1 }

  UserEventUpdateProgress p -> UI.continue state { progress = p }

  UserEventUpdateLyrics l -> UI.continue state { lyrics = l }

  VtyEvent (UI.EvKey UI.KEsc []) -> do
    pstate <- liftIO $ atomically $ readTVar (playerState player)
    if isStopped pstate
       then liftIO exitSuccess
       else UI.continue =<< stop state

  VtyEvent (UI.EvKey (UI.KChar ' ') []) -> playerMenuEvent state (VtyEvent (UI.EvKey UI.KEnter []))
  VtyEvent (UI.EvKey UI.KEnter []) -> do
    pstate <- liftIO $ atomically $ readTVar (playerState player)
    UI.continue =<< case pstate of
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

  VtyEvent (UI.EvKey UI.KUp []) -> UI.continue state { focusedIndex = max 1 (focusedIndex - 1) } 

  VtyEvent (UI.EvKey UI.KDown []) -> do
    state@State {..} <- if focusedIndex == S.length playSequence
                          then liftIO (fetchMore state)
                          else return state
    UI.continue state { focusedIndex = min (S.length playSequence) (focusedIndex + 1) } 

  VtyEvent (UI.EvKey (UI.KChar '-') []) -> UI.continue =<< decreaseVolume state 10

  VtyEvent (UI.EvKey (UI.KChar '=') []) -> UI.continue =<< increaseVolume state 10

  VtyEvent (UI.EvKey (UI.KChar '_') []) -> UI.continue =<< decreaseVolume state 20

  VtyEvent (UI.EvKey (UI.KChar '+') []) -> UI.continue =<< increaseVolume state 20

  _ -> UI.continue state

playerMenuApp :: UI.App State Event
playerMenuApp = UI.App { UI.appDraw = playerMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = playerMenuEvent
                       , UI.appAttrMap = const UI.defaultAttributeMap
                       , UI.appLiftVtyEvent = VtyEvent
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

playerMenuCPS :: MusicSource -> SomeSession -> IO ()
playerMenuCPS source session = do
  player <- initPlayer
  chan <- newChan
  let postEvent = writeChan chan
  let state = State { session = session
                    , player = player
                    , source = source
                    , playSequence = S.empty
                    , onStopped = True
                    , currentIndex = 1
                    , focusedIndex = 1
                    , volume = 100
                    , progress = (0, 0)
                    , lyrics = []
                    , postEvent = postEvent
                    , pendingMasked = True
                    }
  postEvent UserEventFetchMore
  void $ UI.customMain (UI.mkVty def) chan playerMenuApp state

playerMenu :: MusicSource -> SomeSession -> Cont (IO ()) (IO ())
playerMenu source session = cont (const $ playerMenuCPS source session)
