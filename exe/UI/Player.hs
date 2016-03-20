{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module UI.Player (
  musicPlayer
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan (writeChan, newChan)
import           Control.Concurrent.STM.TVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (ContT (..))
import           Control.Monad.STM (atomically)
import           Data.Char (chr)
import           Data.Default.Class
import           Data.Foldable (toList)
import           Data.List (intercalate)
import qualified Data.Sequence as S
import           Text.Printf (printf)
import           System.Random (randomRIO)

import           FM.FM
import qualified FM.Song as Song
import qualified FM.Player as Player
import qualified FM.NetEase as NetEase

import           UI.Menu
import           Types

data State = State {
  session       :: SomeSession
, player        :: Player
, source        :: MusicSource
, playMode      :: PlayMode
, playSequence  :: S.Seq Song.Song
, onStopped     :: Bool
, currentIndex  :: Int
, focusedIndex  :: Int
, volume        :: Int
, progress      :: (Double, Double)
, currentLyrics :: String 
, postEvent     :: Event -> IO ()
, pendingMasked :: Bool
}

data Event = VtyEvent UI.Event
           | UserEventFetchMore
           | UserEventPending
           | UserEventUpdateProgress (Double, Double)
           | UserEventUpdateLyrics String

liftSession :: (MonadIO m, IsSession s) => State -> SessionOnly s a -> m a
liftSession State {..} m = liftIO $ runSessionOnly session m

liftPlayer :: (MonadIO m) => State -> PlayerOnly a -> m a
liftPlayer State {..} m = liftIO $ runPlayerOnly player m

fetch :: (MonadIO m) => State -> m [Song.Song]
fetch state@State {..} = case source of
  NetEaseFM -> liftSession state NetEase.fetchFM
  NetEasePublicFM -> liftSession state NetEase.fetchFM
  NetEaseDailyRecommendation -> liftSession state NetEase.fetchRecommend
  NetEasePlayLists -> undefined
  NetEasePlayList id _ -> liftSession state (NetEase.fetchPlayList id)

-- TODO: check musicSource in fetchLyrics / star / unstar / trash
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
play state@State {..}
  | currentIndex == 0 = return state
  | otherwise = do
      let onTerminate e = when e (postEvent UserEventPending)
      let onProgress p = postEvent (UserEventUpdateProgress p)
      let onLyrics l = postEvent (UserEventUpdateLyrics l)
      liftPlayer state $ Player.play (playSequence `S.index` (currentIndex - 1)) volume (fetchLyrics state) onTerminate onProgress onLyrics
      return state { focusedIndex = currentIndex
                   , onStopped = False
                   , progress = (0, 0)
                   , currentLyrics = [] 
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
               , currentLyrics = []
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

musicPlayerDraw :: State -> [UI.Widget]
musicPlayerDraw State {..} = [ui]
  where
    formatSong Song.Song {..} = printf "%s - %s - %s" title (intercalate " / " artists) album :: String

    formatTime time = printf "%02d:%02d" minute second :: String
      where (minute, second) = floor time `quotRem` 60 :: (Int, Int)
    
    makeProgressBar c total occupied = replicate occupied c ++ replicate (total - occupied) ' '

    ui = UI.vBox [UI.separator, title , UI.separator, bar1, bar2, UI.separator, lyrics, UI.separator, player]

    title = UI.hCenter $ UI.hBox [UI.mkRed $ UI.str star, UI.mkYellow $ UI.str body]
      where 
        (body, star) | onStopped = ("[Stopped]", [])
                     | otherwise = (formatSong song, if Song.starred song then star else [])
          where 
            song = playSequence `S.index` (currentIndex - 1)
            star = chr 9829 : "  "

    bar1 | onStopped = UI.separator
         | otherwise = UI.mkGreen $ UI.hCenter $ UI.str $ printf "[%s] (%s/%s)" (makeProgressBar '>' total occupied) (formatTime cur) (formatTime len)
      where 
        (len, cur) = progress
        ratio = if len == 0 then 0 else cur / len
        total = 35 :: Int
        occupied = ceiling $ fromIntegral total * ratio

    bar2 = UI.mkGreen $ UI.hCenter $ UI.str $ printf "[Mode: %s] [Vol: %s]" (show1 playMode) (makeProgressBar '|' total occupied)
      where
        total = 10 :: Int
        occupied = total * volume `div` 100

    lyrics = UI.mkRed $ UI.hCenter $ UI.str $ if null currentLyrics then " " else currentLyrics

    player = UI.viewport "vp" UI.Vertical $ UI.hCenter $ UI.vBox $ do
      (song, index) <- zip (toList playSequence) [1 .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ formatSong song)

musicPlayerEvent :: State -> Event -> UI.EventM (UI.Next State)
musicPlayerEvent state@State {..} event = case event of
  UserEventFetchMore -> UI.continue =<< fetchMore state

  UserEventPending -> do
    pState <- liftIO $ atomically $ readTVar (playerState player)
    if pendingMasked && isPlaying pState
      then UI.continue state
      else do
        let needMore = currentIndex == S.length playSequence && playMode == Stream
        state@State {..} <- if needMore then fetchMore state else return state
        nextIndex <- case playMode of
          Stream -> return $ min (S.length playSequence) (currentIndex + 1)
          LoopOne -> return currentIndex
          LoopAll -> return $ if currentIndex + 1 >= S.length playSequence then 1 else currentIndex + 1
          Shuffle -> liftIO $ randomRIO (1, S.length playSequence)
        UI.continue =<< play state { currentIndex = nextIndex }

  UserEventUpdateProgress p -> UI.continue state { progress = p }

  UserEventUpdateLyrics l -> UI.continue state { currentLyrics = l }

  VtyEvent (UI.EvKey UI.KEsc []) -> do
    pState <- liftIO $ atomically $ readTVar (playerState player)
    if isStopped pState
       then UI.halt state
       else UI.continue =<< stop state

  VtyEvent (UI.EvKey (UI.KChar ' ') []) -> musicPlayerEvent state (VtyEvent $ UI.EvKey UI.KEnter [])
  VtyEvent (UI.EvKey UI.KEnter []) -> do
    pState <- liftIO $ atomically $ readTVar (playerState player)
    UI.continue =<< case pState of
      Playing _ -> if currentIndex == focusedIndex
        then pause state
        else do
          state@State {..} <- stop state
          play state { currentIndex = focusedIndex }
      Paused _ -> if currentIndex == focusedIndex
        then resume state
        else do
          state@State {..} <- stop state
          play state { currentIndex = focusedIndex }
      Stopped -> play state { currentIndex = focusedIndex }

  VtyEvent (UI.EvKey UI.KUp []) -> UI.continue state { focusedIndex = max 0 (focusedIndex - 1) } 

  VtyEvent (UI.EvKey UI.KDown []) -> do
    let needMore = focusedIndex == S.length playSequence && playMode == Stream
    state@State {..} <- if needMore then liftIO (fetchMore state) else return state
    UI.continue state { focusedIndex = min (S.length playSequence) (focusedIndex + 1) } 

  VtyEvent (UI.EvKey (UI.KChar '-') []) -> UI.continue =<< decreaseVolume state 10

  VtyEvent (UI.EvKey (UI.KChar '=') []) -> UI.continue =<< increaseVolume state 10

  VtyEvent (UI.EvKey (UI.KChar '_') []) -> UI.continue =<< decreaseVolume state 20

  VtyEvent (UI.EvKey (UI.KChar '+') []) -> UI.continue =<< increaseVolume state 20

  VtyEvent (UI.EvKey (UI.KChar 'm') []) -> UI.suspendAndResume $ do
    newPlayMode <- menuSelection_ [minBound .. maxBound] (Just playMode) "Select Mode"
    return $ case newPlayMode of
      Just newPlayMode -> state { playMode = newPlayMode }
      Nothing -> state

  _ -> UI.continue state

musicPlayerApp :: UI.App State Event
musicPlayerApp = UI.App { UI.appDraw = musicPlayerDraw
                        , UI.appStartEvent = return
                        , UI.appHandleEvent = musicPlayerEvent
                        , UI.appAttrMap = const UI.defaultAttributeMap
                        , UI.appLiftVtyEvent = VtyEvent
                        , UI.appChooseCursor = UI.neverShowCursor
                        }

musicPlayer_ :: MusicSource -> SomeSession -> IO ()
musicPlayer_ source session = do
  player <- initPlayer
  chan <- newChan
  let postEvent = writeChan chan
  let state = State { session = session
                    , player = player
                    , source = source
                    , playMode = defaultPlayMode source
                    , playSequence = S.empty
                    , onStopped = True
                    , currentIndex = 0
                    , focusedIndex = 0
                    , volume = 100
                    , progress = (0, 0)
                    , currentLyrics = []
                    , postEvent = postEvent
                    , pendingMasked = True
                    }
  postEvent UserEventFetchMore
  void $ UI.customMain (UI.mkVty def) chan musicPlayerApp state

musicPlayer :: MusicSource -> SomeSession -> ContT () IO ()
musicPlayer source session = ContT (const $ musicPlayer_ source session)
