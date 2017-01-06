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

import           Control.Concurrent.Chan (writeChan, newChan)
import           Control.Concurrent.STM.TVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (ContT (..))
import           Control.Monad.STM (atomically)
import           Data.Default.Class
import           Data.Foldable (toList)
import           Data.List (intercalate)
import           Data.Maybe (isJust, fromJust)
import qualified Data.Sequence as S
import           Text.Printf (printf)
import           System.Random (randomRIO)
import           System.Process (callProcess)

import qualified FM.FM as FM
import qualified FM.Song as Song
import qualified FM.Cache as Cache
import qualified FM.NetEase as NetEase

import           UI.Menu
import           Types

data State = State {
  session       :: SomeSession
, player        :: Player
, cache         :: Cache
, source        :: MusicSource
, playMode      :: PlayMode
, playSequence  :: S.Seq Song.Song
, stopped       :: Bool
, currentIndex  :: Int
, focusedIndex  :: Int
, progress      :: (Double, Double)
, currentLyrics :: String
, postEvent     :: Event -> IO ()
, autoProceed   :: Bool
}

data Event = VtyEvent UI.Event
           | UserEventFetchMore
           | UserEventPending Bool
           | UserEventUpdateProgress (Double, Double)
           | UserEventUpdateLyrics String

liftCache State {..} m = liftIO $ runCache cache m
liftSession State {..} m = liftIO $ runSession session m
liftPlayer State {..} m = liftIO $ runPlayer player m

fetch :: (MonadIO m) => State -> m [Song.Song]
fetch state@State {..} = case source of
  NetEaseFM -> liftSession state NetEase.fetchFM
  NetEaseDailyRecommendation -> liftSession state NetEase.fetchRecommend
  NetEasePlayLists -> undefined
  NetEasePlayList id _ -> liftSession state (NetEase.fetchPlayList id)
  LocalCache -> liftSession state Cache.fetchCache

fetchUrl :: (MonadIO m) => State -> Song.Song -> m (Maybe String)
fetchUrl state@State {..} song = if isLocal source
  then liftSession state (Cache.fetchUrl song)
  else do
    localPath <- liftCache state (Cache.lookupCache (Song.uid song))
    case localPath of
      Just path -> return $ Just path
      Nothing -> liftSession state (NetEase.fetchUrl song)

fetchLyrics :: (MonadIO m) => State -> Song.Song -> m Song.Lyrics
fetchLyrics state@State {..} song = if isLocal source
  then liftSession state (Cache.fetchLyrics song)
  else liftSession state (NetEase.fetchLyrics song)

fetchMore :: (MonadIO m) => State -> m State
fetchMore state@State {..} = do
  new <- S.fromList <$> fetch state
  return state { playSequence = playSequence S.>< new }

play :: (MonadIO m) => State -> m State
play state@State {..}
  | currentIndex == 0 = return state
  | otherwise = do
      let onBegin () = let Song.Song {..} = playSequence `S.index` (currentIndex - 1)
                        in callProcess "notify-send" [title ++ "\n\n" ++ intercalate " / " artists ++ "\n\n" ++ album]
      let onTerminate e = when e (postEvent (UserEventPending False))
      let onProgress p = postEvent (UserEventUpdateProgress p)
      let onLyrics l = postEvent (UserEventUpdateLyrics l)
      liftPlayer state $ FM.play (playSequence `S.index` (currentIndex - 1)) (fetchUrl state) (fetchLyrics state) onBegin onTerminate onProgress onLyrics
      return state { focusedIndex = currentIndex
                   , stopped = False
                   , progress = (0, 0)
                   , currentLyrics = []
                   , autoProceed = True }

pause :: (MonadIO m) => State -> m State
pause state = do
  liftPlayer state FM.pause
  return state { autoProceed = False }

resume :: (MonadIO m) => State -> m State
resume state = do
  liftPlayer state FM.resume
  return state { autoProceed = True }

stop :: (MonadIO m) => State -> m State
stop state = do
  liftPlayer state FM.stop
  return state { stopped = True
               , progress = (0, 0)
               , currentLyrics = []
               , autoProceed = False }

setVolume :: (MonadIO m) => State -> Int -> m State
setVolume state@State {..} d = do
  let newVolume = max 0 $ min 100 $ FM.playerVolume player + d
  let newState = state { player = player { FM.playerVolume = newVolume } }
  liftPlayer newState FM.updateVolume
  return newState

toggleMute :: (MonadIO m) => State -> m State
toggleMute state@State {..} = do
  let newMuted = not (FM.playerMuted player)
  let newState = state { player = player { FM.playerMuted = newMuted } }
  liftPlayer newState FM.updateVolume
  return newState

cacheSong :: (MonadIO m) => State -> m State
cacheSong state@State {..} = do
  when (focusedIndex /= 0 && not (isLocal source)) $ do
    let song = playSequence `S.index` (focusedIndex - 1)
    url <- fetchUrl state song
    when (isJust url) $ liftCache state $ FM.cacheSong song (fromJust url)
  return state

deleteSong :: (MonadIO m) => State -> m State
deleteSong state@State {..} = do
  state <- stop state
  if (focusedIndex /= 0 && isLocal source)
    then liftCache state $ do
      FM.deleteSong $ playSequence `S.index` (focusedIndex - 1)
      let (heads, tails) = S.splitAt (focusedIndex - 1) playSequence
      let newSequence = heads `mappend` S.drop 1 tails
      let newIndex = if S.length tails == 1 then focusedIndex - 1 else focusedIndex
      return state { playSequence = newSequence, focusedIndex = newIndex }
    else return state

musicPlayerDraw :: State -> [UI.Widget]
musicPlayerDraw State {..} = [ui]
  where
    formatSong Song.Song {..} = printf "%s - %s - %s" title (intercalate " / " artists) album :: String

    formatTime time = printf "%02d:%02d" minute second :: String
      where (minute, second) = floor time `quotRem` 60 :: (Int, Int)

    ui = UI.vBox [UI.separator, title , UI.separator, bar1, UI.separator, bar2, UI.separator, lyrics, UI.separator, playList]

    title = UI.mkYellow $ UI.hCenter $ UI.str body
      where
        body | stopped = "[停止]"
             | otherwise = formatSong $ playSequence `S.index` (currentIndex - 1)

    bar1 | stopped = UI.separator
         | otherwise = UI.mkGreen $ UI.hCenter $ UI.str $ printf "[%s] (%s/%s)" (make '>' total occupied) (formatTime cur) (formatTime len)
      where
        (len, cur) = progress
        ratio = if len == 0 then 0 else cur / len
        total = 35 :: Int
        occupied = ceiling $ fromIntegral total * ratio
        make c total occupied = replicate occupied c ++ replicate (total - occupied) ' '

    bar2 = UI.mkCyan $ UI.hCenter $ UI.str $ unwords [playModeBar, volumeBar]
      where
        playModeBar = printf "[播放模式: %s]" (show1 playMode)
        volumeBar | FM.playerMuted player = "[静音]"
                  | otherwise = printf "[音量: %d%%]" (FM.playerVolume player)

    lyrics = UI.mkRed $ UI.hCenter $ UI.str $ if null currentLyrics then " " else currentLyrics

    playList = UI.viewport "vp" UI.Vertical $ UI.hCenter $ UI.vBox $ do
      (song, index) <- zip (toList playSequence) [1 .. ]
      let mkItem | index == focusedIndex = UI.visible . UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem (show index ++ ". " ++ formatSong song)

musicPlayerEvent :: State -> Event -> UI.EventM (UI.Next State)
musicPlayerEvent state@State {..} event = case event of
  UserEventFetchMore -> UI.continue =<< fetchMore state

  UserEventPending forceProceed -> do
    pState <- liftIO $ atomically $ readTVar (FM.playerState player)
    if (forceProceed || autoProceed) && (isStopped pState)
      then do
        let needMore = currentIndex == S.length playSequence && playMode == Stream
        state@State {..} <- if needMore then fetchMore state else return state
        nextIndex <- case playMode of
          Stream -> return $ min (S.length playSequence) (currentIndex + 1)
          LoopOne -> return currentIndex
          LoopAll -> return $ if currentIndex + 1 > S.length playSequence then 1 else currentIndex + 1
          Shuffle -> liftIO $ randomRIO (1, S.length playSequence)
        UI.continue =<< play state { currentIndex = nextIndex }
      else UI.continue state

  UserEventUpdateProgress p -> UI.continue state { progress = p }

  UserEventUpdateLyrics l -> UI.continue state { currentLyrics = l }

  VtyEvent (UI.EvKey UI.KEsc []) -> if stopped
    then UI.halt state
    else UI.continue =<< stop state

  VtyEvent (UI.EvKey (UI.KChar ' ') []) -> musicPlayerEvent state (VtyEvent $ UI.EvKey UI.KEnter [])
  VtyEvent (UI.EvKey UI.KEnter []) -> do
    pState <- liftIO $ atomically $ readTVar (FM.playerState player)
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

  VtyEvent (UI.EvKey (UI.KChar '-') []) -> UI.continue =<< setVolume state (-10)

  VtyEvent (UI.EvKey (UI.KChar '=') []) -> UI.continue =<< setVolume state 10

  VtyEvent (UI.EvKey (UI.KChar 'm') []) -> UI.continue =<< toggleMute state

  VtyEvent (UI.EvKey (UI.KChar 'n') []) -> do
    state <- stop state
    liftIO $ postEvent (UserEventPending True)
    UI.continue state

  VtyEvent (UI.EvKey (UI.KChar 'o') []) -> UI.suspendAndResume $ do
    newPlayMode <- menuSelection_ [minBound .. maxBound] (Just playMode) "播放模式"
    return $ case newPlayMode of
      Just newPlayMode -> state { playMode = newPlayMode }
      Nothing -> state

  VtyEvent (UI.EvKey (UI.KChar 'c') []) -> UI.continue =<< cacheSong state

  VtyEvent (UI.EvKey (UI.KChar 'C') []) -> UI.continue =<< deleteSong state

  _ -> UI.continue state

musicPlayerApp :: UI.App State Event
musicPlayerApp = UI.App { UI.appDraw = musicPlayerDraw
                        , UI.appStartEvent = return
                        , UI.appHandleEvent = musicPlayerEvent
                        , UI.appAttrMap = const UI.defaultAttributeMap
                        , UI.appLiftVtyEvent = VtyEvent
                        , UI.appChooseCursor = UI.neverShowCursor
                        }

musicPlayer_ :: MusicSource -> SomeSession -> Cache -> IO ()
musicPlayer_ source session cache = void $ do
  player <- FM.initPlayer
  chan <- newChan
  let postEvent = writeChan chan
  let state = State { session = session
                    , player = player
                    , cache = cache
                    , source = source
                    , playMode = defaultPlayMode source
                    , playSequence = S.empty
                    , stopped = True
                    , currentIndex = 0
                    , focusedIndex = 0
                    , progress = (0, 0)
                    , currentLyrics = []
                    , postEvent = postEvent
                    , autoProceed = False
                    }
  postEvent UserEventFetchMore
  UI.customMain (UI.mkVty def) chan musicPlayerApp state

musicPlayer :: MusicSource -> SomeSession -> Cache -> ContT () IO ()
musicPlayer source session cache = ContT (const $ musicPlayer_ source session cache)
