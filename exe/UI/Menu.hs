{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module UI.Menu ( 
  sourceMenu
, playerMenu
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Attributes as UI

import           Control.Monad.IO.Class (liftIO)
import           System.Exit (exitSuccess)

import           FM.FM
import           FM.Session
import qualified FM.Song as Song
import qualified FM.NetEase as NetEase

data MusicSource = NetEaseFM
                 | NetEaseDailyRecommendation
  deriving (Enum, Bounded, Eq)

instance Show MusicSource where
  show NetEaseFM = "NetEase Cloud Music FM"
  show NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"

safeSucc :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safeSucc cyclic x 
  | x == maxBound = if cyclic then minBound else maxBound
  | otherwise = succ x

safePred :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safePred cyclic x
  | x == minBound = if cyclic then maxBound else minBound
  | otherwise = pred x

sourceMenuDraw :: MusicSource -> [UI.Widget]
sourceMenuDraw state = [ui]
  where
    ui = UI.vCenter $ UI.vBox [ title, UI.str " ", menu ]
    title = UI.hCenter (UI.mkBanner "Select Source")
    menu = UI.hCenter $ UI.vBox $ do
      source <- [minBound, maxBound] :: [MusicSource]
      let mkItem | source == state = UI.mkFocused
                 | otherwise = UI.mkUnfocused
      return $ mkItem (show source)

sourceMenuEvent :: MusicSource -> UI.Event -> UI.EventM (UI.Next MusicSource)
sourceMenuEvent state event = case event of
  UI.EvKey UI.KEsc [] -> liftIO exitSuccess
  UI.EvKey UI.KEnter [] -> UI.halt state
  UI.EvKey (UI.KChar '\t') [] -> UI.continue $ safeSucc True state
  UI.EvKey UI.KBackTab [] -> UI.continue $ safePred True state
  UI.EvKey UI.KDown [] -> UI.continue $ safeSucc False state
  UI.EvKey UI.KUp [] -> UI.continue $ safePred False state
  _ -> UI.continue state

sourceMenuApp :: UI.App MusicSource UI.Event
sourceMenuApp = UI.App { UI.appDraw = sourceMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = sourceMenuEvent
                       , UI.appAttrMap = const $ UI.attributeMap
                       , UI.appLiftVtyEvent = id
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

sourceMenu :: IO MusicSource
sourceMenu = UI.defaultMain sourceMenuApp minBound

type SongList = ([Song.Song], [Song.Song])
 
data State = State {
  source :: MusicSource
, playList :: SongList
, navigateList :: SongList
, session :: SomeSession
, fmState :: Maybe FMState
}

liftSession :: (IsSession s) => State -> SessionOnly s a -> IO a
liftSession State {..} m = runSessionOnly (fromSession session) m

liftState :: State -> StateOnly a -> IO (a, State)
liftState state@State {..} m = do
  (r, s) <- runStateOnly fmState m
  return $ (r, state { fmState = Just s })

fetch :: State -> IO [Song.Song]
fetch state@State {..} = case source of
  NetEaseFM -> liftSession state NetEase.fetchFM
  NetEaseDailyRecommendation -> liftSession state NetEase.fetchRListAsFM

playerMenu :: MusicSource -> IO ()
playerMenu source = undefined
