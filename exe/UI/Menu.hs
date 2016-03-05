{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module UI.Menu ( 
  sourceMenu
, playerMenu
) where

import qualified Brick.AttrMap as UI
import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Brick.Util as UI
import qualified Graphics.Vty as UI
import qualified UI.Attribute as UI

import           Control.Monad.IO.Class (liftIO)
import           System.Exit (exitSuccess)

import           FM.FM
import           FM.Session
import qualified FM.Song as Song

data MusicSource = NetEaseFM
                 | NetEaseDailyRecommendation
  deriving (Enum, Bounded, Eq)

instance Show MusicSource where
  show NetEaseFM = "NetEase FM"
  show NetEaseDailyRecommendation = "NetEase Daily Recommendation"

safeSucc :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safeSucc cyclic x 
  | x == maxBound = if cyclic then minBound else maxBound
  | otherwise = succ x

safePred :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safePred cyclic x
  | x == minBound = if cyclic then maxBound else minBound
  | otherwise = pred x

selectedAttr :: UI.AttrName
selectedAttr = "selected"

sourceMenuDraw :: MusicSource -> [UI.Widget]
sourceMenuDraw state = [ui]
  where
    ui = UI.vCenter $ UI.vBox [ title, UI.str " ", menu ]
    title = UI.hCenter (UI.mkBanner "Select Source")
    menu = UI.hCenter $ UI.vBox $ do
      source <- [minBound, maxBound] :: [MusicSource]
      let mkItem | source == state = UI.mkSelected
                 | otherwise = UI.mkUnselected
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

data State = State {
  source :: MusicSource
, playList :: ([Song.Song], [Song.Song])
, session :: SomeSession
, fmState :: Maybe FMState
}

playerMenu :: MusicSource -> IO ()
playerMenu source = undefined
