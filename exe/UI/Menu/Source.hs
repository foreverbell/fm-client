{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UI.Menu.Source (
  sourceMenu
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           UI.Types

import           Control.Monad (void)
import           Control.Monad.Cont (ContT (..))
import qualified Data.Sequence as S

data State = State {
  sourceSequence :: S.Seq MusicSource
, currentIndex   :: Int
, continuation   :: MusicSource -> IO ()
}

sourceMenuDraw :: State -> [UI.Widget]
sourceMenuDraw State {..} = [ui]
  where
    ui = UI.vCenter $ UI.vLimit 15 $ UI.vBox [ title, UI.str " ", menu ]
    title = UI.mkYellow $ UI.hCenter $ UI.str "Select Source"
    menu = UI.viewport "vp" UI.Vertical $ UI.hCenter $ UI.vBox $ do
      index <- [0 .. S.length sourceSequence - 1]
      let mkItem | currentIndex == index = UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem $ show $ sourceSequence `S.index` index

sourceMenuEvent :: State -> UI.Event -> UI.EventM (UI.Next State)
sourceMenuEvent state@State {..} event = case event of
  UI.EvKey UI.KEsc [] -> UI.halt state
  UI.EvKey (UI.KChar ' ') [] -> sourceMenuEvent state (UI.EvKey UI.KEnter [])
  UI.EvKey UI.KEnter [] -> guard $ UI.suspendAndResume $ do
                             continuation (sourceSequence `S.index` currentIndex)
                             return state
  UI.EvKey UI.KDown [] -> guard $ UI.continue $ state { currentIndex = (currentIndex + 1) `mod` S.length sourceSequence }
  UI.EvKey UI.KUp [] -> guard $ UI.continue $ state { currentIndex = (currentIndex - 1) `mod` S.length sourceSequence }
  _ -> UI.continue state
  where guard m = if S.null sourceSequence then UI.continue state else m

sourceMenuApp :: UI.App State UI.Event
sourceMenuApp = UI.App { UI.appDraw = sourceMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = sourceMenuEvent
                       , UI.appAttrMap = const UI.defaultAttributeMap
                       , UI.appLiftVtyEvent = id
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

sourceMenuCPS :: [MusicSource] -> (MusicSource -> IO ()) -> IO ()
sourceMenuCPS sources cont = void $ UI.defaultMain sourceMenuApp $ State (S.fromList sources) 0 cont

sourceMenu :: [MusicSource] -> ContT () IO MusicSource
sourceMenu sources = ContT (sourceMenuCPS sources)
