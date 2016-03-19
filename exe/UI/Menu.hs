{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UI.Menu (
  menuSelection
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Monad (void)
import           Control.Monad.Cont (ContT (..))
import qualified Data.Sequence as S

import           Types

data State a = State {
  menuSequence :: S.Seq a
, uiTitle      :: String
, currentIndex :: Int
, continuation :: a -> IO ()
}

menuSelectionDraw :: (Show1 a) => State a -> [UI.Widget]
menuSelectionDraw State {..} = [ui]
  where
    ui = UI.vCenter $ UI.vLimit 15 $ UI.vBox [ title, UI.str " ", menu ]
    title = UI.mkYellow $ UI.hCenter $ UI.str uiTitle
    menu = UI.viewport "vp" UI.Vertical $ UI.hCenter $ UI.vBox $ do
      index <- [0 .. S.length menuSequence - 1]
      let mkItem | currentIndex == index = UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem $ show1 $ menuSequence `S.index` index

menuSelectionEvent :: State a -> UI.Event -> UI.EventM (UI.Next (State a))
menuSelectionEvent state@State {..} event = case event of
  UI.EvKey UI.KEsc [] -> UI.halt state
  UI.EvKey (UI.KChar ' ') [] -> menuSelectionEvent state (UI.EvKey UI.KEnter [])
  UI.EvKey UI.KEnter [] -> emptyGuard $ UI.suspendAndResume $ do
                             continuation (menuSequence `S.index` currentIndex)
                             return state
  UI.EvKey UI.KDown [] -> emptyGuard $ UI.continue $ state { currentIndex = (currentIndex + 1) `mod` S.length menuSequence }
  UI.EvKey UI.KUp [] -> emptyGuard $ UI.continue $ state { currentIndex = (currentIndex - 1) `mod` S.length menuSequence }
  _ -> UI.continue state
  where 
    emptyGuard m = if S.null menuSequence then UI.continue state else m

menuSelectionApp :: (Show1 a) => UI.App (State a) UI.Event
menuSelectionApp = UI.App { UI.appDraw = menuSelectionDraw
                          , UI.appStartEvent = return
                          , UI.appHandleEvent = menuSelectionEvent
                          , UI.appAttrMap = const UI.defaultAttributeMap
                          , UI.appLiftVtyEvent = id
                          , UI.appChooseCursor = UI.neverShowCursor
                          }

menuSelectionCPS :: (Show1 a) => [a] -> String -> (a -> IO ()) -> IO ()
menuSelectionCPS menu title cont = void $ UI.defaultMain menuSelectionApp state
  where
    state = State { menuSequence = S.fromList menu
                  , uiTitle = title 
                  , currentIndex = 0 
                  , continuation = cont
                  }

menuSelection :: (Show1 a) => [a] -> String -> ContT () IO a
menuSelection menu title = ContT (menuSelectionCPS menu title)
