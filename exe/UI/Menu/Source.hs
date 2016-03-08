{-# LANGUAGE RecordWildCards #-}

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
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Cont (Cont, cont)
import           System.Exit (exitSuccess)

data State = State {
  source :: MusicSource
, continuation :: MusicSource -> IO ()
}

safeSucc :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safeSucc cyclic x 
  | x == maxBound = if cyclic then minBound else maxBound
  | otherwise = succ x

safePred :: (Enum a, Bounded a, Eq a) => Bool -> a -> a
safePred cyclic x
  | x == minBound = if cyclic then maxBound else minBound
  | otherwise = pred x

sourceMenuDraw :: State -> [UI.Widget]
sourceMenuDraw state = [ui]
  where
    ui = UI.vCenter $ UI.vBox [ title, UI.str " ", menu ]
    title = UI.mkYellow $ UI.hCenter $ UI.str "Select Source"
    menu = UI.hCenter $ UI.vBox $ do
      ms <- [minBound .. maxBound] :: [MusicSource]
      let mkItem | source state == ms = UI.mkCyan . UI.str . UI.mkFocused
                 | otherwise = UI.mkWhite . UI.str . UI.mkUnfocused
      return $ mkItem (show ms)

sourceMenuEvent :: State -> UI.Event -> UI.EventM (UI.Next State)
sourceMenuEvent state@State {..} event = case event of
  UI.EvKey UI.KEsc [] -> liftIO exitSuccess
  UI.EvKey UI.KEnter [] -> UI.suspendAndResume $ do
    continuation source
    exitSuccess
  UI.EvKey (UI.KChar '\t') [] -> UI.continue $ state { source = safeSucc True source }
  UI.EvKey UI.KBackTab [] -> UI.continue $ state { source = safePred True source }
  UI.EvKey UI.KDown [] -> UI.continue $ state { source = safeSucc False source }
  UI.EvKey UI.KUp [] -> UI.continue $ state { source = safePred False source }
  _ -> UI.continue state

sourceMenuApp :: UI.App State UI.Event
sourceMenuApp = UI.App { UI.appDraw = sourceMenuDraw
                       , UI.appStartEvent = return
                       , UI.appHandleEvent = sourceMenuEvent
                       , UI.appAttrMap = const UI.defaultAttributeMap
                       , UI.appLiftVtyEvent = id
                       , UI.appChooseCursor = UI.neverShowCursor
                       }

sourceMenuCPS :: (MusicSource -> IO ()) -> IO ()
sourceMenuCPS cont = void $ UI.defaultMain sourceMenuApp (State minBound cont)

sourceMenu :: Cont (IO ()) MusicSource
sourceMenu = cont sourceMenuCPS
