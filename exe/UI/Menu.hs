{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UI.Menu (
  menuSelection
, menuSelection_
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Monad (void)
import           Control.Monad.Cont (ContT (..))
import           Data.List (elemIndex)
import           Data.Maybe (fromMaybe)
import qualified Data.Sequence as S

import           Types

data State a = State {
  menuSequence :: S.Seq a
, uiTitle      :: String
, currentIndex :: Int
, continuation :: Maybe (a -> IO ())
, isSelected   :: Bool
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
  UI.EvKey UI.KEsc [] ->
    UI.halt state

  UI.EvKey (UI.KChar ' ') [] ->
    menuSelectionEvent state (UI.EvKey UI.KEnter [])
  UI.EvKey UI.KEnter [] -> emptyGuard $
    case continuation of
      Just continuation -> UI.suspendAndResume $ do
        continuation (menuSequence `S.index` currentIndex)
        return state { isSelected = True }
      Nothing -> UI.halt state { isSelected = True }

  UI.EvKey UI.KDown [] -> emptyGuard $
    UI.continue $ state
      { currentIndex = (currentIndex + 1) `mod` S.length menuSequence }

  UI.EvKey UI.KUp [] -> emptyGuard $
    UI.continue $ state
      { currentIndex = (currentIndex - 1) `mod` S.length menuSequence }

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

menuSelectionCont :: (Show1 a, Eq a) => [a] -> Maybe a -> String -> Maybe (a -> IO ()) -> IO (Maybe Int)
menuSelectionCont menu def title continuation = do
  let startIndex = maybe 0 (\def -> fromMaybe 0 (elemIndex def menu)) def
  let state = State { menuSequence = S.fromList menu
                    , uiTitle = title
                    , currentIndex = startIndex
                    , continuation = continuation
                    , isSelected = False
                    }
  State {..} <- UI.defaultMain menuSelectionApp state
  return $ if isSelected then Just currentIndex else Nothing

menuSelection :: (Show1 a, Eq a) => [a] -> Maybe a -> String -> ContT () IO a
menuSelection menu def title = ContT (void . menuSelectionCont menu def title . Just)

menuSelection_ :: (Show1 a, Eq a) => [a] -> Maybe a -> String -> IO (Maybe a)
menuSelection_ menu def title = do
  result <- menuSelectionCont menu def title Nothing
  return $ case result of
    Just id -> Just (menu !! id)
    Nothing -> Nothing
