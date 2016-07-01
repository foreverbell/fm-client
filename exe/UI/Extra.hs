{-# LANGUAGE OverloadedStrings #-}

module UI.Extra (
  defaultAttributeMap
, mkFocused, mkUnfocused
, mkYellow
, mkCyan
, mkWhite
, mkRed
, mkGreen
, separator
) where

import           Brick.AttrMap
import           Brick.Widgets.Core
import           Brick.Types
import           Data.Default.Class
import qualified Graphics.Vty as V

mkFocused :: String -> String
mkFocused = (++) "~> "

mkUnfocused :: String -> String
mkUnfocused = (++) "   "

mkYellow :: Widget -> Widget
mkYellow = withAttr "yellow"

mkCyan :: Widget -> Widget
mkCyan = withAttr "cyan"

mkWhite :: Widget -> Widget
mkWhite = withAttr "white"

mkRed :: Widget -> Widget
mkRed = withAttr "red"

mkGreen :: Widget -> Widget
mkGreen = withAttr "green"

defaultAttributeMap :: AttrMap
defaultAttributeMap = attrMap def $ map comb [ ("yellow", V.yellow)
                                             , ("cyan", V.cyan)
                                             , ("white", V.white)
                                             , ("red", V.red)
                                             , ("green", V.green)
                                             ]
  where comb (t, c) = (t, V.defAttr `V.withForeColor` c)

separator :: Widget
separator = str " "
