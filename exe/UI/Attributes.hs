{-# LANGUAGE OverloadedStrings #-}

module UI.Attributes ( 
  attributeMap
, mkBanner
, mkFocused
, mkUnfocused
) where

import           Brick.AttrMap
import           Brick.Widgets.Core (str, withAttr)
import           Brick.Types (Widget)
import           Data.Default.Class
import qualified Graphics.Vty as V

bannerAttr :: AttrName
bannerAttr = "banner"

mkBanner :: String -> Widget
mkBanner t = withAttr bannerAttr (str t)

focusedAttr :: AttrName
focusedAttr = "focused"

mkFocused :: String -> Widget
mkFocused t = withAttr focusedAttr (str $ "-> " ++ t)

unfocusedAttr :: AttrName
unfocusedAttr = "unfocused"

mkUnfocused :: String -> Widget
mkUnfocused t = withAttr unfocusedAttr (str $ "   " ++ t)

attributeMap :: AttrMap
attributeMap = attrMap def 
  [ (bannerAttr, V.defAttr `V.withForeColor` V.yellow)
  , (focusedAttr, V.defAttr `V.withForeColor` V.cyan)
  , (unfocusedAttr, V.defAttr `V.withForeColor` V.white)
  ]
