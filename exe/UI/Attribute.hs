{-# LANGUAGE OverloadedStrings #-}

module UI.Attribute ( 
  attributeMap
, mkBanner
, mkSelected
, mkUnselected
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

selectedAttr :: AttrName
selectedAttr = "selected"

mkSelected :: String -> Widget
mkSelected t = withAttr selectedAttr (str $ "-> " ++ t)

unselectedAttr :: AttrName
unselectedAttr = "unselected"

mkUnselected :: String -> Widget
mkUnselected t = withAttr unselectedAttr (str $ "   " ++ t)

attributeMap :: AttrMap
attributeMap = attrMap def 
  [ (bannerAttr, V.defAttr `V.withForeColor` V.yellow)
  , (selectedAttr, V.defAttr `V.withForeColor` V.cyan)
  , (unselectedAttr, V.defAttr `V.withForeColor` V.white)
  ]
