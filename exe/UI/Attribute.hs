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

attrBanner :: AttrName
attrBanner = "banner"

mkBanner :: String -> Widget
mkBanner t = withAttr attrBanner (str t)

attrSelected :: AttrName
attrSelected = "selected"

mkSelected :: String -> Widget
mkSelected t = withAttr attrSelected (str $ "-> " ++ t)

attrUnselected :: AttrName
attrUnselected = "unselected"

mkUnselected :: String -> Widget
mkUnselected t = withAttr attrUnselected (str $ "   " ++ t)

attributeMap :: AttrMap
attributeMap = attrMap def 
  [ (attrBanner, V.defAttr `V.withForeColor` V.yellow)
  , (attrSelected, V.defAttr `V.withForeColor` V.cyan)
  , (attrUnselected, V.defAttr `V.withForeColor` V.white)
  ]
