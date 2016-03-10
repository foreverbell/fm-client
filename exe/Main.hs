module Main where

import Control.Monad.Cont

import           UI.Types
import qualified UI.Login as Login
import qualified UI.Menu.Source as Menu
import qualified UI.Menu.Player as Menu

{- FIXME: Use Cont(CPS) is a workaround, since brick doesn't support stacking windows well. -}
main :: IO ()
main = flip runCont id $ do
  source <- Menu.sourceMenu [NetEaseFM, NetEasePublicFM, NetEaseDailyRecommendation]
  session <- Login.login source
  Menu.playerMenu source session
