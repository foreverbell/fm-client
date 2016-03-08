module Main where

import Control.Monad.Cont

import qualified UI.Login as Login
import qualified UI.Menu.Source as Menu
import qualified UI.Menu.Player as Menu

{- FIXME: Use Cont(CPS) is a workaround, since brick doesn't support stacking windows well. -}
main :: IO ()
main = flip runCont id $ do
  source <- Menu.sourceMenu
  passport <- Login.login source
  Menu.playerMenu source passport
