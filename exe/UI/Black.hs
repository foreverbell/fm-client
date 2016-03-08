module UI.Black (
  black
) where

-- HACK HACK.

import qualified Brick.Main as UI
import qualified Brick.Widgets.Core as UI
import qualified Brick.Types as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Concurrent.Chan (newChan, writeChan)
import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class
import           System.Exit (exitSuccess)

data Event = Event UI.Event | Ohayou
type State a = (IO a, a -> IO ())

blackEvent :: State a -> Event -> UI.EventM (UI.Next (State a))
blackEvent (m, f) Ohayou = do
  m' <- liftIO $ m
  UI.suspendAndResume $ f m' >> exitSuccess
blackEvent state _ = UI.continue state

blackApp :: UI.App (State a) Event
blackApp = UI.App { UI.appDraw = const [UI.str []]
                  , UI.appStartEvent = return
                  , UI.appHandleEvent = blackEvent
                  , UI.appAttrMap = const UI.defaultAttributeMap
                  , UI.appLiftVtyEvent = Event
                  , UI.appChooseCursor = UI.neverShowCursor
                  }

black :: IO a -> (a -> IO ()) -> IO ()
black m f = do
  chan <- newChan
  writeChan chan Ohayou
  void $ UI.customMain (UI.mkVty def) chan blackApp (m, f)
