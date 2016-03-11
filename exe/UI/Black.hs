module UI.Black (
  black
) where

import qualified Brick.Main as UI
import qualified Brick.Widgets.Core as UI
import qualified Brick.Types as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Concurrent.Chan (newChan, writeChan)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class

data Event = Event UI.Event | Ohayou | Oyasumi
type State a b = (IO a, a -> IO b, Maybe b)

blackEvent :: State a b -> Event -> UI.EventM (UI.Next (State a b))
blackEvent (m, f, _) Ohayou = do
  m' <- liftIO m
  UI.suspendAndResume $ do
    r <- f m'
    return (m, f, Just r)
blackEvent state Oyasumi = UI.halt state
blackEvent state _ = UI.continue state

blackApp :: UI.App (State a b) Event
blackApp = UI.App { UI.appDraw = const [UI.str []]
                  , UI.appStartEvent = return
                  , UI.appHandleEvent = blackEvent
                  , UI.appAttrMap = const UI.defaultAttributeMap
                  , UI.appLiftVtyEvent = Event
                  , UI.appChooseCursor = UI.neverShowCursor
                  }

black :: IO a -> (a -> IO b) -> IO b
black m f = do
  chan <- newChan
  writeChan chan Ohayou
  writeChan chan Oyasumi
  (_, _, Just r) <- UI.customMain (UI.mkVty def) chan blackApp (m, f, Nothing)
  return r
