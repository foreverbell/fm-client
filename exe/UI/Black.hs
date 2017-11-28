module UI.Black (
  black
) where

import qualified Brick.Main as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import qualified Brick.Types as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           Control.Concurrent.Chan (newChan, writeChan)
import           Control.Monad.IO.Class (liftIO)
import           Data.Default.Class
import           Data.Maybe (fromMaybe)

data Event = Event UI.Event | Ohayou | Oyasumi
type State a b = (String, IO a, a -> IO b, Maybe b)

blackEvent :: State a b -> Event -> UI.EventM (UI.Next (State a b))
blackEvent (t, m, f, _) Ohayou = do
  m' <- liftIO m
  UI.suspendAndResume $ do
    r <- f m'
    return (t, m, f, Just r)
blackEvent state Oyasumi = UI.halt state
blackEvent state _ = UI.continue state

blackApp :: UI.App (State a b) Event
blackApp = UI.App { UI.appDraw = \(t, _, _, _) -> [UI.center (UI.str t)]
                  , UI.appStartEvent = return
                  , UI.appHandleEvent = blackEvent
                  , UI.appAttrMap = const UI.defaultAttributeMap
                  , UI.appLiftVtyEvent = Event
                  , UI.appChooseCursor = UI.neverShowCursor
                  }

black :: Maybe String -> IO a -> (a -> IO b) -> IO b
black t m f = do
  chan <- newChan
  writeChan chan Ohayou
  writeChan chan Oyasumi
  let title = fromMaybe [] t
  (_, _, _, Just r) <-
    UI.customMain (UI.mkVty def) chan blackApp (title, m, f, Nothing)
  return r
