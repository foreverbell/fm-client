{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UI.Login ( 
  login
) where

import qualified Brick.Main as UI
import qualified Brick.Types as UI
import qualified Brick.Widgets.Center as UI
import qualified Brick.Widgets.Core as UI
import           Brick.Widgets.Core ((<+>))
import qualified Brick.Widgets.Edit as UI
import qualified Graphics.Vty as UI
import qualified UI.Extra as UI

import           UI.Types

import           Control.Concurrent.Chan (Chan, newChan, writeChan)

import           Control.Monad (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Cont (ContT (..))
import           Data.Default.Class

import qualified FM.NetEase as NetEase
import           FM.FM (runSessionOnly)

data Event = Event UI.Event | Ohayou | Oyasumi

data EditorType = UserNameEditor | PasswordEditor
  deriving (Show)

data State = State { 
  currentEditor  :: EditorType
, userNameEditor :: UI.Editor
, passwordEditor :: UI.Editor
, musicSource    :: MusicSource
, uiTitle        :: String
, chan           :: Chan Event
, continuation   :: SomeSession -> IO ()
}

editorName :: EditorType -> UI.Name
editorName e = UI.Name (show e)

selectEditor :: State -> UI.Editor
selectEditor State {..} = case currentEditor of
  UserNameEditor -> userNameEditor
  PasswordEditor -> passwordEditor

switchEditor :: State -> State
switchEditor state@State {..} = state { currentEditor = newEditor }
  where
    newEditor = case currentEditor of
      UserNameEditor -> PasswordEditor
      PasswordEditor -> UserNameEditor

loginDraw :: State -> [UI.Widget]
loginDraw State {..} = [ui]
  where
    black = UI.str []
    ui = case musicSource of
      NetEasePublicFM -> black
      _ -> UI.vCenter $ UI.vBox [ UI.mkYellow $ UI.hCenter $ UI.str uiTitle
                                , UI.separator
                                , UI.hCenter $ UI.str "username: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor userNameEditor)
                                , UI.hCenter $ UI.str "password: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor passwordEditor)
                                ]

loginEvent :: State -> Event -> UI.EventM (UI.Next State)
loginEvent state@State {..} event = case event of
  Ohayou -> do
    session <- NetEase.initSession True
    UI.suspendAndResume $ continuation session >> writeChan chan Oyasumi >> return state
  Oyasumi -> UI.halt state
  Event (UI.EvKey UI.KEsc []) -> UI.halt state
  Event (UI.EvKey UI.KEnter []) -> case currentEditor of
    PasswordEditor -> do
      let [userName] = UI.getEditContents userNameEditor
      let [password] = UI.getEditContents passwordEditor
      session <- case viewMusicSource musicSource of
        NetEaseMusic -> do
          session <- NetEase.initSession True
          liftIO $ runSessionOnly session (NetEase.login userName password)
          return session
      UI.suspendAndResume $ continuation session >> writeChan chan Oyasumi >> return state
    UserNameEditor -> UI.continue $ switchEditor state
  Event (UI.EvKey (UI.KChar '\t') []) -> UI.continue $ switchEditor state
  Event (UI.EvKey UI.KBackTab []) -> UI.continue $ switchEditor state
  Event event -> do
    editor <- UI.handleEvent event (selectEditor state)
    UI.continue $ case currentEditor of
      UserNameEditor -> state { userNameEditor = editor }
      PasswordEditor -> state { passwordEditor = editor }

loginCursor :: State -> [UI.CursorLocation] -> Maybe UI.CursorLocation
loginCursor state = UI.showCursorNamed (editorName $ currentEditor state)

loginApp :: UI.App State Event
loginApp = UI.App { UI.appDraw = loginDraw
                  , UI.appChooseCursor = loginCursor
                  , UI.appHandleEvent = loginEvent
                  , UI.appStartEvent = return
                  , UI.appAttrMap = const UI.defaultAttributeMap
                  , UI.appLiftVtyEvent = Event
                  }

loginCPS :: String -> MusicSource -> (SomeSession -> IO ()) -> IO ()
loginCPS title source cont = do
  let editor1 = UI.editor (editorName UserNameEditor) (UI.str . unlines) Nothing []
  let editor2 = UI.editor (editorName PasswordEditor) (\[s] -> UI.str $ replicate (length s) '*') Nothing []
  chan <- newChan
  when (source == NetEasePublicFM) (writeChan chan Ohayou)
  void $ UI.customMain (UI.mkVty def) chan loginApp State { currentEditor = UserNameEditor
                                                          , userNameEditor = editor1
                                                          , passwordEditor = editor2
                                                          , musicSource = source
                                                          , uiTitle = title
                                                          , chan = chan
                                                          , continuation = cont
                                                          }

login :: String -> MusicSource -> ContT () IO SomeSession
login title source = ContT (loginCPS title source)
