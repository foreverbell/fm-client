{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import           UI.Black (black)

import           UI.Types

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Cont (Cont, cont)
import           System.Exit (exitSuccess)

import qualified FM.NetEase as NetEase
import           FM.FM (runSessionOnly)

data EditorType = UserNameEditor | PasswordEditor
  deriving (Show)

data State = State { 
  currentEditor  :: EditorType
, userNameEditor :: UI.Editor
, passwordEditor :: UI.Editor
, musicSource    :: MusicSource
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
    ui = UI.vCenter $ UI.vBox 
           [ UI.mkYellow $ UI.hCenter $ UI.str "NetEase Login"
           , UI.separator
           , UI.hCenter $ UI.str "username: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor userNameEditor)
           , UI.hCenter $ UI.str "password: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor passwordEditor)
           ]

loginEvent :: State -> UI.Event -> UI.EventM (UI.Next State)
loginEvent state@State {..} event = case event of
  UI.EvKey UI.KEsc [] -> liftIO exitSuccess
  UI.EvKey UI.KEnter [] -> case currentEditor of
    PasswordEditor -> do
      let [userName] = UI.getEditContents $ userNameEditor
      let [password] = UI.getEditContents $ passwordEditor
      session <- NetEase.initSession True
      liftIO $ runSessionOnly session (NetEase.login userName password)
      UI.suspendAndResume $ do
        continuation (SomeSession session)
        exitSuccess
    UserNameEditor -> UI.continue $ switchEditor state
  UI.EvKey (UI.KChar '\t') [] -> UI.continue $ switchEditor state
  UI.EvKey UI.KBackTab [] -> UI.continue $ switchEditor state
  _ -> do
    editor <- UI.handleEvent event (selectEditor state)
    UI.continue $ case currentEditor of
      UserNameEditor -> state { userNameEditor = editor }
      PasswordEditor -> state { passwordEditor = editor }

loginCursor :: State -> [UI.CursorLocation] -> Maybe UI.CursorLocation
loginCursor state = UI.showCursorNamed (editorName $ currentEditor state)

loginApp :: UI.App State UI.Event
loginApp = UI.App { UI.appDraw = loginDraw
                  , UI.appChooseCursor = loginCursor
                  , UI.appHandleEvent = loginEvent
                  , UI.appStartEvent = return
                  , UI.appAttrMap = const UI.defaultAttributeMap
                  , UI.appLiftVtyEvent = id
                  }

loginCPS :: MusicSource -> (SomeSession -> IO ()) -> IO ()
loginCPS NetEasePublicFM cont = black (SomeSession <$> NetEase.initSession True) cont
loginCPS source cont = do
  let editor1 = UI.editor (editorName UserNameEditor) (UI.str . unlines) Nothing []
  let editor2 = UI.editor (editorName PasswordEditor) (\[s] -> UI.str $ replicate (length s) '*') Nothing []
  void $ UI.defaultMain loginApp State { currentEditor = UserNameEditor
                                       , userNameEditor = editor1
                                       , passwordEditor = editor2
                                       , musicSource = source
                                       , continuation = cont
                                       }

login :: MusicSource -> Cont (IO ()) SomeSession
login source = cont (loginCPS source)
