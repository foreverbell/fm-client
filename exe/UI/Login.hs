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
import qualified UI.Attributes as UI

import           Control.Monad.IO.Class (liftIO)
import           System.Exit (exitSuccess)

data EditorType = UserNameEditor | PasswordEditor
  deriving (Show)

data State = State { 
  currentEditor  :: EditorType
, userNameEditor :: UI.Editor
, passwordEditor :: UI.Editor
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
    ui = UI.vCenter $ UI.vBox $ map UI.hCenter 
           [ UI.mkBanner "NetEase Login"
           , UI.str " "
           , UI.str "username: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor userNameEditor)
           , UI.str "password: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor passwordEditor)
           ]

loginEvent :: State -> UI.Event -> UI.EventM (UI.Next State)
loginEvent state event = case event of
  UI.EvKey UI.KEsc [] -> liftIO exitSuccess
  UI.EvKey UI.KEnter [] -> UI.halt state
  UI.EvKey (UI.KChar '\t') [] -> UI.continue $ switchEditor state
  UI.EvKey UI.KBackTab [] -> UI.continue $ switchEditor state
  _ -> do
    editor <- UI.handleEvent event (selectEditor state)
    UI.continue $ case currentEditor state of
      UserNameEditor -> state { userNameEditor = editor }
      PasswordEditor -> state { passwordEditor = editor }

loginCursor :: State -> [UI.CursorLocation] -> Maybe UI.CursorLocation
loginCursor state = UI.showCursorNamed (editorName $ currentEditor state)

loginApp :: UI.App State UI.Event
loginApp = UI.App { UI.appDraw = loginDraw
                  , UI.appChooseCursor = loginCursor
                  , UI.appHandleEvent = loginEvent
                  , UI.appStartEvent = return
                  , UI.appAttrMap = const $ UI.attributeMap
                  , UI.appLiftVtyEvent = id
                  }

login :: IO (String, String)
login = do
  let editor1 = UI.editor (editorName UserNameEditor) (UI.str . unlines) Nothing []
  let editor2 = UI.editor (editorName PasswordEditor) (\[s] -> UI.str $ replicate (length s) '*') Nothing []
  state <- UI.defaultMain loginApp $ State UserNameEditor editor1 editor2
  let [userName] = UI.getEditContents $ userNameEditor state
  let [password] = UI.getEditContents $ passwordEditor state
  return (userName, password)
