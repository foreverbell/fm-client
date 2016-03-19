{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import           Control.Concurrent.Chan (Chan, newChan, writeChan)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (ContT (..))
import           Data.Default.Class
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)

import qualified FM.NetEase as NetEase
import           FM.FM (runSessionOnly)
import           SessionManager
import           Types

getConfig :: (MonadIO m) => MusicSourceType -> m String
getConfig source = do
  dir <- (++ "/.fm") <$> liftIO getHomeDirectory
  liftIO $ createDirectoryIfMissing True dir
  return $ dir ++ "/" ++ show1 source ++ ".conf"

readLoginConfig :: (MonadIO m) => MusicSourceType -> m (String, String)
readLoginConfig source = do
  conf <- getConfig source
  [userName, password] <- lines <$> liftIO (readFile conf)
  return (userName, password)

writeLoginConfig :: MusicSourceType -> (String, String) -> IO ()
writeLoginConfig source (userName, password) = do
  conf <- getConfig source
  liftIO $ writeFile conf (unlines [userName, password])

data Event = Event UI.Event | Hi | Hello | Goodbye

data EditorType = UserNameEditor | PasswordEditor
  deriving (Show)

data State = State { 
  currentEditor  :: EditorType
, userNameEditor :: UI.Editor
, passwordEditor :: UI.Editor
, musicSource    :: MusicSource
, sessionManager :: SessionManager
, onGreetings    :: Bool
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
    ui = UI.vCenter $ if onGreetings
            then UI.str []
            else UI.vBox [ UI.mkYellow $ UI.hCenter $ UI.str uiTitle
                         , UI.separator
                         , UI.hCenter $ UI.str "username: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor userNameEditor)
                         , UI.hCenter $ UI.str "password: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor passwordEditor)
                         ]

loginEvent :: State -> Event -> UI.EventM (UI.Next State)
loginEvent state@State {..} event = case event of
  Hi -> do
    session <- case viewType musicSource of
      NetEaseMusic -> NetEase.initSession True
    UI.suspendAndResume $ continuation session >> writeChan chan Goodbye >> return state

  Hello -> do
    let sourceType = viewType musicSource
    session <- liftIO $ try $ do
      mSession <- lookupSession sessionManager sourceType
      case mSession of
        Just session -> return session
        Nothing -> do
          (userName, password) <- readLoginConfig sourceType
          session <- case sourceType of
            NetEaseMusic -> do
              session <- NetEase.initSession True
              liftIO $ runSessionOnly session (NetEase.login userName password)
              return session
          insertSession sessionManager sourceType session
          return session
    case session of
      Left (_ :: SomeException) -> deleteSession sessionManager sourceType >> UI.continue state { onGreetings = False }
      Right session -> UI.suspendAndResume $ continuation session >> writeChan chan Goodbye >> return state

  Goodbye -> UI.halt state

  Event (UI.EvKey UI.KEsc []) -> UI.halt state

  Event (UI.EvKey UI.KEnter []) -> case currentEditor of
    PasswordEditor -> do
      let sourceType = viewType musicSource
      let [userName] = UI.getEditContents userNameEditor
      let [password] = case sourceType of
             NetEaseMusic -> NetEase.encryptPassword <$> UI.getEditContents passwordEditor
      session <- case sourceType of
        NetEaseMusic -> do
          session <- NetEase.initSession True
          liftIO $ runSessionOnly session (NetEase.login userName password)
          return session
      liftIO $ writeLoginConfig sourceType (userName, password)
      insertSession sessionManager sourceType session
      UI.suspendAndResume $ continuation session >> writeChan chan Goodbye >> return state
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

loginCont :: String -> MusicSource -> SessionManager -> (SomeSession -> IO ()) -> IO ()
loginCont title source manager continuation = do
  let editor1 = UI.editor (editorName UserNameEditor) (UI.str . unlines) Nothing []
  let editor2 = UI.editor (editorName PasswordEditor) (\[s] -> UI.str $ replicate (length s) '*') Nothing []
  chan <- newChan
  if requireLogin source
     then writeChan chan Hello
     else writeChan chan Hi
  void $ UI.customMain (UI.mkVty def) chan loginApp State { currentEditor = UserNameEditor
                                                          , userNameEditor = editor1
                                                          , passwordEditor = editor2
                                                          , musicSource = source
                                                          , sessionManager = manager
                                                          , onGreetings = True
                                                          , uiTitle = title
                                                          , chan = chan
                                                          , continuation = continuation
                                                          }

login :: String -> MusicSource -> SessionManager -> ContT () IO SomeSession
login title source manager = ContT (loginCont title source manager)
