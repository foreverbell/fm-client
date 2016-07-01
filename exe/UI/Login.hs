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

import           Control.Concurrent.Chan (newChan, writeChan)
import           Control.Exception (SomeException, try)
import           Control.Monad (void)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Cont (ContT (..))
import           Data.Default.Class
import           Data.IORef
import           System.Directory (createDirectoryIfMissing, getHomeDirectory)

import qualified FM.Cache as Cache
import qualified FM.NetEase as NetEase
import           Types

getConfig :: (MonadIO m) => m FilePath
getConfig = liftIO $ do
  dir <- (++ "/.fm") <$> getHomeDirectory
  createDirectoryIfMissing True dir
  return $ dir ++ "/login.conf"

readLoginConfig :: (MonadIO m) => m (String, String)
readLoginConfig = liftIO $ do
  conf <- getConfig
  [userName, password] <- take 2 . lines <$> readFile conf
  return (userName, password)

writeLoginConfig :: (MonadIO m) => (String, String) -> m ()
writeLoginConfig (userName, password) = liftIO $ do
  conf <- getConfig
  writeFile conf (unlines [userName, password])

data Event = Event UI.Event | Hi | Hello | Goodbye

data EditorType = UserNameEditor | PasswordEditor
  deriving (Show)

type NetEaseSavedSession = IORef (Maybe SomeSession)

data State = State {
  currentEditor  :: EditorType
, userNameEditor :: UI.Editor
, passwordEditor :: UI.Editor
, musicSource    :: MusicSource
, netEaseSession :: NetEaseSavedSession
, cache          :: Cache
, onGreetings    :: Bool
, postEvent      :: Event -> IO ()
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
            else UI.vBox [ UI.mkYellow $ UI.hCenter $ UI.str "网易通行证登陆"
                         , UI.separator
                         , UI.hCenter $ UI.str "账号: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor userNameEditor)
                         , UI.hCenter $ UI.str "密码: " <+> UI.hLimit 15 (UI.vLimit 1 $ UI.renderEditor passwordEditor)
                         ]

loginEvent :: State -> Event -> UI.EventM (UI.Next State)
loginEvent state@State {..} event = case event of
  Hi -> do
    session <- if isLocal musicSource
      then Cache.initSession cache
      else NetEase.initSession True
    UI.suspendAndResume $ continuation session >> postEvent Goodbye >> return state

  Hello -> do
    session <- liftIO $ try $ do
      mSession <- readIORef netEaseSession
      case mSession of
        Just session -> return session
        Nothing -> do
          (userName, password) <- readLoginConfig
          session <- NetEase.initSession True
          runSession session (NetEase.login userName password)
          writeIORef netEaseSession (Just session)
          return session
    case session of
      Left (_ :: SomeException) -> liftIO (writeIORef netEaseSession Nothing) >> UI.continue state { onGreetings = False }
      Right session -> UI.suspendAndResume $ continuation session >> postEvent Goodbye >> return state

  Goodbye -> UI.halt state

  Event (UI.EvKey UI.KEsc []) -> UI.halt state

  Event (UI.EvKey UI.KEnter []) -> case currentEditor of
    PasswordEditor -> do
      let [userName] = UI.getEditContents userNameEditor
      let [password] = NetEase.encryptPassword <$> UI.getEditContents passwordEditor
      session <- NetEase.initSession True
      liftIO $ do
        runSession session (NetEase.login userName password)
        writeLoginConfig (userName, password)
        writeIORef netEaseSession (Just session)
      UI.suspendAndResume $ continuation session >> postEvent Goodbye >> return state
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

loginCont :: MusicSource -> NetEaseSavedSession -> Cache -> (SomeSession -> IO ()) -> IO ()
loginCont source netEaseSession cache continuation = void $ do
  let editor1 = UI.editor (editorName UserNameEditor) (UI.str . unlines) Nothing []
  let editor2 = UI.editor (editorName PasswordEditor) (\[s] -> UI.str $ replicate (length s) '*') Nothing []
  chan <- newChan
  let postEvent = writeChan chan
  postEvent $ if requireLogin source then Hello else Hi
  UI.customMain (UI.mkVty def) chan loginApp State { currentEditor = UserNameEditor
                                                   , userNameEditor = editor1
                                                   , passwordEditor = editor2
                                                   , musicSource = source
                                                   , netEaseSession = netEaseSession
                                                   , cache = cache
                                                   , onGreetings = True
                                                   , postEvent = postEvent
                                                   , continuation = continuation
                                                   }

login :: MusicSource -> NetEaseSavedSession -> Cache -> ContT () IO SomeSession
login source netEaseSession cache = ContT (loginCont source netEaseSession cache)
