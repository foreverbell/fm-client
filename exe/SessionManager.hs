module SessionManager ( 
  SessionManager
, newSessionManager
, insertSession
, deleteSession
, lookupSession
) where

import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.IORef
import qualified Data.Map as M

import           Types

data SessionManager = SessionManager (IORef (M.Map MusicSourceType SomeSession))

newSessionManager :: (MonadIO m) => m SessionManager
newSessionManager = liftIO $ SessionManager <$> newIORef M.empty

insertSession :: (MonadIO m) => SessionManager -> MusicSourceType -> SomeSession -> m ()
insertSession (SessionManager manager) source session = liftIO $ do
  map <- readIORef manager 
  writeIORef manager $ M.insert source session map

deleteSession :: (MonadIO m) => SessionManager -> MusicSourceType -> m ()
deleteSession (SessionManager manager) source = liftIO $ do
  map <- readIORef manager
  writeIORef manager $ M.delete source map

lookupSession :: (MonadIO m) => SessionManager -> MusicSourceType -> m (Maybe SomeSession)
lookupSession (SessionManager manager) source = liftIO $ M.lookup source <$> readIORef manager
