{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FM.Session (
  -- * Session
  -- A session is either a NetEase FM session, or a cache session.
  MonadSession, runSession
, IsSession
, SomeSession (..)
, fromSession
) where

import Control.Monad.Reader
import Data.Maybe (fromJust)
import Data.Typeable (Typeable, cast)

newtype MonadSession s a = MonadSession (ReaderT s IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader s)

runSession :: (IsSession s) => SomeSession -> MonadSession s a -> IO a
runSession session (MonadSession m) = runReaderT m (fromSession session)

class Typeable s => IsSession s

data SomeSession = forall s. (IsSession s, Typeable s) => SomeSession s
  deriving (Typeable)

-- | Unwraps the session from 'SomeSession'.
fromSession :: (IsSession s, Typeable s) => SomeSession -> s
fromSession (SomeSession k) = fromJust $ cast k
