{-# LANGUAGE ExistentialQuantification #-}

module FM.Session ( 
  IsSession
, SomeSession (..)
, fromSession
) where

import Data.Maybe (fromJust)
import Data.Typeable (Typeable, cast)

class Typeable s => IsSession s

instance IsSession ()

data SomeSession = forall s. (IsSession s, Typeable s) => SomeSession s
  deriving (Typeable)

fromSession :: (IsSession s, Typeable s) => SomeSession -> s
fromSession (SomeSession k) = fromJust $ cast k
