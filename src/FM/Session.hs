{-# LANGUAGE ExistentialQuantification #-}

module FM.Session ( 
  IsSession
, SomeSession (..)
) where

import Data.Typeable

class Typeable s => IsSession s

data SomeSession = forall s. (IsSession s) => SomeSession s
