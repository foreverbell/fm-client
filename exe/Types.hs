module Types (
  Show1 (..)
, MusicSource (..)
, MusicSourceType (..)
, viewType
, requireLogin
, module FM.Session
) where

import FM.Session

class Show1 a where
  show1 :: a -> String

data MusicSource = NetEaseFM
                 | NetEasePublicFM
                 | NetEaseDailyRecommendation
                 | NetEasePlayLists
                 | NetEasePlayList Int String
  deriving (Eq, Ord)

data MusicSourceType = NetEaseMusic
  deriving (Eq, Ord)

instance Show1 MusicSource where
  show1 NetEaseFM = "NetEase Cloud Music FM"
  show1 NetEasePublicFM = "NetEase Cloud Music FM (Public)"
  show1 NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"
  show1 NetEasePlayLists = "NetEase Cloud Music Play List"
  show1 (NetEasePlayList _ title) = title

instance Show1 MusicSourceType where
  show1 NetEaseMusic = "net-ease"

viewType :: MusicSource -> MusicSourceType
viewType _ = NetEaseMusic

requireLogin :: MusicSource -> Bool
requireLogin NetEasePublicFM = False
requireLogin _ = True
