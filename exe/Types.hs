module Types (
  Show1 (..)
, MusicSource (..)
, MusicSourceType (..)
, PlayMode (..)
, viewType
, requireLogin
, defaultPlayMode
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

data PlayMode = Stream | LoopOne | LoopAll | Shuffle
  deriving (Eq, Ord, Enum, Bounded)

instance Show1 MusicSource where
  show1 NetEaseFM = "NetEase Cloud Music FM"
  show1 NetEasePublicFM = "NetEase Cloud Music FM (Public)"
  show1 NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"
  show1 NetEasePlayLists = "NetEase Cloud Music Play List"
  show1 (NetEasePlayList _ title) = title

instance Show1 MusicSourceType where
  show1 NetEaseMusic = "NetEase"

instance Show1 PlayMode where
  show1 Stream  = "Stream"
  show1 LoopOne = "Loop One"
  show1 LoopAll = "Loop All"
  show1 Shuffle = "Random Shuffle"

viewType :: MusicSource -> MusicSourceType
viewType _ = NetEaseMusic

requireLogin :: MusicSource -> Bool
requireLogin NetEasePublicFM = False
requireLogin _ = True

defaultPlayMode :: MusicSource -> PlayMode
defaultPlayMode NetEaseFM = Stream
defaultPlayMode NetEasePublicFM = Stream
defaultPlayMode _ = LoopAll
