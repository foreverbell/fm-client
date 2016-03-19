module Types (
  Show1 (..)
, MusicSource (..)
, MusicSourceType (..)
, PlayOrder (..)
, viewType
, requireLogin
, defaultPlayOrder
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

data PlayOrder = Stream | LoopOne | LoopAll | Shuffle
  deriving (Eq, Ord, Enum, Bounded)

instance Show1 MusicSource where
  show1 NetEaseFM = "NetEase Cloud Music FM"
  show1 NetEasePublicFM = "NetEase Cloud Music FM (Public)"
  show1 NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"
  show1 NetEasePlayLists = "NetEase Cloud Music Play List"
  show1 (NetEasePlayList _ title) = title

instance Show1 MusicSourceType where
  show1 NetEaseMusic = "NetEase"

instance Show1 PlayOrder where
  show1 Stream = "Stream"
  show1 LoopOne = "Loop One"
  show1 LoopAll = "Loop All"
  show1 Shuffle = "Shuffle"

viewType :: MusicSource -> MusicSourceType
viewType _ = NetEaseMusic

requireLogin :: MusicSource -> Bool
requireLogin NetEasePublicFM = False
requireLogin _ = True

defaultPlayOrder :: MusicSource -> PlayOrder
defaultPlayOrder NetEaseFM = Stream
defaultPlayOrder NetEasePublicFM = Stream
defaultPlayOrder _ = LoopAll
