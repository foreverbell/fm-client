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
  show1 NetEaseFM = "网易云音乐私人兆赫"
  show1 NetEasePublicFM = "网易云音乐公共兆赫"
  show1 NetEaseDailyRecommendation = "网易云音乐每日歌曲推荐"
  show1 NetEasePlayLists = "网易云音乐歌单"
  show1 (NetEasePlayList _ title) = title

instance Show1 MusicSourceType where
  show1 NetEaseMusic = "NetEase"

instance Show1 PlayMode where
  show1 Stream  = "流"
  show1 LoopOne = "单曲循环"
  show1 LoopAll = "列表循环"
  show1 Shuffle = "随机播放"

viewType :: MusicSource -> MusicSourceType
viewType _ = NetEaseMusic

requireLogin :: MusicSource -> Bool
requireLogin NetEasePublicFM = False
requireLogin _ = True

defaultPlayMode :: MusicSource -> PlayMode
defaultPlayMode NetEaseFM = Stream
defaultPlayMode NetEasePublicFM = Stream
defaultPlayMode _ = LoopAll
