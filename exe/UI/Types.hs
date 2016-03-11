module UI.Types (
  MusicSource (..)
, MusicSourceType (..)
, viewMusicSource
, module FM.Session
) where

import FM.Session

data MusicSource = NetEaseFM
                 | NetEasePublicFM
                 | NetEaseDailyRecommendation
                 | NetEasePlayLists
                 | NetEasePlayList Int String
  deriving (Eq)

data MusicSourceType = NetEaseMusic

instance Show MusicSource where
  show NetEaseFM = "NetEase Cloud Music FM"
  show NetEasePublicFM = "NetEase Cloud Music FM (Public)"
  show NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"
  show NetEasePlayLists = "NetEase Cloud Music Play List"
  show (NetEasePlayList _ title) = title

viewMusicSource :: MusicSource -> MusicSourceType
viewMusicSource _ = NetEaseMusic
