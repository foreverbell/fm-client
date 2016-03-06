module UI.Types ( 
  MusicSource (..)
, module FM.Session
) where

import FM.Session

data MusicSource = NetEaseFM
                 | NetEasePublicFM
                 | NetEaseDailyRecommendation
  deriving (Enum, Bounded, Eq)

instance Show MusicSource where
  show NetEaseFM = "NetEase Cloud Music FM"
  show NetEasePublicFM = "NetEase Cloud Music FM (Public)"
  show NetEaseDailyRecommendation = "NetEase Cloud Music Daily Recommendation"
