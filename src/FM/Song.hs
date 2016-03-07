{-# LANGUAGE RecordWildCards #-}

module FM.Song ( 
  SongId
, Song (..)
, Lyrics (..)
) where

import Data.List (intercalate)
import Data.Default.Class

type SongId = Int

data Song = Song {
  uid :: SongId
, title :: String
, artists :: [String]
, album :: String
, url :: String
, starred :: Bool
}

newtype Lyrics = Lyrics [String]

instance Default Lyrics where
  def = Lyrics [ "[00:00.00]No lyrics." ]

instance Show Song where
  show Song {..} = unlines [ "Song {"
                           , "  uid = " ++ show uid ++ ","
                           , "  title = " ++ title ++ ","
                           , "  artists = " ++ dumpList artists ++ ","
                           , "  album = " ++ album ++ ","
                           , "  url = " ++ url ++ ","
                           , "  starred = " ++ show starred
                           , "}"
                           ]
    where
      dumpList :: [String] -> String
      dumpList [] = "(null)"
      dumpList xs = intercalate " / " xs

instance Show Lyrics where
  show (Lyrics lyrics) = unlines [ "Lyrics {", unlines $ map ("  " ++) lyrics, "}" ]
