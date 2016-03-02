{-# LANGUAGE RecordWildCards #-}

module FM.Song ( 
  SongId
, Song (..)
, Lyrics (..)
) where

import Data.List (intercalate)

type SongId = Int

data Song = Song {
  uid :: SongId
, title :: String
, titleAliases :: [String]
, artists :: [String]
, album :: String
, mp3URL :: String
, starred :: Bool
}

newtype Lyrics = Lyrics [String]

instance Show Song where
  show Song {..} = unlines [ "Song {"
                           , "  uid = " ++ show uid ++ ","
                           , "  title = " ++ title ++ ","
                           , "  titleAliases = " ++ dumpList titleAliases ++ ","
                           , "  artists = " ++ dumpList artists ++ ","
                           , "  album = " ++ album ++ ","
                           , "  mp3URL = " ++ mp3URL ++ ","
                           , "  starred = " ++ show starred
                           , "}"
                           ]
    where
      dumpList :: [String] -> String
      dumpList [] = "(null)"
      dumpList xs = intercalate " / " xs

instance Show Lyrics where
  show (Lyrics lyrics) = unlines [ "Lyrics {", unlines $ map ("  " ++) lyrics, "}" ]
