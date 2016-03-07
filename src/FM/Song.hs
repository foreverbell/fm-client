{-# LANGUAGE RecordWildCards #-}

module FM.Song ( 
  SongId
, Song (..)
, Lyrics (..)
, parseLyrics
) where

import Data.Default.Class
import Data.List (intercalate, sort)
import Text.Parsec

type SongId = Int

data Song = Song {
  uid     :: SongId
, title   :: String
, artists :: [String]
, album   :: String
, url     :: String
, starred :: Bool
}

newtype Lyrics = Lyrics [(Double, String)]

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
  show (Lyrics lyrics) = unlines [ "Lyrics {", unlines $ map (\(t, l) -> "  [" ++ show t ++ "]" ++ l) lyrics, "}" ]

instance Default Lyrics where
  def = Lyrics [ (0, "No lyrics.") ]

type Parser = Parsec String ()

parseLyrics :: [String] -> Lyrics
parseLyrics = Lyrics . sort . concatMap (either (const []) return . runParser parser () [])
  where
    parser :: Parser (Double, String)
    parser = do
      let number = read <$> many1 digit
      time <- between (char '[') (char ']') $ do
        mm <- fromInteger <$> number
        char ':'
        ss <- fromInteger <$> number
        char '.'
        xx <- fromInteger <$> number
        return $ mm * 60 + ss + xx / 100
      body <- manyTill anyToken eof 
      return (time, body)
