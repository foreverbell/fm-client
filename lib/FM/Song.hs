{-# LANGUAGE RecordWildCards #-}

module FM.Song ( 
  SongId
, Song (..)
, Lyrics (..)
, parseLyrics
) where

import Data.Default.Class
import Data.List (sort)
import Text.Parsec

type SongId = Int

data Song = Song {
  uid     :: SongId
, title   :: String
, artists :: [String]
, album   :: String
, url     :: String
}

newtype Lyrics = Lyrics [(Double, String)]
  deriving (Show, Read)

{-

instance Show Song where
  show Song {..} = unlines [ "Song {"
                           , "  uid = " ++ show uid ++ ","
                           , "  title = " ++ title ++ ","
                           , "  artists = " ++ dumpList artists ++ ","
                           , "  album = " ++ album ++ ","
                           , "  url = " ++ url ++ ","
                           , "}"
                           ]
    where
      dumpList :: [String] -> String
      dumpList [] = "(null)"
      dumpList xs = intercalate " / " xs

instance Show Lyrics where
  show (Lyrics lyrics) = unlines [ "Lyrics {", unlines $ map (\(t, l) -> "  [" ++ show t ++ "]" ++ l) lyrics, "}" ]

-}

instance Default Lyrics where
  def = Lyrics [ (0, "No Lyrics") ]

type Parser = Parsec String ()

parseLyrics :: [String] -> Lyrics
parseLyrics = Lyrics . sort . concatMap (either (const []) id . runParser parser () [])
  where
    number :: Parser Double
    number = read <$> do
      a <- many1 digit
      b <- option [] $ try $ do
        char '.'
        ('.' :) <$> many1 digit
      return (a ++ b)

    parser :: Parser [(Double, String)]
    parser = do
      times <- many1 $ between (char '[') (char ']') $ do
        mm <- number
        char ':'
        ss <- number
        return $ mm * 60 + ss
      body <- manyTill anyToken eof 
      return $ zip times $ repeat body
