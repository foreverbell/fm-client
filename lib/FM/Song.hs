{-# LANGUAGE RecordWildCards #-}

module FM.Song (
  SongId
, Song (..)
, Lyrics (..)
, parseLyrics
) where

import Data.Default.Class
import Data.List (sort, intercalate)
import Text.Parsec

type SongId = Int

data Song = Song {
  uid     :: SongId         -- ^ Uniform id.
, title   :: String         -- ^ Title.
, artists :: [String]       -- ^ Artists list.
, album   :: String         -- ^ Album.
, url     :: Maybe String   -- ^ The URL of this song from NetEase cloud or
                            --   local. Nothing if we haven't retrieved the URL
                            --   from NetEase server.
}

newtype Lyrics = Lyrics [(Double, String)]

instance Show Song where
  show Song {..} = unlines [ "Song {"
                           , "  uid = " ++ show uid ++ ","
                           , "  title = " ++ title ++ ","
                           , "  artists = " ++ dumpList artists ++ ","
                           , "  album = " ++ album ++ ","
                           , "  url = " ++ show url ++ ","
                           , "}"
                           ]
    where
      dumpList :: [String] -> String
      dumpList [] = "(null)"
      dumpList xs = intercalate " / " xs

instance Show Lyrics where
  show (Lyrics lyrics) = unlines
    [ "Lyrics {"
    , unlines $ map (\(t, l) -> "  [" ++ show t ++ "]" ++ l) lyrics, "}"
    ]

instance Default Lyrics where
  def = Lyrics [ (0, "No Lyrics") ]

type Parser = Parsec String ()

-- | Parses lyrics from string.
parseLyrics :: [String] -> Lyrics
parseLyrics =
  Lyrics . sort . concatMap (either (const []) id . runParser parser () [])
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
