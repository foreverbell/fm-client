{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module FM.NetEase.JSON (
  encodeJSON
, decodeFM
, decodeRecommend
, decodePlayList
, decodePlayLists
, decodeUrl
, decodeLyrics
, decodeUserId
) where

import           Control.Monad (forM)
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:))
import           Data.Aeson.Extra
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T

import qualified FM.Song as Song

newtype FM = FM [Song.Song]
newtype Recommend = Recommend [Song.Song]
newtype PlayList = PlayList [Song.Song]

data PlayListVersion = PlayListV1 | PlayListV3

instance JSON.FromJSON FM where
  parseJSON = onObject $ \v -> FM <$> (v .: "data" >>= parseSongList PlayListV1)

instance JSON.FromJSON Recommend where
  parseJSON = onObject $ \v ->  Recommend <$> (v .: "recommend" >>= parseSongList PlayListV1)

instance JSON.FromJSON PlayList where
  parseJSON = onObject $ \v -> PlayList <$> (v .: "playlist" >>= parse)
    where parse = onObject $ \v -> v .: "tracks" >>= parseSongList PlayListV3

parseSongList :: PlayListVersion -> JSON.Value -> JSON.Parser [Song.Song]
parseSongList ver = onArray $ \v -> mapM (parseSong ver) (V.toList v)

parseSong :: PlayListVersion -> JSON.Value -> JSON.Parser Song.Song
parseSong ver = onObject $ \v -> do
  let url = Nothing
  let (arKey, alKey) =
        case ver of
          PlayListV1 -> ("artists", "album")
          PlayListV3 -> ("ar", "al")
  title <- v .: "name"
  uid <- v .: "id"
  artists <- parseArtists =<< (v .: arKey)
  album <- parseAlbum =<< (v .: alKey)
  return Song.Song {..}

parseArtists :: JSON.Value -> JSON.Parser [String]
parseArtists = onArray $ \v -> forM (V.toList v) (onObject $ \v -> v .: "name")

parseAlbum :: JSON.Value -> JSON.Parser String
parseAlbum = onObject $ \v -> v .: "name"

decodeFM :: BS.ByteString -> Either String [Song.Song]
decodeFM bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (FM xs) = xs

decodeRecommend :: BS.ByteString -> Either String [Song.Song]
decodeRecommend bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (Recommend xs) = xs

decodePlayList :: BS.ByteString -> Either String [Song.Song]
decodePlayList bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (PlayList xs) = xs

newtype PlayLists = PlayLists [(Int, String)]

instance JSON.FromJSON PlayLists where
  parseJSON = onObject $ \v -> PlayLists <$> (v .: "playlist" >>= parse)
    where
      parse = onArray $ \v -> mapM parsePlayLists (V.toList v)
        where
          parsePlayLists = onObject $ \v -> do
            id <- v .: "id"
            name <- v .: "name"
            return (id, name)

decodePlayLists :: BS.ByteString -> Either String [(Int, String)]
decodePlayLists bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (PlayLists xs) = xs

newtype Url = Url [String]

instance JSON.FromJSON Url where
  parseJSON = onObject $ \v -> v .: "data" >>= parse
    where parse = onArray $ \v -> Url <$> mapM parse2 (V.toList v)
          parse2 = onObject $ \v -> v .: "url"

decodeUrl :: BS.ByteString -> Either String [String]
decodeUrl bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (Url url) = url

newtype Lyrics = Lyrics [String]

instance JSON.FromJSON Lyrics where
  parseJSON = onObject $ \v -> v .: "lrc" >>= parse
    where parse = onObject $ \v -> Lyrics . lines . T.unpack <$> (v .: "lyric")

decodeLyrics :: BS.ByteString -> Either String [String]
decodeLyrics bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (Lyrics lyrics) = lyrics

newtype UserId = UserId Int

instance JSON.FromJSON UserId where
  parseJSON = onObject $ \v -> v .: "account" >>= parse
    where parse = onObject $ \v -> UserId <$> (v .: "id")

decodeUserId :: BS.ByteString -> Either String Int
decodeUserId bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (UserId id) = id
