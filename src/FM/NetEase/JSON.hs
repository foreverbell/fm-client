{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module FM.NetEase.JSON (
  encodeJSON
, decodeFM
, decodeRecommend
, decodePlayList
, decodePlayLists
, decodeLyrics
, decodeUserId
) where

import           Control.Monad (mzero)
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:))
import qualified Data.Aeson.Encode as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector as V

import qualified FM.Song as Song

instance JSON.ToJSON BS.ByteString where
  toJSON = JSON.String . decodeUtf8

encodeJSON :: JSON.Value -> BS.ByteString
encodeJSON = BL.toStrict . BB.toLazyByteString . JSON.encodeToBuilder

newtype FM = FM [Song.Song]
newtype Recommend = Recommend [Song.Song]
newtype PlayList = PlayList [Song.Song]

instance JSON.FromJSON FM where
  parseJSON (JSON.Object v) = FM <$> (v .: "data" >>= parseSongList)
  parseJSON _ = mzero

instance JSON.FromJSON Recommend where
  parseJSON (JSON.Object v) = Recommend <$> (v .: "recommend" >>= parseSongList)
  parseJSON _ = mzero

instance JSON.FromJSON PlayList where
  parseJSON (JSON.Object v) = PlayList <$> (v .: "result" >>= parse)
    where
      parse (JSON.Object v) = v .: "tracks" >>= parseSongList
      parse _ = mzero
  parseJSON _ = mzero

parseSongList :: JSON.Value -> JSON.Parser [Song.Song]
parseSongList (JSON.Array v) = mapM parseSong (V.toList v)
parseSongList _ = mzero

parseSong :: JSON.Value -> JSON.Parser Song.Song
parseSong (JSON.Object v) = do
  starred <- v .: "starred"
  url <- v .: "mp3Url"
  title <- v .: "name"
  uid <- v .: "id"
  artists <- parseArtists =<< (v .: "artists")
  album <- parseAlbum =<< (v .: "album")
  return Song.Song {..}
parseSong _ = mzero

parseArtists :: JSON.Value -> JSON.Parser [String]
parseArtists (JSON.Array v) = mapM parseOne (V.toList v)
  where
    parseOne (JSON.Object v) = v .: "name"
    parseOne _ = mzero
parseArtists _ = mzero

parseAlbum :: JSON.Value -> JSON.Parser String
parseAlbum (JSON.Object v) = v .: "name"
parseAlbum _ = mzero

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
  parseJSON (JSON.Object v) = PlayLists <$> (v .: "playlist" >>= parse)
    where
      parse (JSON.Array v) = mapM parsePlayLists (V.toList v)
        where
          parsePlayLists (JSON.Object v) = do
            id <- v .: "id"
            name <- v .: "name"
            return (id, name)
          parsePlayLists _ = mzero
      parse _ = mzero
  parseJSON _ = mzero

decodePlayLists :: BS.ByteString -> Either String [(Int, String)]
decodePlayLists bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (PlayLists xs) = xs

newtype Lyrics = Lyrics [String]

instance JSON.FromJSON Lyrics where
  parseJSON (JSON.Object v) = v .: "lrc" >>= parse
    where
      parse (JSON.Object v) = Lyrics . lines . T.unpack <$> (v .: "lyric")
      parse _ = mzero
  parseJSON _ = mzero

decodeLyrics :: BS.ByteString -> Either String [String]
decodeLyrics bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (Lyrics lyrics) = lyrics

newtype UserId = UserId Int

instance JSON.FromJSON UserId where
  parseJSON (JSON.Object v) = v .: "account" >>= parse
    where
      parse (JSON.Object v) = UserId <$> (v .: "id")
      parse _ = mzero
  parseJSON _ = mzero

decodeUserId :: BS.ByteString -> Either String Int
decodeUserId bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (UserId id) = id
