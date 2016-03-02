{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module FM.NetEase.JSON (
  encodeJSON
, decodeFM
, decodeRList
, decodeLyrics
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
newtype RList = RList [Song.Song]

instance JSON.FromJSON FM where
  parseJSON (JSON.Object v) = FM <$> (v .: "data" >>= parseSongList)
  parseJSON _ = mzero

instance JSON.FromJSON RList where
  parseJSON (JSON.Object v) = RList <$> (v .: "recommend" >>= parseSongList)
  parseJSON _ = mzero

parseSongList :: JSON.Value -> JSON.Parser [Song.Song]
parseSongList (JSON.Array v) = mapM parseSong (V.toList v)
parseSongList _ = mzero

parseSong :: JSON.Value -> JSON.Parser Song.Song
parseSong (JSON.Object v) = do
  starred <- v .: "starred"
  mp3URL <- v .: "mp3Url"
  title <- v .: "name"
  uid <- v .: "id"
  titleAliases <- parseTitleAliases =<< (v .: "alias")
  artists <- parseArtists =<< (v .: "artists")
  album <- parseAlbum =<< (v .: "album")
  return Song.Song {..}
parseSong _ = mzero

parseTitleAliases :: JSON.Value -> JSON.Parser [String]
parseTitleAliases (JSON.Array v) = mapM parseOne (V.toList v)
  where
    parseOne (JSON.String t) = return $ T.unpack t
    parseOne _ = mzero
parseTitleAliases _ = mzero

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

decodeRList :: BS.ByteString -> Either String [Song.Song]
decodeRList bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (RList xs) = xs

newtype Lyrics = Lyrics Song.Lyrics

instance JSON.FromJSON Lyrics where
  parseJSON (JSON.Object v) = v .: "lrc" >>= parse
    where
      parse (JSON.Object v) = Lyrics . Song.Lyrics . lines . T.unpack <$> (v .: "lyric")
      parse _ = mzero
  parseJSON _ = mzero

decodeLyrics :: BS.ByteString -> Either String Song.Lyrics
decodeLyrics bs = unwrap <$> JSON.eitherDecode (BL.fromStrict bs)
  where unwrap (Lyrics lyrics) = lyrics
