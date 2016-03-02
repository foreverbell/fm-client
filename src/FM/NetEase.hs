{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module FM.NetEase (
  initSession
, login
, fetchFM
, fetchRListAsFM
, star, unstar, trash
, fetchLyricsIO
) where

import           Control.Exception
import           Control.Monad.Catch (throwM)
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit)
import           Data.IORef
import           Data.List (find)
import           Data.Time.Clock (getCurrentTime)
import           Data.Typeable
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI as HTTP
import           System.Random (getStdRandom, randomR)

import           FM.FM
import qualified FM.Song as Song
import           FM.NetEase.JSON
import           FM.NetEase.Crypto

class IsQuery q where
  fromQuery :: q -> BS.ByteString

data EmptyQuery = EmptyQuery

instance IsQuery EmptyQuery where
  fromQuery EmptyQuery = BS.empty

emptyQuery :: EmptyQuery
emptyQuery = EmptyQuery

data Session = Session {
  sessionManager        :: HTTP.Manager
, sessionRequestHeaders :: HTTP.RequestHeaders
, sessionCookies        :: IORef HTTP.CookieJar
}

data HTTPMethod = Post | Get | PostCookies

data NetEaseException = NetEaseHTTPException HTTP.HttpException
                      | NetEaseStatusCodeException Int (HTTP.Response BS.ByteString)
                      | NetEaseParseException String
                      | NetEaseOtherExceptipon Int (Maybe String)
  deriving (Typeable, Show)

instance Exception NetEaseException

data ResponseMessage = ResponseMessage Int (Maybe String)

instance JSON.FromJSON ResponseMessage where
  parseJSON (JSON.Object v) = ResponseMessage <$> v .: "code" <*> v .:? "message"
  parseJSON _ = fail "invalid response"

{- TODO: Seems using IORef for cookies is a bit problematic, or fix it with MVar/STM ? -}
sendRequest :: (MonadIO m, IsQuery q) => Session -> HTTPMethod -> String -> q -> m BS.ByteString
sendRequest Session {..} method url query = liftIO $ case method of
    Get -> catch get (throwM . NetEaseHTTPException)
    Post -> catch (post False) (throwM . NetEaseHTTPException)
    PostCookies -> catch (post True) (throwM . NetEaseHTTPException)
  where
    post saveCookies = do
      initialRequest <- liftIO $ HTTP.parseUrl url
      cookies <- readIORef sessionCookies
      let request = initialRequest { HTTP.method = "POST"
                                   , HTTP.requestHeaders = sessionRequestHeaders
                                   , HTTP.cookieJar = Just cookies
                                   , HTTP.requestBody = HTTP.RequestBodyBS $ fromQuery query
                                   }
      send saveCookies request

    get = do
      let action = mconcat [ url, "?", BS8.unpack $ fromQuery query ]
      initialRequest <- liftIO $ HTTP.parseUrl action
      cookies <- readIORef sessionCookies
      let request = initialRequest { HTTP.method = "GET"
                                   , HTTP.requestHeaders = sessionRequestHeaders
                                   , HTTP.cookieJar = Just cookies
                                   }
      send False request

    send saveCookies request = do
      response <- fmap BL.toStrict <$> liftIO (HTTP.httpLbs request sessionManager)
      let HTTP.Status {..} = HTTP.responseStatus response
      case statusCode of
        200 -> do
          when saveCookies $ liftIO $ do
            now <- getCurrentTime
            let updateCookieJar = fst . HTTP.updateCookieJar response request now
            modifyIORef' sessionCookies updateCookieJar
          let body = HTTP.responseBody response
          case JSON.eitherDecode (BL.fromStrict body) of
            Right (ResponseMessage 200 _) -> return body
            Right (ResponseMessage rc m) -> throwM $ NetEaseOtherExceptipon rc m
            Left err -> throwM $ NetEaseParseException err
        _ -> liftIO $ throwM (NetEaseStatusCodeException statusCode response)

initSession :: (MonadIO m) => m Session
initSession = do
  sessionManager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  let sessionRequestHeaders = [ (HTTP.hAccept, "*/*")
                              , (HTTP.hAcceptEncoding, "gzip,deflate,sdch")
                              , (HTTP.hAcceptLanguage, "zh-CN,zh;q=0.8,gl;q=0.6,zh-TW;q=0.4")
                              , (HTTP.hConnection, "keep-alive")
                              , (HTTP.hContentType, "application/x-www-form-urlencoded")
                              , (HTTP.hHost, "music.163.com")
                              , (HTTP.hReferer, "http://music.163.com/search/")
                              ]
  sessionCookies <- liftIO $ newIORef (HTTP.createCookieJar [])
  return Session {..}

data EncryptedData = EncryptedData {
  encryptedText :: BS.ByteString
, encryptedSecretKey :: BS.ByteString
}

instance IsQuery EncryptedData where
  fromQuery (EncryptedData text secretKey) = HTTP.renderSimpleQuery False [ ("params", text), ("encSecKey", secretKey) ]

createSecretKey :: (MonadIO m) => Int -> m BS.ByteString
createSecretKey n = mconcat <$> replicateM n (toHex <$> liftIO (getStdRandom $ randomR (0 :: Int, 15)))

createEncryptedText :: (MonadIO m) => BS.ByteString -> m EncryptedData
createEncryptedText text = do
  secretKey <- createSecretKey 16
  return EncryptedData { encryptedText = encryptAES text secretKey, encryptedSecretKey = encryptRSA secretKey }

createEncryptedLogin :: (MonadIO m) => BS.ByteString -> BS.ByteString -> m EncryptedData
createEncryptedLogin username password = createEncryptedText $ encodeJSON $ JSON.object 
  [ ("username", JSON.toJSON username)
  , ("password", JSON.toJSON password)
  , ("rememberLogin", JSON.toJSON True)
  ]

createEncryptedPhoneLogin :: (MonadIO m) => BS.ByteString -> BS.ByteString -> m EncryptedData
createEncryptedPhoneLogin phone password = createEncryptedText $ encodeJSON $ JSON.object 
  [ ("phone", JSON.toJSON phone)
  , ("password", JSON.toJSON password)
  , ("rememberLogin", JSON.toJSON True)
  ]

data FMOperation = Star Song.SongId 
                 | Unstar Song.SongId
                 | Trash Song.SongId
                 | FetchLyrics Song.SongId

instance IsQuery FMOperation where
  fromQuery (Star id) = HTTP.renderSimpleQuery False 
    [ ("alg", "itembased"), ("trackId", BS8.pack (show id)), ("like", "true"), ("time", "25") ]

  fromQuery (Unstar id) = HTTP.renderSimpleQuery False 
    [ ("alg", "itembased"), ("trackId", BS8.pack (show id)), ("like", "false"), ("time", "25") ]

  fromQuery (Trash id) = HTTP.renderSimpleQuery False 
    [ ("alg", "RT"), ("songId", BS8.pack (show id)), ("time", "25") ]

  fromQuery (FetchLyrics id) = HTTP.renderSimpleQuery False 
    [ ("os", "osx"), ("id", BS8.pack (show id)), ("lv", "-1"), ("kv", "-1"), ("tv", "-1") ]

login :: (MonadIO m, MonadReader Session m) => String -> String -> m ()
login username password = do
  session <- ask
  let isPhone = all isDigit username
  let (loginMethod, loginAction) | isPhone = (createEncryptedPhoneLogin, "http://music.163.com/weapi/login/cellphone")
                                 | otherwise = (createEncryptedLogin, "http://music.163.com/weapi/login")
  loginMethod (BS8.pack username) (encryptPassword $ BS8.pack password) >>= void . sendRequest session PostCookies loginAction

fetchFM :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchFM = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/radio/get" emptyQuery
  case decodeFM body of
    Left err -> liftIO $ throwM $ NetEaseParseException err
    Right fm -> return fm

fetchRListAsFM :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchRListAsFM = do
  session@Session {..} <- ask
  cookies <- liftIO $ HTTP.destroyCookieJar <$> readIORef sessionCookies
  case find (\HTTP.Cookie {..} -> cookie_name == "__csrf") cookies of
    Just HTTP.Cookie {..} -> do
      let csrf = BS8.unpack cookie_value
      let url = "http://music.163.com/weapi/v1/discovery/recommend/songs?csrf_token=" ++ csrf
      request <- createEncryptedText $ encodeJSON $ JSON.object 
        [ ("offset", JSON.toJSON (0 :: Int))
        , ("total", JSON.toJSON True)
        , ("limit", JSON.toJSON (20 :: Int))
        , ("csrf_token", JSON.toJSON csrf)
        ]
      body <- sendRequest session Post url request
      case decodeRList body of
        Left err -> liftIO $ throwM $ NetEaseParseException err
        Right fm -> return fm
    Nothing -> return []

-- | TODO: test star, unstar, trash
star :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Song
star song@Song.Song {..} = do
  session <- ask
  sendRequest session Get "http://music.163.com/api/radio/like" (Star uid)
  return song { Song.starred = True }

unstar :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Song
unstar song@Song.Song {..} = do
  session <- ask
  sendRequest session Get "http://music.163.com/api/radio/like" (Unstar uid)
  return song { Song.starred = False }

trash :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Song
trash song@Song.Song {..} = do
  session <- ask
  sendRequest session Get "http://music.163.com/api/radio/trash/add" (Trash uid)
  return song { Song.starred = False }

fetchLyricsIO :: Session -> Song.Song -> IO (Maybe Song.Lyrics)
fetchLyricsIO session Song.Song {..} = do
  body <- sendRequest session Get "http://music.163.com/api/song/lyric" (FetchLyrics uid)
  case decodeLyrics body of
    Right lyrics -> return $ Just lyrics
    Left _ -> return Nothing
