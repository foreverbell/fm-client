{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module FM.NetEase (
  initSession
, login
, encryptPassword
, fetchFM
, fetchRecommend
, fetchPlayLists
, fetchPlayList
, fetchUrl
, fetchLyrics
) where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Catch (throwM)
import qualified Data.Aeson as JSON
import           Data.Aeson ((.:), (.:?))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import           Data.Char (isDigit)
import           Data.Default.Class
import           Data.IORef
import           Data.List (find)
import           Data.Time.Clock (getCurrentTime)
import           Data.Typeable
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as HTTP
import qualified Network.HTTP.Types.Header as HTTP
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.HTTP.Types.URI as HTTP
import           System.Random (getStdRandom, randomR)

import           FM.Session
import qualified FM.Song as Song
import           FM.NetEase.JSON
import           FM.NetEase.Crypto

class IsQuery q where
  fromQuery :: q -> BS.ByteString

instance IsQuery () where
  fromQuery () = BS.empty

data Session = Session {
  sessionManager :: HTTP.Manager
, sessionUserId  :: IORef Int
, sessionCsrf    :: IORef String
, sessionSecure  :: Bool
, sessionCookies :: IORef HTTP.CookieJar
} deriving (Typeable)
instance IsSession Session

data HTTPMethod = Post | Get | PostAndSaveCookies

data NetEaseException = NetEaseHTTPException HTTP.HttpException
                      | NetEaseStatusCodeException Int (HTTP.Response String)
                      | NetEaseParseException String
                      | NetEaseOtherExceptipon Int (Maybe String)
  deriving (Typeable, Show)
instance Exception NetEaseException

data ResponseMessage = ResponseMessage Int (Maybe String)

instance JSON.FromJSON ResponseMessage where
  parseJSON (JSON.Object v) = ResponseMessage <$> v .: "code" <*> v .:? "message"
  parseJSON _ = fail "invalid response"

validateJSON :: (MonadIO m) => Either String a -> (a -> m b) -> m b
validateJSON r f = case r of
  Right x -> f x
  Left err -> liftIO $ throwM $ NetEaseParseException err

sendRequest :: (MonadIO m, IsQuery q) => Session
                                      -> HTTPMethod
                                      -> String
                                      -> q
                                      -> m BS.ByteString
sendRequest Session {..} method url query = liftIO $ case method of
    Get -> catch get (throwM . NetEaseHTTPException)
    Post -> catch (post False) (throwM . NetEaseHTTPException)
    PostAndSaveCookies -> catch (post True) (throwM . NetEaseHTTPException)
  where
    initRequest request = do
      cookies <- liftIO $ readIORef sessionCookies
      return request
        { HTTP.requestHeaders =
            [ (HTTP.hAccept, "*/*")
            , (HTTP.hAcceptLanguage, "zh-CN,zh;q=0.8,gl;q=0.6,zh-TW;q=0.4")
            , (HTTP.hConnection, "keep-alive")
            , (HTTP.hContentType, "application/x-www-form-urlencoded")
            , (HTTP.hHost, "music.163.com")
            , (HTTP.hReferer, "http://music.163.com")
            , (HTTP.hUserAgent, "Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Win64; x64; Trident/6.0)")
            ]
        , HTTP.cookieJar = Just cookies
        , HTTP.secure = sessionSecure
        , HTTP.port = if sessionSecure then 443 else 80
        }

    post saveCookies = do
      initialRequest <- initRequest =<< HTTP.parseRequest url
      send saveCookies $ initialRequest
        { HTTP.method = "POST"
        , HTTP.requestBody = HTTP.RequestBodyBS $ fromQuery query
        }

    get = do
      let action = mconcat [ url, "?", BS8.unpack $ fromQuery query ]
      initialRequest <- initRequest =<< HTTP.parseRequest action
      send False $ initialRequest { HTTP.method = "GET" }

    send saveCookies request = do
      response <- fmap BL.toStrict <$> HTTP.httpLbs request sessionManager
      let HTTP.Status {..} = HTTP.responseStatus response
      case statusCode of
        200 -> do
          when saveCookies $ do
            now <- getCurrentTime
            let updateCookieJar = fst . HTTP.updateCookieJar response request now
            modifyIORef' sessionCookies updateCookieJar
          let body = HTTP.responseBody response
          validateJSON (JSON.eitherDecode (BL.fromStrict body)) $ \case
            ResponseMessage 200 _ -> return body
            ResponseMessage rc m -> throwM (NetEaseOtherExceptipon rc m)
        _ -> throwM (NetEaseStatusCodeException statusCode
                                                (BS8.unpack <$> response))

initSession :: (MonadIO m) => Bool -> m SomeSession
initSession sessionSecure = liftIO $ do
  let settings = if sessionSecure
                   then HTTP.tlsManagerSettings
                   else HTTP.defaultManagerSettings
  sessionManager <- HTTP.newManager settings
  sessionUserId <- newIORef 0
  sessionCsrf <- newIORef []
  sessionCookies <- newIORef (HTTP.createCookieJar [])
  return $ SomeSession Session {..}

data NetEaseQuery = FetchLyrics Song.SongId
                  | FetchPlayLists Int
                  | EncryptData BS.ByteString BS.ByteString

instance IsQuery NetEaseQuery where
  fromQuery (EncryptData text key) = HTTP.renderSimpleQuery False
    [ ("params", text), ("encSecKey", key) ]

  fromQuery (FetchLyrics id) = HTTP.renderSimpleQuery False
    [ ("os", "osx"), ("id", BS8.pack (show id))
    , ("lv", "-1"), ("kv", "-1"), ("tv", "-1") ]

  fromQuery (FetchPlayLists uid) = HTTP.renderSimpleQuery False
    [ ("offset", "0"), ("limit", "1000"), ("uid", BS8.pack (show uid)) ]

createSecretKey :: (MonadIO m) => Int -> m BS.ByteString
createSecretKey n = liftIO $
  mconcat <$> replicateM n (toHex <$> getStdRandom (randomR (0 :: Int, 15)))

encryptQuery :: (MonadIO m) => BS.ByteString -> m NetEaseQuery
encryptQuery q = do
  secretKey <- createSecretKey 16
  return $ EncryptData (encryptAES secretKey q) (encryptRSA secretKey)

login :: (MonadIO m, MonadReader Session m) => String -> String -> m ()
login userName password = do
  session@Session {..} <- ask
  let isPhone = length userName == 11 && all isDigit userName
  let loginURL | isPhone = "http://music.163.com/weapi/login/cellphone"
               | otherwise = "http://music.163.com/weapi/login"
  request <- encryptQuery $ encodeJSON $ JSON.object $ if isPhone
               then [ ("phone", JSON.toJSON $ BS8.pack userName)
                    , ("password", JSON.toJSON $ BS8.pack password)
                    , ("rememberLogin", JSON.toJSON False)
                    ]
               else [ ("username", JSON.toJSON $ BS8.pack userName)
                    , ("password", JSON.toJSON $ BS8.pack password)
                    , ("rememberLogin", JSON.toJSON False)
                    ]
  body <- sendRequest session PostAndSaveCookies loginURL request
  validateJSON (decodeUserId body) (liftIO . writeIORef sessionUserId)
  cookies <- liftIO $ HTTP.destroyCookieJar <$> readIORef sessionCookies
  let csrf = BS8.unpack . HTTP.cookie_value <$>
               find (\HTTP.Cookie {..} -> cookie_name == "__csrf") cookies
  case csrf of
    Just csrf -> liftIO $ writeIORef sessionCsrf csrf
    Nothing -> return ()

fetchFM :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchFM = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/radio/get" ()
  validateJSON (decodeFM body) return

fetchRecommend :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchRecommend = do
  session@Session {..} <- ask
  csrf <- liftIO $ readIORef sessionCsrf
  let url = "http://music.163.com/weapi/v1/discovery/recommend/songs?csrf_token=" ++ csrf
  request <- encryptQuery $ encodeJSON $ JSON.object
    [ ("offset", JSON.toJSON (0 :: Int))
    , ("total", JSON.toJSON True)
    , ("limit", JSON.toJSON (20 :: Int))
    , ("csrf_token", JSON.toJSON csrf)
    ]
  body <- sendRequest session Post url request
  validateJSON (decodeRecommend body) return

fetchPlayLists :: (MonadIO m, MonadReader Session m) => m [(Int, String)]
fetchPlayLists = do
  session@Session {..} <- ask
  userId <- liftIO $ readIORef sessionUserId
  if userId == 0
     then return []
     else do
       body <- sendRequest session Get "http://music.163.com/api/user/playlist" (FetchPlayLists userId)
       validateJSON (decodePlayLists body) return

fetchPlayList :: (MonadIO m, MonadReader Session m) => Int -> m [Song.Song]
fetchPlayList id = do
  session@Session {..} <- ask
  csrf <- liftIO $ readIORef sessionCsrf
  let url = "http://music.163.com/weapi/v3/playlist/detail?csrf_token=" ++ csrf
  request <- encryptQuery $ encodeJSON $ JSON.object
    [ ("id", JSON.toJSON id)
    , ("n", JSON.toJSON (1000 :: Int))
    , ("total", JSON.toJSON True)
    , ("csrf_token", JSON.toJSON csrf)
    ]
  body <- sendRequest session Post url request
  validateJSON (decodePlayList body) return

fetchUrl :: (MonadIO m, MonadReader Session m) => Song.Song -> m (Maybe String)
fetchUrl Song.Song {..} = do
  session@Session {..} <- ask
  csrf <- liftIO $ readIORef sessionCsrf
  let url = "http://music.163.com/weapi/song/enhance/player/url?csrf_token=" ++ csrf
  request <- encryptQuery $ encodeJSON $ JSON.object
    [ ("ids", JSON.toJSON [uid])
    , ("br", JSON.toJSON (320000 :: Int))
    , ("csrf_token", JSON.toJSON csrf)
    ]
  body <- sendRequest session Post url request
  case decodeUrl body of
    Right [url] -> return (Just url)
    _ -> return Nothing

fetchLyrics :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Lyrics
fetchLyrics Song.Song {..} = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/song/lyric" (FetchLyrics uid)
  return $ either (const def) Song.parseLyrics (decodeLyrics body)
