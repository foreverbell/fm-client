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
, star
, unstar
, trash
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

checkJSON :: (MonadIO m) => Either String a -> (a -> m b) -> m b
checkJSON r f = case r of
  Right x -> f x
  Left err -> liftIO $ throwM $ NetEaseParseException err

sendRequest :: (MonadIO m, IsQuery q) => Session -> HTTPMethod -> String -> q -> m BS.ByteString
sendRequest Session {..} method url query = liftIO $ case method of
    Get -> catch get (throwM . NetEaseHTTPException)
    Post -> catch (post False) (throwM . NetEaseHTTPException)
    PostAndSaveCookies -> catch (post True) (throwM . NetEaseHTTPException)
  where
    initRequest request = do
      cookies <- liftIO $ readIORef sessionCookies
      return request { HTTP.requestHeaders = [ (HTTP.hAccept, "*/*")
                                             , (HTTP.hAcceptEncoding, "gzip,deflate,sdch")
                                             , (HTTP.hAcceptLanguage, "zh-CN,zh;q=0.8,gl;q=0.6,zh-TW;q=0.4")
                                             , (HTTP.hConnection, "keep-alive")
                                             , (HTTP.hContentType, "application/x-www-form-urlencoded")
                                             , (HTTP.hHost, "music.163.com")
                                             , (HTTP.hReferer, "http://music.163.com/search/")
                                             ]
                     , HTTP.cookieJar = Just cookies
                     , HTTP.secure = sessionSecure
                     , HTTP.port = if sessionSecure then 443 else 80
                     }

    post saveCookies = do
      initialRequest <- initRequest =<< HTTP.parseUrl url
      send saveCookies $ initialRequest { HTTP.method = "POST"
                                        , HTTP.requestBody = HTTP.RequestBodyBS $ fromQuery query
                                        }

    get = do
      let action = mconcat [ url, "?", BS8.unpack $ fromQuery query ]
      initialRequest <- initRequest =<< HTTP.parseUrl action
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
          checkJSON (JSON.eitherDecode (BL.fromStrict body)) $ \case
            ResponseMessage 200 _ -> return body
            ResponseMessage rc m -> throwM (NetEaseOtherExceptipon rc m)
        _ -> throwM (NetEaseStatusCodeException statusCode (BS8.unpack <$> response))

initSession :: (MonadIO m) => Bool -> m SomeSession
initSession sessionSecure = do
  sessionManager <- liftIO $ HTTP.newManager (if sessionSecure then HTTP.tlsManagerSettings else HTTP.defaultManagerSettings)
  sessionUserId <- liftIO $ newIORef 0
  sessionCookies <- liftIO $ newIORef (HTTP.createCookieJar [])
  return $ SomeSession Session {..}

data NetEaseQuery = Star Song.SongId 
                  | Unstar Song.SongId
                  | Trash Song.SongId
                  | FetchLyrics Song.SongId
                  | FetchPlayLists Int
                  | FetchPlayList Int
                  | EncryptData BS.ByteString BS.ByteString

instance IsQuery NetEaseQuery where
  fromQuery (EncryptData text key) = HTTP.renderSimpleQuery False 
    [ ("params", text), ("encSecKey", key) ]

  fromQuery (Star id) = HTTP.renderSimpleQuery False 
    [ ("alg", "itembased"), ("trackId", BS8.pack (show id)), ("like", "true"), ("time", "25") ]

  fromQuery (Unstar id) = HTTP.renderSimpleQuery False 
    [ ("alg", "itembased"), ("trackId", BS8.pack (show id)), ("like", "false"), ("time", "25") ]

  fromQuery (Trash id) = HTTP.renderSimpleQuery False 
    [ ("alg", "RT"), ("songId", BS8.pack (show id)), ("time", "25") ]

  fromQuery (FetchLyrics id) = HTTP.renderSimpleQuery False 
    [ ("os", "osx"), ("id", BS8.pack (show id)), ("lv", "-1"), ("kv", "-1"), ("tv", "-1") ]

  fromQuery (FetchPlayLists uid) = HTTP.renderSimpleQuery False 
    [ ("offset", "0"), ("limit", "100"), ("uid", BS8.pack (show uid)) ]

  fromQuery (FetchPlayList id) = HTTP.renderSimpleQuery False 
    [ ("id", BS8.pack (show id)) ]

createSecretKey :: (MonadIO m) => Int -> m BS.ByteString
createSecretKey n = mconcat <$> replicateM n (toHex <$> liftIO (getStdRandom $ randomR (0 :: Int, 15)))

encryptQuery :: (MonadIO m) => BS.ByteString -> m NetEaseQuery
encryptQuery q = do
  secretKey <- createSecretKey 16
  return $ EncryptData (encryptAES secretKey q) (encryptRSA secretKey)

login :: (MonadIO m, MonadReader Session m) => String -> String -> m ()
login userName password = do
  session <- ask
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
  liftIO $ checkJSON (decodeUserId body) (writeIORef (sessionUserId session))

fetchFM :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchFM = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/radio/get" ()
  liftIO $ checkJSON (decodeFM body) return

fetchRecommend :: (MonadIO m, MonadReader Session m) => m [Song.Song]
fetchRecommend = do
  session@Session {..} <- ask
  cookies <- liftIO $ HTTP.destroyCookieJar <$> readIORef sessionCookies
  case find (\HTTP.Cookie {..} -> cookie_name == "__csrf") cookies of
    Just HTTP.Cookie {..} -> do
      let csrf = BS8.unpack cookie_value
      let url = "http://music.163.com/weapi/v1/discovery/recommend/songs?csrf_token=" ++ csrf
      request <- encryptQuery $ encodeJSON $ JSON.object 
        [ ("offset", JSON.toJSON (0 :: Int))
        , ("total", JSON.toJSON True)
        , ("limit", JSON.toJSON (20 :: Int))
        , ("csrf_token", JSON.toJSON csrf)
        ]
      body <- sendRequest session Post url request
      liftIO $ checkJSON (decodeRecommend body) return
    Nothing -> return []

fetchPlayLists :: (MonadIO m, MonadReader Session m) => m [(Int, String)]
fetchPlayLists = do
  session@Session {..} <- ask
  userId <- liftIO $ readIORef sessionUserId
  if userId == 0
     then return []
     else do
       body <- sendRequest session Get "http://music.163.com/api/user/playlist" (FetchPlayLists userId)
       liftIO $ checkJSON (decodePlayLists body) return

fetchPlayList :: (MonadIO m, MonadReader Session m) => Int -> m [Song.Song]
fetchPlayList id = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/playlist/detail" (FetchPlayList id)
  liftIO $ checkJSON (decodePlayList body) return

-- | TODO: test star, unstar, trash
star :: (MonadIO m, MonadReader Session m) => Song.Song -> m ()
star Song.Song {..} = do
  session <- ask
  void $ sendRequest session Get "http://music.163.com/api/radio/like" (Star uid)

unstar :: (MonadIO m, MonadReader Session m) => Song.Song -> m ()
unstar Song.Song {..} = do
  session <- ask
  void $ sendRequest session Get "http://music.163.com/api/radio/like" (Unstar uid)

trash :: (MonadIO m, MonadReader Session m) => Song.Song -> m ()
trash Song.Song {..} = do
  session <- ask
  void $ sendRequest session Get "http://music.163.com/api/radio/trash/add" (Trash uid)

fetchLyrics :: (MonadIO m, MonadReader Session m) => Song.Song -> m Song.Lyrics
fetchLyrics Song.Song {..} = do
  session <- ask
  body <- sendRequest session Get "http://music.163.com/api/song/lyric" (FetchLyrics uid)
  return $ either (const def) Song.parseLyrics (decodeLyrics body)
