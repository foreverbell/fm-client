{-# LANGUAGE OverloadedStrings #-}

module FM.NetEase.Crypto (
  toHex
, encryptAES
, encryptRSA
, encryptSongId
, encryptPassword
) where

import qualified Crypto.Error as C
import qualified Crypto.Cipher.AES as C
import qualified Crypto.Cipher.Types as C
import qualified Crypto.Hash as C
import           Data.Bits (xor)
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import           Data.Char (chr, ord)
import           Data.Maybe (fromJust)
import           Numeric (showHex)

toHex :: (Integral a, Show a) => a -> BS.ByteString
toHex n = BS8.pack (showHex n [])

encryptAES :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES key text = encrypt key (encrypt ("0CoJUm6Qyw8W8jud" :: BS.ByteString) text)
  where
    encrypt key text = Base64.encode $ C.cbcEncrypt cipher iv plain
      where
        cipher = fromJust $ C.maybeCryptoError (C.cipherInit key) :: C.AES128
        plain = text `mappend` BS.replicate k (toEnum k)
          where k = 16 - BS.length text `mod` 16
        iv = fromJust $ C.makeIV ("0102030405060708" :: BS.ByteString)

encryptRSA :: BS.ByteString -> BS.ByteString
encryptRSA text = zfill 256 $ toHex $ (base^publicKey) `mod` modulo
  where
    zfill n xs = BS.replicate (n - BS.length xs) (toEnum $ ord '0') `mappend` xs
    base = foldr ((\x y -> y * 256 + x) . toInteger) 0 (BS.unpack text)
    publicKey = 65537 :: Integer
    modulo = read $ concat [ "1577947502671315022124768178003"
                           , "4549812187278333338974742401153"
                           , "1025366277535262539913701806290"
                           , "7664791894775335978549896068031"
                           , "9425397866032994198078607243280"
                           , "6427833685472618792592200595694"
                           , "3468729513017705807651353492595"
                           , "9016749053613808246968063851441"
                           , "6594216629258349130257685001248"
                           , "172188325316586707301643237607"
                           ] :: Integer

encryptSongId :: String -> String
encryptSongId id = flip map hashValue $
  \c -> case c of
    '/' -> '_'
    '+' -> '-'
    c -> c
  where
    key = map ord "3go8&$8*3*3h0k(2)2"
    bytes = zipWith xor (map ord id) (concat (repeat key))
    hashValue = BS8.unpack $ Base64.encode $ BS.pack $ BA.unpack $ C.hashWith C.MD5 $ BS8.pack $ map chr bytes

encryptPassword :: String -> String
encryptPassword = show . C.hashWith C.MD5 . BS8.pack
