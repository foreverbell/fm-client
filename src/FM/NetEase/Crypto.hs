{-# LANGUAGE OverloadedStrings #-}

module FM.NetEase.Crypto ( 
  toHex
, encryptAES
, encryptRSA
, encryptPassword
) where

import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as Base64
import           Data.Char (ord)
import           Numeric (showHex)

toHex :: (Integral a, Show a) => a -> BS.ByteString
toHex n = BS8.pack (showHex n [])

encryptAES :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptAES text key = encrypt (encrypt text nonce) key
  where
    nonce = "0CoJUm6Qyw8W8jud" :: BS.ByteString
    encrypt text key = Base64.encode $ AES.encryptCBC (AES.initAES key) iv padded
      where
        padded = let need = 16 - BS.length text `mod` 16 
                  in text `mappend` BS.replicate need (toEnum need)
        iv = "0102030405060708" :: BS.ByteString

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

encryptPassword :: BS.ByteString -> BS.ByteString
encryptPassword = hexDigest . MD5.hash
  where
    hexDigest = mconcat . map toHex . concatMap (\w -> [fromEnum w `div` 16, fromEnum w `mod` 16]) . BS.unpack
