module Data.Aeson.Extra ( 
  encodeJSON
, onObject
, onArray
) where

import           Control.Monad (mzero)
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import           Data.Text.Encoding (decodeUtf8)

instance JSON.ToJSON BS.ByteString where
  toJSON = JSON.String . decodeUtf8

encodeJSON :: JSON.Value -> BS.ByteString
encodeJSON = BL.toStrict . BB.toLazyByteString . JSON.encodeToBuilder

onObject :: (JSON.Object -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
onObject f (JSON.Object o) = f o
onObject _ _ = mzero

onArray :: (JSON.Array -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
onArray f (JSON.Array a) = f a
onArray _ _ = mzero
