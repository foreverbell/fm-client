module Data.Aeson.Extra (
  -- * Extra utilities for Aeson
  encodeJSON
, onObject
, onArray
) where

import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Text.Encoding (decodeUtf8)

instance JSON.ToJSON BS.ByteString where
  toJSON = JSON.String . decodeUtf8

-- | Encodes a JSON value to 'BS.ByteString'.
encodeJSON :: JSON.Value -> BS.ByteString
encodeJSON = BL.toStrict . JSON.encode

-- | Parses a JSON value as a 'JSON.Object'.
onObject :: (JSON.Object -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
onObject = JSON.withObject "Object"

-- | Parses a JSON value as a 'JSON.Array'.
onArray :: (JSON.Array -> JSON.Parser a) -> JSON.Value -> JSON.Parser a
onArray = JSON.withArray "Array"
