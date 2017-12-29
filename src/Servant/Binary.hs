{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE PolyKinds             #-}
module Servant.Binary where

import qualified Network.HTTP.Media   as M
import           Servant.API          (Accept (..), MimeRender (..),
                                      MimeUnrender (..))
import           Data.Proxy           (Proxy (..))
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable        (Typeable)
import           Data.List.NonEmpty   (NonEmpty(..))
import           Data.Maybe           (maybeToList)
import           Data.Binary          (Binary)
import qualified Data.Binary          as Binary
import qualified Codec.Compression.GZip as GZip

data Bin binaryCompression
  deriving (Typeable)

class BinaryCompressionEncoding binaryCompression where
  compressionContentType :: Proxy binaryCompression -> Maybe M.MediaType
  compress :: Proxy binaryCompression -> ByteString -> ByteString
  decompress :: Proxy binaryCompression -> ByteString -> ByteString

data NoCompression

instance BinaryCompressionEncoding NoCompression where
  compressionContentType _ = Nothing
  compress _ = id
  decompress _ = id

data GZip

instance BinaryCompressionEncoding GZip where
  compressionContentType _ = Just $ "content-coding" M.// "gzip"
  compress _ = GZip.compress
  decompress _ = GZip.decompress

instance (BinaryCompressionEncoding binaryCompression) => Accept (Bin binaryCompression) where
  contentTypes _ =
    "application" M.// "vnd.hsbin"
      :| (maybeToList $ compressionContentType compressionTypeProxy)
    where
      compressionTypeProxy = Proxy :: Proxy binaryCompression

instance (Binary a, BinaryCompressionEncoding binaryCompression) => MimeUnrender (Bin binaryCompression) a where
  mimeUnrender _ bs =
    case Binary.decodeOrFail $ decompress compressionTypeProxy bs of
      Left (_, _, err) -> Left $ "Error decoding binary thing: " ++ err
      Right (_, _, res) -> Right res
    where
      compressionTypeProxy = Proxy :: Proxy binaryCompression

instance (Binary a, BinaryCompressionEncoding binaryCompression) => MimeRender (Bin binaryCompression) a where
  mimeRender _ val =
    compress compressionTypeProxy $ Binary.encode val
    where
      compressionTypeProxy = Proxy :: Proxy binaryCompression