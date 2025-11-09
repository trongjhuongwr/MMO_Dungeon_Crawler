{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Config 
  ( ClientConfig(..)
  , loadConfig
  ) where

import Data.Yaml
import GHC.Generics (Generic)
import Network.Socket (HostName, PortNumber)

-- | Định nghĩa cấu trúc file client.yaml
data ClientConfig = ClientConfig
  { server_host :: HostName
  , server_tcp_port :: Int
  , server_udp_port :: String -- Giữ là String vì getAddrInfo cần
  } deriving (Show, Generic, FromJSON)

-- | Tải config
loadConfig :: FilePath -> IO ClientConfig
loadConfig path = do
  eConfig <- decodeFileEither path
  case eConfig of
    Left err -> fail $ "Failed to load client config: " ++ prettyPrintParseException err
    Right cfg -> pure cfg