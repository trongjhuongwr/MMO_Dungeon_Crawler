{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Core.Config
  ( AppConfig(..)
  , loadConfig
  ) where

import Data.Yaml
import GHC.Generics (Generic)
import Network.Socket (PortNumber)

-- Định nghĩa cấu trúc của file server.yaml
data AppConfig = AppConfig
  { port     :: Int
  , udpPort  :: Int
  , tickRate :: Int
  , mapFile  :: FilePath
  } deriving (Show, Generic, FromJSON)

-- Tải config từ một file
loadConfig :: FilePath -> IO AppConfig
loadConfig path = do
  eConfig <- decodeFileEither path
  case eConfig of
    Left err -> fail $ "Failed to load config: " ++ prettyPrintParseException err
    Right cfg -> pure cfg