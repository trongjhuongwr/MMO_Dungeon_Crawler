module Data.Database (connectDb) where

import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8 as BS

-- | Kết nối đến DB. Lấy thông tin từ docker-compose.yaml
connectDb :: IO Connection
connectDb = connect defaultConnectInfo
  { connectHost = "database" -- Tên service trong docker-compose
  , connectPort = 5432
  , connectUser = "mmouser"
  , connectPassword = "mmopass"
  , connectDatabase = "mmodb"
  }