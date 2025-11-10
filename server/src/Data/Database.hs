{-# LANGUAGE OverloadedStrings #-}
module Data.Database (connectDb) where

import Database.SQLite.Simple (Connection, open, execute_)
import System.IO (FilePath)

-- | Đường dẫn đến tệp database SQLite
dbPath :: FilePath
dbPath = "mmo_server.db"

-- | Kết nối đến DB SQLite và chạy migration
connectDb :: IO Connection
connectDb = do
  putStrLn $ "Opening SQLite database at: " ++ dbPath
  conn <- open dbPath
  -- Bật hỗ trợ foreign key (khuyến nghị)
  execute_ conn "PRAGMA foreign_keys = ON;"
  -- Chạy migration để tạo bảng
  runMigrations conn
  pure conn

-- | Tạo bảng nếu chưa tồn tại
runMigrations :: Connection -> IO ()
runMigrations conn = do
  putStrLn "Running SQLite migrations..."
  execute_ conn "CREATE TABLE IF NOT EXISTS players ( id INTEGER PRIMARY KEY AUTOINCREMENT, username TEXT NOT NULL UNIQUE, password_hash TEXT NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP );"
  putStrLn "Migrations complete."