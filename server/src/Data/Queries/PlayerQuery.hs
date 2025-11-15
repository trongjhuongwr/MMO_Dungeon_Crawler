{-# LANGUAGE OverloadedStrings #-}
module Data.Queries.PlayerQuery
  ( registerUser
  , authenticateUser
  ) where

import Database.SQLite.Simple (Connection, Only(..), query, execute, lastInsertRowId, FromRow(..), field)

import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Kiểu dữ liệu trả về từ DB
data UserRow = UserRow Int Text BS.ByteString -- (id, username, password_hash)
instance FromRow UserRow where
  fromRow = UserRow <$> field <*> field <*> field

-- Đăng ký user mới
registerUser :: Connection -> String -> String -> IO (Either String Int)
registerUser conn user pass = do
  -- Kiểm tra xem user đã tồn tại chưa
  let userT = (TE.decodeUtf8 . BS.pack) user
  [Only count] <- query conn "SELECT COUNT(*) FROM players WHERE username = ?" (Only userT)

  if (count :: Int) > 0
    then pure $ Left "Username already exists"
    else do
      -- Băm mật khẩu
      let passBS = BS.pack pass
      mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy passBS
      case mHash of
        Nothing -> pure $ Left "Server error: Could not hash password"
        Just hashedPass -> do
          -- Thêm user vào DB
          execute conn "INSERT INTO players (username, password_hash) VALUES (?, ?)" (userT, hashedPass)
          -- Lấy ID của hàng vừa được chèn
          newId64 <- lastInsertRowId conn
          -- Chuyển đổi từ Int64 sang Int
          pure $ Right (fromIntegral newId64)

-- Xác thực user
authenticateUser :: Connection -> String -> String -> IO (Maybe (Int, Text))
authenticateUser conn user pass = do
  -- Tìm user bằng username
  let userT = (TE.decodeUtf8 . BS.pack) user
  results <- query conn "SELECT id, username, password_hash FROM players WHERE username = ?" (Only userT)

  case results of
    -- Không tìm thấy user thì trả về Nothing
    [] -> pure Nothing
    -- Tìm thấy thì kiểm tra mật khẩu
    (UserRow uid uname storedHash : _) -> do
      let passBS = BS.pack pass
      let isValid = validatePassword storedHash passBS
      if isValid
        then pure $ Just (uid, uname) -- Thành công
        else pure Nothing             -- Sai mật khẩu