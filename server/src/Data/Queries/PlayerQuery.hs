{-# LANGUAGE OverloadedStrings #-}
module Data.Queries.PlayerQuery
  ( registerUser
  , authenticateUser
  ) where

import Database.PostgreSQL.Simple (Connection, Only(..), query)
import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

-- Kiểu dữ liệu trả về từ DB
data UserRow = UserRow Int Text BS.ByteString -- (id, username, password_hash)
instance FromRow UserRow where
  fromRow = UserRow <$> field <*> field <*> field

-- | Đăng ký user mới
registerUser :: Connection -> String -> String -> IO (Either String Int)
registerUser conn user pass = do
  -- 1. Kiểm tra xem user đã tồn tại chưa
  let userT = (TE.decodeUtf8 . BS.pack) user
  [Only count] <- query conn "SELECT COUNT(*) FROM players WHERE username = ?" (Only userT)

  if (count :: Int) > 0
    then pure $ Left "Username already exists"
    else do
      -- 2. Băm mật khẩu
      let passBS = BS.pack pass
      mHash <- hashPasswordUsingPolicy slowerBcryptHashingPolicy passBS
      case mHash of
        Nothing -> pure $ Left "Server error: Could not hash password"
        Just hashedPass -> do
          -- 3. Thêm user vào DB
          [Only newId] <- query conn "INSERT INTO players (username, password_hash) VALUES (?, ?) RETURNING id" (userT, hashedPass)
          pure $ Right newId

-- | Xác thực user
authenticateUser :: Connection -> String -> String -> IO (Maybe (Int, Text))
authenticateUser conn user pass = do
  -- 1. Tìm user bằng username
  let userT = (TE.decodeUtf8 . BS.pack) user
  results <- query conn "SELECT id, username, password_hash FROM players WHERE username = ?" (Only userT)

  case results of
    -- 2. Không tìm thấy user
    [] -> pure Nothing
    -- 3. Tìm thấy, kiểm tra mật khẩu
    (UserRow uid uname storedHash : _) -> do
      let passBS = BS.pack pass
      let isValid = validatePassword storedHash passBS
      if isValid
        then pure $ Just (uid, uname) -- Thành công
        else pure Nothing -- Sai mật khẩu