{-# LANGUAGE DeriveGeneric #-}

-- SỬA DÒNG NÀY
module Types.Tank (Tank(..), TankType(..)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)

-- THÊM MỚI: Định nghĩa loại tank
data TankType
  = Rapid
  | Blast
  deriving (Show, Eq, Generic)

instance Binary TankType

-- Dữ liệu cũ, có thể dùng sau
data Tank = Tank { tid :: Int, tname :: String } deriving (Show, Generic) 

instance Binary Tank