{-# LANGUAGE DeriveGeneric #-}

-- SỬA DÒNG NÀY
module Types.Bullet (BulletState(..), BulletType(..)) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)

-- THÊM MỚI: Định nghĩa loại đạn
data BulletType
  = Normal
  | Blast
  deriving (Show, Eq, Generic)

instance Binary BulletType

-- | Trạng thái của một viên đạn, do server quản lý và đồng bộ
data BulletState = BulletState
  { bsId         :: Int
  , bsOwnerId    :: Int
  , bsBulletType :: BulletType -- <-- THÊM DÒNG NÀY
  , bsPosition   :: Vec2
  , bsVelocity   :: Vec2
  , bsLifetime   :: Float
  } deriving (Show, Eq, Generic)

instance Binary BulletState