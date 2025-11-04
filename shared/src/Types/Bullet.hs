{-# LANGUAGE DeriveGeneric #-}

module Types.Bullet where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)

-- | Trạng thái của một viên đạn, do server quản lý và đồng bộ
data BulletState = BulletState
  { bsId       :: Int     -- ID duy nhất
  , bsPosition :: Vec2    -- Vị trí
  , bsVelocity :: Vec2    -- Vector vận tốc (đã bao gồm hướng và tốc độ)
  , bsLifetime :: Float   -- Thời gian sống còn lại (tính bằng giây)
  } deriving (Show, Eq, Generic)

instance Binary BulletState