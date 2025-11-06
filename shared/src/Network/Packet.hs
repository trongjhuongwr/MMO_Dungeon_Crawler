{-# LANGUAGE DeriveGeneric #-}

module Network.Packet where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Player (PlayerState)
import Types.Bullet (BulletState)
import Types.Enemy (EnemyState)
import Types.Map (GameMap)

-- THÊM MỚI: Gói tin "mẹ"
data ServerPacket
  = SPWelcome Int -- Chứa ID của người chơi, ví dụ: SPWelcome 1
  | SPSnapshot WorldSnapshot
  deriving (Show, Generic)

instance Binary ServerPacket

-- (WorldSnapshot không đổi)
data WorldSnapshot = WorldSnapshot
  { wsPlayers :: [PlayerState]
  , wsEnemies :: [EnemyState]
  , wsBullets :: [BulletState]
  } deriving (Show, Generic)

instance Binary WorldSnapshot