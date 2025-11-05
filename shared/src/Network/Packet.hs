{-# LANGUAGE DeriveGeneric #-}

module Network.Packet where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Player (PlayerState)
import Types.Bullet (BulletState)
import Types.Enemy (EnemyState)
import Types.Map (GameMap)

-- | Trạng thái của toàn bộ thế giới game tại một thời điểm.
-- | THAY ĐỔI: Chuyển từ 'newtype' sang 'data' vì có nhiều hơn 1 trường.
data WorldSnapshot = WorldSnapshot
  { wsPlayers :: [PlayerState]
  , wsEnemies :: [EnemyState]
  , wsBullets :: [BulletState]
  , wsMap     :: GameMap
  } deriving (Show, Generic)

instance Binary WorldSnapshot