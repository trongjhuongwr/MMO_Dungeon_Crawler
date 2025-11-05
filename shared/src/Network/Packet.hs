{-# LANGUAGE DeriveGeneric #-}

module Network.Packet where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Player (PlayerState)
import Types.Bullet (BulletState)
import Types.Enemy (EnemyState)
import Types.Map (GameMap)


data WorldSnapshot = WorldSnapshot
  { wsPlayers :: [PlayerState]
  , wsEnemies :: [EnemyState]
  , wsBullets :: [BulletState]
  } deriving (Show, Generic)

instance Binary WorldSnapshot