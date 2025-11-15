{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)
import Types.Tank (TankType)

-- Trạng thái của một người chơi, do server quản lý và đồng bộ
data PlayerState = PlayerState
  { psId          :: Int
  , psPosition    :: Vec2
  , psBodyAngle   :: Float
  , psTurretAngle :: Float
  , psHealth      :: Int
  , psTankType    :: TankType
  , psLives       :: Int
  , psLastFireTime :: Float
  } deriving (Show, Generic)

instance Binary PlayerState

-- Lệnh của người chơi, do client gửi lên server
data PlayerCommand = PlayerCommand
  { pcMoveVec     :: Vec2
  , pcTurretAngle :: Float
  , pcDidFire     :: Bool
  } deriving (Show, Generic)

instance Binary PlayerCommand