{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)
import Types.Tank (TankType)

-- | Dữ liệu trạng thái của một người chơi, được server gửi về client.
data PlayerState = PlayerState
  { psId          :: Int
  , psPosition    :: Vec2
  , psBodyAngle   :: Float
  , psTurretAngle :: Float
  , psHealth      :: Int
  , psTankType    :: TankType
  } deriving (Show, Generic)

instance Binary PlayerState

data PlayerCommand = PlayerCommand
  { pcMoveVec     :: Vec2
  , pcTurretAngle :: Float
  , pcDidFire     :: Bool
  } deriving (Show, Generic)

instance Binary PlayerCommand