{-# LANGUAGE DeriveGeneric #-}

module Types.Player where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)

-- | Dữ liệu trạng thái của một người chơi, được server gửi về client.
data PlayerState = PlayerState
  { psPosition    :: Vec2    -- Vị trí hiện tại
  , psBodyAngle   :: Float   -- Góc xoay của thân xe (radians)
  , psTurretAngle :: Float  
  , psHealth      :: Int
  } deriving (Show, Generic)

instance Binary PlayerState

-- | Lệnh do người chơi gửi lên server.
-- | THAY ĐỔI: Chuyển sang dạng record để gửi toàn bộ input trong 1 gói tin.
data PlayerCommand = PlayerCommand
  { pcMoveVec     :: Vec2    -- Input di chuyển (WASD)
  , pcTurretAngle :: Float   -- Góc nòng súng (từ chuột)
  , pcDidFire     :: Bool    -- True nếu người chơi nhấn bắn ở frame này
  } deriving (Show, Generic)

instance Binary PlayerCommand