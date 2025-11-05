{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Types.Map where

import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Data.Serialize (Serialize)
import Data.Array (Array)
import Data.Binary (Binary)

-- | Định danh chi tiết cho từng tile, khớp với cấu trúc file
data TileType
  -- | Vùng rỗng, không thể đi vào -0
  = Empty

  -- === Sàn === 1-12
  | Floor_00 | Floor_01 | Floor_02 | Floor_03
  | Floor_04 | Floor_05 | Floor_06 | Floor_07
  | Floor_08 | Floor_09 | Floor_10 | Floor_11

  -- === Cạnh Sàn === 13-22
  | Floor_Edge_DL
  | Floor_Edge_Down_00 | Floor_Edge_Down_01
  | Floor_Edge_DR
  | Floor_Edge_Left
  | Floor_Edge_Right
  | Floor_Edge_TL
  | Floor_Edge_Top_00 | Floor_Edge_Top_01
  | Floor_Edge_TR

  -- === Tường (Nhìn từ trên) === 23-24
  | Wall_Back_00 | Wall_Back_01

  -- === Tường (Mặt trước) === 25-27
  | Wall_Front_00 | Wall_Front_01 | Wall_Front_02

  -- === Tường (Cạnh trái) === 28-33
  | Wall_Left_00 | Wall_Left_01 | Wall_Left_02 | Wall_Left_03
  | Wall_Left_End | Wall_Left_Start

  -- === Tường (Cạnh phải) === 34-38
  | Wall_Right_00 | Wall_Right_01 | Wall_Right_02
  | Wall_Right_End | Wall_Right_Start
  
  deriving (Eq, Show, Enum, Bounded, Ord, Generic)

instance NFData TileType
instance Serialize TileType
instance Binary TileType

-- | Dữ liệu bản đồ game
data GameMap = GameMap
  { gmapWidth  :: Int
  , gmapHeight :: Int
  , gmapTiles  :: Array (Int, Int) TileType -- (y, x)
  } deriving (Eq, Show, Generic)

instance NFData GameMap
instance Serialize GameMap
instance Binary GameMap

-- | Hàm kiểm tra va chạm
isSolid :: TileType -> Bool
isSolid tt = case tt of
  Empty -> True 
  -- Doors are traversable
  Wall_Back_00   -> True
  Wall_Back_01   -> True
  Wall_Front_00  -> True
  Wall_Front_01  -> True
  Wall_Front_02  -> True
  Wall_Left_00   -> True
  Wall_Left_01   -> True
  Wall_Left_02   -> True
  Wall_Left_03   -> True
  Wall_Left_End  -> True
  Wall_Left_Start -> True
  Wall_Right_00  -> True
  Wall_Right_01  -> True
  Wall_Right_02  -> True
  Wall_Right_End -> True
  Wall_Right_Start -> True
  _ -> False