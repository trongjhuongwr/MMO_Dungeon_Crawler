module Types.Map where

import Data.Array (Array)
import Data.Word (Word8)

data TileType
  = Wall_Top
  | Wall_Side
  | Wall_Corner_TopLeft
  | Wall_Corner_TopRight
  | Floor_Main
  | Floor_Alt
  | Door_Closed
  | Door_Open
  | Empty 
  deriving (Eq, Show, Enum, Bounded, Generic)

-- Cần instance để gửi qua mạng
instance NFData TileType
instance Serialize TileType

-- | Dữ liệu bản đồ game, dùng Array 2D để truy cập hiệu quả.
data GameMap = GameMap
  { gmapWidth  :: Int
  , gmapHeight :: Int
  , gmapTiles  :: Array (Int, Int) TileType -- (y, x) hoặc (x, y) - thống nhất!
  } deriving (Eq, Show, Generic)

instance NFData GameMap
instance Serialize GameMap

isSolid :: TileType -> Bool
isSolid tt = case tt of
  Wall_Top    -> True
  Wall_Side   -> True
  Door_Closed -> True
  _           -> False