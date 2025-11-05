module Systems.DungeonSystem (generateTestLevel) where

import Types.Map (GameMap(..), TileType(..))
import qualified Data.Array as Array
import Data.Array (Array, (//))

-- | Tạo một map rỗng (toàn bộ là 'Empty' hoặc tường)
createMap :: Int -> Int -> TileType -> GameMap
createMap width height fillTile =
  let
    bounds = ((0, 0), (height - 1, width - 1))
    tiles = Array.listArray bounds (repeat fillTile)
  in GameMap
       { gmapWidth = width
       , gmapHeight = height
       , gmapTiles = tiles
       }

-- | Helper: "Đục" một căn phòng
carveRoom :: GameMap -> (Int, Int) -> (Int, Int) -> TileType -> GameMap
carveRoom gmap (y1, x1) (y2, x2) tile =
  let
    indices = [(r, c) | r <- [y1..y2], c <- [x1..x2]]
    updates = [(idx, tile) | idx <- indices]
  in
    gmap { gmapTiles = gmapTiles gmap // updates }

-- | Helper: "Đục" một hành lang ngang
carveHCorridor :: GameMap -> Int -> Int -> Int -> TileType -> GameMap
carveHCorridor gmap y x1 x2 tile = -- y là tọa độ trung tâm của hành lang
  carveRoom gmap (y - 1, min x1 x2) (y + 2, max x1 x2) tile -- Rộng 4 ô (y-1, y, y+1, y+2)

-- | Helper: "Đục" một hành lang dọc
carveVCorridor :: GameMap -> Int -> Int -> Int -> TileType -> GameMap
carveVCorridor gmap x y1 y2 tile = -- x là tọa độ trung tâm của hành lang
  carveRoom gmap (min y1 y2, x - 1) (max y1 y2, x + 2) tile -- Rộng 4 ô (x-1, x, x+1, x+2)

-- | Hàm chính tạo map tĩnh theo yêu cầu
generateTestLevel :: GameMap
generateTestLevel =
  let
    -- Bắt đầu với một map 100x50 toàn là tường
    baseMap = createMap 100 50 Wall_Back_00
    
    -- 1. Phòng Spawn (Spawn Room)
    (spawnY, spawnX) = (25, 5)
    map1 = carveRoom baseMap (spawnY - 2, spawnX - 2) (spawnY + 2, spawnX + 2) Floor_01
    
    -- 2. Hành lang 1
    map2 = carveHCorridor map1 spawnY (spawnX + 3) 20 Floor_01
    
    -- 3. Phòng 1.1 (nhỏ)
    (room1Y, room1X) = (25, 25)
    map3 = carveRoom map2 (room1Y - 4, room1X - 4) (room1Y + 4, room1X + 4) Floor_01
    
    -- 4. Hành lang 2
    map4 = carveHCorridor map3 room1Y (room1X + 5) 40 Floor_01
    
    -- 5. Phòng 1.2 (vừa)
    (room2Y, room2X) = (25, 48)
    map5 = carveRoom map4 (room2Y - 7, room2X - 7) (room2Y + 7, room2X + 7) Floor_01
    
    -- 6. Hành lang 3 (dẫn đến Boss)
    map6 = carveHCorridor map5 room2Y (room2X + 8) 70 Floor_01
    
    -- 7. Phòng Boss (lớn)
    (bossY, bossX) = (25, 85)
    map7 = carveRoom map6 (bossY - 12, bossX - 12) (bossY + 12, bossX + 12) Floor_01

    -- (Bạn có thể thêm cửa vào/ra tại các điểm nối hành lang nếu muốn)
    -- Ví dụ: Đặt cửa vào phòng 1.1
    map8 = map7 { gmapTiles = gmapTiles map7 // [((room1Y, room1X - 5), Door_Entrance_Left)] }
    
  in
    map8