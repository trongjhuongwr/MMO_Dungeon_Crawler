{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Systems.MapLoader
  ( loadMapFromFile
  , Vec2Int(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import qualified Data.Array as Array
import Data.List (zip)
import Control.Monad (when)

import Types.Map (GameMap(..), TileType(..))
import Types.Common (Vec2(..))

data Vec2Int = Vec2Int { x :: Int, y :: Int }
  deriving (Show, Generic, FromJSON)

data MapDefinition = MapDefinition
  { gridWidth    :: Int
  , gridHeight   :: Int
  , playerSpawns :: [Vec2Int]
  , tileIDs      :: [[Int]]
  } deriving (Show, Generic)

instance FromJSON MapDefinition

-- Chuyển đổi tọa độ Grid (Int) sang World (Float)
gridToWorld :: Vec2Int -> Vec2
gridToWorld (Vec2Int gx gy) =
  -- Nhân với tileSize (32.0)
  Vec2 (fromIntegral gx * 32.0) (fromIntegral gy * 32.0)

-- Chuyển đổi Int sang TileType, có kiểm tra
intToTile :: Int -> TileType
intToTile i =
  let maxVal = fromEnum (maxBound :: TileType)
  in if i >= 0 && i <= maxVal
       then toEnum i
       else Empty -- Mặc định là 'Empty' nếu ID trong JSON không hợp lệ

-- HÀM CHÍNH: Cập nhật logic tải map
loadMapFromFile :: FilePath -> IO (Either String (GameMap, [Vec2]))
loadMapFromFile path = do
  jsonData <- B.readFile path
  case eitherDecode jsonData of
    Left err -> return $ Left ("Lỗi parse JSON: " ++ err)
    Right mapDef -> do
      let
        w = gridWidth mapDef
        h = gridHeight mapDef
        bounds = ((0, 0), (h - 1, w - 1)) -- (y, x)
        rows = tileIDs mapDef

      -- Kiểm tra lỗi kích thước
      when (h /= length rows) $
        fail $ "Lỗi Map: gridHeight (" ++ show h ++ ") không khớp số hàng tileIDs (" ++ show (length rows) ++ ")"
        
      when (any (\row -> w /= length row) rows) $
        fail $ "Lỗi Map: Một hàng có gridWidth (" ++ show w ++ ") không khớp với độ dài hàng"

      let
        -- 1. Biến [[Int]] thành [Int] (làm phẳng)
        flatInts = concat rows
        
        -- 2. Biến [Int] thành [TileType]
        tileData = map intToTile flatInts
        
        -- 3. Biến [TileType] thành danh sách tọa độ (y, x)
        asList = [ (((i `div` w), (i `mod` w)), tile) | (i, tile) <- zip [0..] tileData ]
        
        -- 4. Tạo Array
        arrayData = Array.array bounds asList
        
        gameMap = GameMap
           { gmapWidth = w
           , gmapHeight = h
           , gmapTiles = arrayData
           }
        
        -- 5. Chuyển đổi spawn points
        spawnVecs = map gridToWorld (playerSpawns mapDef)

      return $ Right (gameMap, spawnVecs)