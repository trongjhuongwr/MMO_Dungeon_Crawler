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

-- Đại diện tọa độ lưới dưới dạng số nguyên
data Vec2Int = Vec2Int { x :: Int, y :: Int }
  deriving (Show, Generic, FromJSON)

-- Đại diện định nghĩa bản đồ trong JSON
data MapDefinition = MapDefinition
  { gridWidth    :: Int
  , gridHeight   :: Int
  , playerSpawns :: [Vec2Int]
  , tileIDs      :: [[Int]]
  } deriving (Show, Generic)

-- Tự động tạo instance FromJSON cho MapDefinition
instance FromJSON MapDefinition

-- Chuyển đổi tọa độ lưới sang tọa độ thế giới
gridToWorld :: Vec2Int -> Vec2
gridToWorld (Vec2Int gx gy) =
  Vec2 (fromIntegral gx * 32.0) (fromIntegral gy * 32.0)

-- Chuyển đổi từ Int sang TileType
intToTile :: Int -> TileType
intToTile i =
  let maxVal = fromEnum (maxBound :: TileType)
  in if i >= 0 && i <= maxVal
       then toEnum i
       else Empty -- Mặc định là 'Empty' nếu ID trong JSON không hợp lệ

-- Tải bản đồ từ file JSON
loadMapFromFile :: FilePath -> IO (Either String (GameMap, [Vec2]))
loadMapFromFile path = do
  jsonData <- B.readFile path
  case eitherDecode jsonData of
    Left err -> return $ Left ("Error parse JSON: " ++ err)
    Right mapDef -> do
      let
        w = gridWidth mapDef
        h = gridHeight mapDef
        bounds = ((0, 0), (h - 1, w - 1)) -- (y, x)
        rows = tileIDs mapDef

      when (h /= length rows) $
        fail $ "Map Error: gridHeight(" ++ show h ++ ") does not match tileID row numbers (" ++ show (length rows) ++ ")"
        
      when (any (\row -> w /= length row) rows) $
        fail $ "Map Error: A row with gridWidth(" ++ show w++ ") does not match the row length"

      let
        flatInts = concat rows
        
        tileData = map intToTile flatInts
        
        asList = [ (((i `div` w), (i `mod` w)), tile) | (i, tile) <- zip [0..] tileData ]

        arrayData = Array.array bounds asList
        
        gameMap = GameMap
           { gmapWidth = w
           , gmapHeight = h
           , gmapTiles = arrayData
           }
        
        spawnVecs = map gridToWorld (playerSpawns mapDef)

      return $ Right (gameMap, spawnVecs)