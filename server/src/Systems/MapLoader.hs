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

gridToWorld :: Vec2Int -> Vec2
gridToWorld (Vec2Int gx gy) =
  Vec2 (fromIntegral gx * 32.0) (fromIntegral gy * 32.0)

intToTile :: Int -> TileType
intToTile i =
  let maxVal = fromEnum (maxBound :: TileType)
  in if i >= 0 && i <= maxVal
       then toEnum i
       else Empty -- Mặc định là 'Empty' nếu ID trong JSON không hợp lệ


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

      when (h /= length rows) $
        fail $ "Lỗi Map: gridHeight (" ++ show h ++ ") không khớp số hàng tileIDs (" ++ show (length rows) ++ ")"
        
      when (any (\row -> w /= length row) rows) $
        fail $ "Lỗi Map: Một hàng có gridWidth (" ++ show w ++ ") không khớp với độ dài hàng"

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