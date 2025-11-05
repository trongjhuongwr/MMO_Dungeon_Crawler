module Renderer.Resources
  ( loadResources
  , Resources(..)
  , getTilePic
  ) where

import Graphics.Gloss (Picture(..))
import Graphics.Gloss.Juicy (loadJuicyPNG)  -- Using gloss-juicy instead
import Types.Map (TileType(..))
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Control.Exception (try, SomeException)
import Data.Maybe (catMaybes)

-- | Chứa tất cả tài nguyên đã được load
data Resources = Resources
  { resTiles :: Map.Map TileType Picture
  , resTankBody :: Picture
  , resTankTurret :: Picture
  }

-- | Ánh xạ TileType sang đường dẫn file PNG tương ứng
tileTypeToPath :: TileType -> Maybe FilePath
tileTypeToPath tt = case tt of
  -- Sàn
  Floor_00 -> Just "client/assets/textures/map/floors/floor_00.png"
  Floor_01 -> Just "client/assets/textures/map/floors/floor_01.png"
  Floor_02 -> Just "client/assets/textures/map/floors/floor_02.png"
  Floor_03 -> Just "client/assets/textures/map/floors/floor_03.png"
  Floor_04 -> Just "client/assets/textures/map/floors/floor_04.png"
  Floor_05 -> Just "client/assets/textures/map/floors/floor_05.png"
  Floor_06 -> Just "client/assets/textures/map/floors/floor_06.png"
  Floor_07 -> Just "client/assets/textures/map/floors/floor_07.png"
  Floor_08 -> Just "client/assets/textures/map/floors/floor_08.png"
  Floor_09 -> Just "client/assets/textures/map/floors/floor_09.png"
  Floor_10 -> Just "client/assets/textures/map/floors/floor_10.png"
  Floor_11 -> Just "client/assets/textures/map/floors/floor_11.png"
  Floor_Edge_DL -> Just "client/assets/textures/map/floors/floor_edge_dl.png"
  Floor_Edge_Down_00 -> Just "client/assets/textures/map/floors/floor_edge_down_00.png"
  Floor_Edge_Down_01 -> Just "client/assets/textures/map/floors/floor_edge_down_01.png"
  Floor_Edge_DR -> Just "client/assets/textures/map/floors/floor_edge_dr.png"
  Floor_Edge_Left -> Just "client/assets/textures/map/floors/floor_edge_left.png"
  Floor_Edge_Right -> Just "client/assets/textures/map/floors/floor_edge_right.png"
  Floor_Edge_TL -> Just "client/assets/textures/map/floors/floor_edge_tl.png"
  Floor_Edge_Top_00 -> Just "client/assets/textures/map/floors/floor_edge_top_00.png"
  Floor_Edge_Top_01 -> Just "client/assets/textures/map/floors/floor_edge_top_01.png"
  Floor_Edge_TR -> Just "client/assets/textures/map/floors/floor_edge_tr.png"

  -- Tường
  Wall_Back_00 -> Just "client/assets/textures/map/walls/wall_back_00.png"
  Wall_Back_01 -> Just "client/assets/textures/map/walls/wall_back_01.png"
  Wall_Front_00 -> Just "client/assets/textures/map/walls/wall_front_00.png"
  Wall_Front_01 -> Just "client/assets/textures/map/walls/wall_front_01.png"
  Wall_Front_02 -> Just "client/assets/textures/map/walls/wall_front_02.png"
  Wall_Left_00 -> Just "client/assets/textures/map/walls/wall_left_00.png"
  Wall_Left_01 -> Just "client/assets/textures/map/walls/wall_left_01.png"
  Wall_Left_02 -> Just "client/assets/textures/map/walls/wall_left_02.png"
  Wall_Left_03 -> Just "client/assets/textures/map/walls/wall_left_03.png"
  Wall_Left_End -> Just "client/assets/textures/map/walls/wall_left_end.png"
  Wall_Left_Start -> Just "client/assets/textures/map/walls/wall_left_start.png"
  Wall_Right_00 -> Just "client/assets/textures/map/walls/wall_right_00.png"
  Wall_Right_01 -> Just "client/assets/textures/map/walls/wall_right_01.png"
  Wall_Right_02 -> Just "client/assets/textures/map/walls/wall_right_02.png"
  Wall_Right_End -> Just "client/assets/textures/map/walls/wall_right_end.png"
  Wall_Right_Start -> Just "client/assets/textures/map/walls/wall_right_start.png"

  Door_Entrance_Left -> Just "client/assets/textures/map/items/door_left.png"
  Door_Exit_Right -> Just "client/assets/textures/map/items/door_right.png"

  -- Empty tiles don't have an image
  Empty -> Nothing

  -- Fallback for any future constructors
  _ -> Nothing

-- | Hàm load TẤT CẢ tài nguyên game
loadResources :: IO Resources
loadResources = do
  tankBodyMaybe <- loadJuicyPNG "/assets/textures/tanks/rapid_tank/body.png"
  tankTurretMaybe <- loadJuicyPNG "/assets/textures/tanks/rapid_tank/turret.png"
  let tankBody = maybe Blank id tankBodyMaybe
      tankTurret = maybe Blank id tankTurretMaybe

  let allTileTypes = [minBound .. maxBound] :: [TileType]
  
  tilePairs <- fmap catMaybes $ forM allTileTypes $ \tt -> do
    case tileTypeToPath tt of
      Nothing -> return Nothing
      Just path -> do
        maybePic <- loadJuicyPNG path
        case maybePic of
          Nothing -> do
            putStrLn $ "Cảnh báo: Không load được " ++ path
            return Nothing
          Just pic -> return $ Just (tt, pic)
        
  let tileMap = Map.fromList tilePairs
  putStrLn $ "Đã load " ++ show (Map.size tileMap) ++ " tile pictures."
  
  return $ Resources
    { resTiles = tileMap
    , resTankBody = tankBody
    , resTankTurret = tankTurret
    }

getTilePic :: Resources -> TileType -> Picture
getTilePic res tt = Map.findWithDefault Blank tt (resTiles res)