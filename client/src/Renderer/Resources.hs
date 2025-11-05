module Renderer.Resources
  ( loadResources
  , Resources(..)
  ) where

import Graphics.Gloss (Picture(..))
import Graphics.Gloss.Juicy (loadJuicyPNG, fromDynamicImage)
import Types.Map (TileType(..))
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Maybe (catMaybes, mapMaybe)
import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)

-- | HÀM TỪ Core/Renderer.hs (Di chuyển về đây)
loadSpriteSheet :: DynamicImage -> Int -> Int -> Int -> [Picture]
loadSpriteSheet dynImg frameWidth frameHeight frameCount =
  let
    rgba = convertRGBA8 dynImg
    frames = [ (i, 0) | i <- [0..(frameCount-1)] ] 
    
    cropFrame (fx, fy) =
      let
        offsetX = fx * frameWidth
        offsetY = fy * frameHeight
        cropped = generateImage (\i j -> pixelAt rgba (offsetX + i) (offsetY + j)) frameWidth frameHeight
      in
        fromDynamicImage (ImageRGBA8 cropped)
  in
    mapMaybe cropFrame frames

-- | HÀM TỪ Main.hs (Di chuyển về đây)
loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

-- SỬA ĐỔI: data Resources (thay thế cho GameAssets)
data Resources = Resources
  { resTiles          :: Map.Map TileType Picture
  , resTankBody       :: Picture
  , resTurretFrames   :: [Picture]
  , resBullet         :: Picture
  , resExplosionFrames :: [Picture]
  , resVignetteMask   :: Picture
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

  -- Empty tiles don't have an image
  Empty -> Nothing

-- SỬA ĐỔI: loadResources (tải TẤT CẢ)
loadResources :: IO (Either String Resources)
loadResources = do
  -- Load Player/FX assets
  mTankBody <- loadSprite "client/assets/textures/tanks/rapid_tank/body.png" (0, 0) (128, 128)
  eTurretImg <- readImage "client/assets/textures/tanks/rapid_tank/turret.png" 
  mBullet <- loadJuicyPNG "client/assets/textures/projectiles/bullet_normal.png"
  eExplosionImg <- readImage "client/assets/textures/projectiles/explosion_spritesheet_blast.png"

  mVignette <- loadJuicyPNG "client/assets/textures/ui/vignette_mask.png"
  -- Load Tile assets
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
  
  -- Kiểm tra lỗi
  case (mTankBody, eTurretImg, mBullet, eExplosionImg, mVignette) of
    (Just body, Right dynTurretImg, Just bullet, Right dynExplosionImg, Just vignette) -> -- <-- THÊM
      let
        turretFrames = loadSpriteSheet dynTurretImg 128 128 8 
        explosionFrames = loadSpriteSheet dynExplosionImg 256 256 8 
      in
        return $ Right $ Resources
          { resTiles = tileMap
          , resTankBody = body
          , resTurretFrames = turretFrames
          , resBullet = bullet
          , resExplosionFrames = explosionFrames
          , resVignetteMask = vignette
          }
    _ -> return $ Left "Failed to load critical assets (tank, bullet, explosion, or vignette)"