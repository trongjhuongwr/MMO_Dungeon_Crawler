{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module Renderer.Resources
  ( loadResources
  , Resources(..)
  ) where

import Graphics.Gloss (Picture(..))
import Graphics.Gloss.Juicy (loadJuicyPNG, fromDynamicImage)
import Types.Map (TileType(..))
import qualified Data.Map.Strict as Map
import Control.Monad (forM)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)
import qualified Settings as Settings
import qualified Data.Aeson.Encoding as Settings

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

loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

data Resources = Resources
  { resTiles             :: Map.Map TileType Picture
  , resTankBodyRapid     :: Picture
  , resTurretFramesRapid :: [Picture]
  , resTankBodyBlast     :: Picture
  , resTurretFramesBlast :: [Picture]
  , resBulletNormal      :: Picture
  , resBulletBlast       :: Picture
  , resExplosionFrames   :: [Picture]
  , resVignetteMask      :: Picture
  , resLifeIcons         :: [Picture]
  }

tileTypeToPath :: TileType -> Maybe FilePath
tileTypeToPath tt = fmap Settings.texture $ case tt of
  Floor_00 -> Just "map/floors/floor_00.png"
  Floor_01 -> Just "map/floors/floor_01.png"
  Floor_02 -> Just "map/floors/floor_02.png"
  Floor_03 -> Just "map/floors/floor_03.png"
  Floor_04 -> Just "map/floors/floor_04.png"
  Floor_05 -> Just "map/floors/floor_05.png"
  Floor_06 -> Just "map/floors/floor_06.png"
  Floor_07 -> Just "map/floors/floor_07.png"
  Floor_08 -> Just "map/floors/floor_08.png"
  Floor_09 -> Just "map/floors/floor_09.png"
  Floor_10 -> Just "map/floors/floor_10.png"
  Floor_11 -> Just "map/floors/floor_11.png"
  Floor_Edge_DL -> Just "map/floors/floor_edge_dl.png"
  Floor_Edge_Down_00 -> Just "map/floors/floor_edge_down_00.png"
  Floor_Edge_Down_01 -> Just "map/floors/floor_edge_down_01.png"
  Floor_Edge_DR -> Just "map/floors/floor_edge_dr.png"
  Floor_Edge_Left -> Just "map/floors/floor_edge_left.png"
  Floor_Edge_Right -> Just "map/floors/floor_edge_right.png"
  Floor_Edge_TL -> Just "map/floors/floor_edge_tl.png"
  Floor_Edge_Top_00 -> Just "map/floors/floor_edge_top_00.png"
  Floor_Edge_Top_01 -> Just "map/floors/floor_edge_top_01.png"
  Floor_Edge_TR -> Just "map/floors/floor_edge_tr.png"
  Wall_Back_00 -> Just "map/walls/wall_back_00.png"
  Wall_Back_01 -> Just "map/walls/wall_back_01.png"
  Wall_Front_00 -> Just "map/walls/wall_front_00.png"
  Wall_Front_01 -> Just "map/walls/wall_front_01.png"
  Wall_Front_02 -> Just "map/walls/wall_front_02.png"
  Wall_Left_00 -> Just "map/walls/wall_left_00.png"
  Wall_Left_01 -> Just "map/walls/wall_left_01.png"
  Wall_Left_02 -> Just "map/walls/wall_left_02.png"
  Wall_Left_03 -> Just "map/walls/wall_left_03.png"
  Wall_Left_End -> Just "map/walls/wall_left_end.png"
  Wall_Left_Start -> Just "map/walls/wall_left_start.png"
  Wall_Right_00 -> Just "map/walls/wall_right_00.png"
  Wall_Right_01 -> Just "map/walls/wall_right_01.png"
  Wall_Right_02 -> Just "map/walls/wall_right_02.png"
  Wall_Right_End -> Just "map/walls/wall_right_end.png"
  Wall_Right_Start -> Just "map/walls/wall_right_start.png"
  Empty -> Nothing


-- SỬA ĐỔI 2: Cập nhật 'loadResources'
loadResources :: IO (Either String Resources)
loadResources = do
  -- Load Rapid Tank
  mTankBodyRapid <- loadSprite (Settings.tank "rapid_tank" "body.png") (0, 0) (128, 128)
  eTurretImgRapid <- readImage (Settings.tank "rapid_tank" "turret.png")
  mBulletNormal <- loadJuicyPNG (Settings.projectile "bullet_normal.png")

  -- Load Blast Tank
  mTankBodyBlast <- loadSprite (Settings.tank "blast_tank" "body.png") (0, 0) (128, 128)
  eTurretImgBlast <- readImage (Settings.tank "blast_tank" "turret.png")
  mBulletBlast <- loadJuicyPNG (Settings.projectile "bullet_blast.png")

  -- Load Common FX
  eExplosionImg <- readImage (Settings.projectile "explosion_spritesheet_blast.png")
  mVignette <- loadJuicyPNG (Settings.ui "vignette_mask_01.png")

  -- Tải 4 khung hình 'lives'
  let lifeIconPath = Settings.ui "life_icons.png"
  let lifeCoords = [ ( (297, 132), (77, 20) )   -- 3 Mạng
                   , ( (201, 132), (77, 20) ) -- 2 Mạng
                   , ( (105, 132), (77, 20) ) -- 1 Mạng
                   , ( (9, 132), (77, 20) ) -- 0 Mạng
                   ]
  
  mLifeFrames <- mapM (\(pos, size) -> loadSprite lifeIconPath pos size) lifeCoords
  let mLifeFrames' = catMaybes mLifeFrames

  let allTileTypes = [minBound .. maxBound] :: [TileType]
  tilePairs <- fmap catMaybes $ forM allTileTypes $ \tt -> do
    case tileTypeToPath tt of
      Nothing -> return Nothing
      Just path -> do
        maybePic <- loadJuicyPNG path
        case maybePic of
          Nothing -> do
            putStrLn $ "Warning: Cannot load " ++ path
            return Nothing
          Just pic -> return $ Just (tt, pic)
        
  let tileMap = Map.fromList tilePairs
  putStrLn $ "Loaded " ++ show (Map.size tileMap) ++ " tile pictures."
  
  case ( mTankBodyRapid,  eTurretImgRapid,  mBulletNormal
       , mTankBodyBlast,  eTurretImgBlast,  mBulletBlast
       , eExplosionImg, mVignette
       , mLifeFrames'
       ) of
    ( Just bodyRapid,  Right dynTurretRapid,  Just bulletNormal
      , Just bodyBlast,  Right dynTurretBlast,  Just bulletBlast
      , Right dynExplosionImg, Just vignette
      , lifeFrames
      ) -> 
      let
        turretFramesRapid = loadSpriteSheet dynTurretRapid 128 128 8 
        turretFramesBlast = loadSpriteSheet dynTurretBlast 128 128 8
        explosionFrames = loadSpriteSheet dynExplosionImg 256 256 8 
      in
        return $ Right $ Resources
          { resTiles = tileMap
          , resTankBodyRapid = bodyRapid
          , resTurretFramesRapid = turretFramesRapid
          , resTankBodyBlast = bodyBlast
          , resTurretFramesBlast = turretFramesBlast
          , resBulletNormal = bulletNormal
          , resBulletBlast = bulletBlast
          , resExplosionFrames = explosionFrames
          , resVignetteMask = vignette
          , resLifeIcons = lifeFrames
          }
    _ -> return $ Left "Failed to load critical assets (tanks, bullets, explosion, vignette, or life icons)"