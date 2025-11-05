module Core.Renderer (render, GameAssets(..), loadSpriteSheet) where

import Graphics.Gloss
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Network.Packet (WorldSnapshot(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..)) -- THÊM MỚI
import Core.Effect (Effect(..))
import Core.Animation (Animation, getCurrentFrame) 
import Codec.Picture (generateImage, pixelAt, convertRGBA8, DynamicImage(ImageRGBA8))
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe (mapMaybe)
import qualified Data.Map as Map -- THÊM MỚI
import qualified Data.Array as Array -- THÊM MỚI
import UI.HUD (renderHUD) -- THÊM MỚI

data GameAssets = GameAssets
  { gaTankBody        :: Picture
  , gaTurretFrames    :: [Picture]
  , gaBullet          :: Picture
  , gaExplosionFrames :: [Picture]
  , gaTiles           :: Map.Map TileType Picture -- THÊM MỚI
  }

-- Kích thước tile (PHẢI KHỚP VỚI SERVER)
tileSize :: Float
tileSize = 32.0

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

loadTile :: FilePath -> IO Picture
loadTile path = do
  mPic <- loadJuicyPNG path
  case mPic of
    Just pic -> return pic
    Nothing  -> do
      putStrLn $ "Warning: Failed to load tile: " ++ path
      return Blank

loadTiles :: IO (Map.Map TileType Picture)
loadTiles = do
  -- Sàn
  f01 <- loadTile "client/assets/textures/map/floors/floor_01.png"
  
  -- Tường
  wBack00 <- loadTile "client/assets/textures/map/walls/wall_back_00.png"
  
  -- Cửa
  dEntrance <- loadTile "client/assets/textures/map/items/door_left.png"
  
  return $ Map.fromList
    [ (Floor_01, f01)
    
    -- Thêm các loại sàn khác nếu dùng
    , (Floor_00, f01), (Floor_02, f01), (Floor_03, f01)
    , (Floor_04, f01), (Floor_05, f01), (Floor_06, f01)
    , (Floor_07, f01), (Floor_08, f01), (Floor_09, f01)
    , (Floor_10, f01), (Floor_11, f01)
    
    -- Tường
    , (Wall_Back_00, wBack00)
    , (Wall_Back_01, wBack00) -- Dùng tạm
    , (Wall_Front_00, wBack00), (Wall_Front_01, wBack00), (Wall_Front_02, wBack00)
    , (Wall_Left_00, wBack00), (Wall_Left_01, wBack00), (Wall_Left_02, wBack00), (Wall_Left_03, wBack00)
    , (Wall_Left_End, wBack00), (Wall_Left_Start, wBack00)
    , (Wall_Right_00, wBack00), (Wall_Right_01, wBack00), (Wall_Right_02, wBack00)
    , (Wall_Right_End, wBack00), (Wall_Right_Start, wBack00)
    
    -- Gán tile 'Empty' cho tường để đảm bảo va chạm
    , (Empty, wBack00)
    ]

-- THÊM MỚI: Hàm vẽ map
drawMap :: GameAssets -> GameMap -> Picture
drawMap assets gmap =
  let
    tiles = gmapTiles gmap
    ((yMin, xMin), (yMax, xMax)) = Array.bounds tiles
    
    -- Lấy danh sách (chỉ số, loại tile)
    tileList = Array.assocs tiles
    
    -- Chuyển đổi (y, x) grid -> (x, y) world coords
    drawTile ((gy, gx), tileType) =
      let
        wx = (fromIntegral gx) * tileSize
        wy = (fromIntegral gy) * tileSize
        
        -- Lấy ảnh từ assets, nếu không có thì dùng ảnh trống
        tilePic = Map.findWithDefault Blank tileType (gaTiles assets)
      in
        -- Dịch chuyển đến đúng tọa độ thế giới
        Translate wx wy (Scale 2 2 tilePic)
        
  in
    Pictures (map drawTile tileList)

render :: GameAssets -> GameMap -> WorldSnapshot -> [Effect] -> Animation -> Picture
render assets gameMap snapshot effects turretAnim =
  let
    (ourPlayer, otherPlayers) = case wsPlayers snapshot of
                                  (p:ps) -> (Just p, ps)
                                  []     -> (Nothing, [])
    
    -- SỬA ĐỔI: mapPic dùng gameMap, KHÔNG phải snapshot
    mapPic = drawMap assets gameMap
    
    hudPic = case ourPlayer of
               Just p  -> renderHUD p
               Nothing -> Blank
               
    ourPlayerPic = case ourPlayer of
                     Just p  -> [drawOurPlayer assets p turretAnim]
                     Nothing -> []
    
    otherPlayerPics = map (drawOtherPlayer assets) otherPlayers
    
    (camX, camY) = case ourPlayer of
                     Just p  -> (vecX $ psPosition p, vecY $ psPosition p)
                     Nothing -> (0, 0)
                     
    worldPics = Pictures $
      [ mapPic ] ++
      ourPlayerPic ++ otherPlayerPics ++
      map drawEnemy (wsEnemies snapshot) ++
      map (drawBullet assets) (wsBullets snapshot) ++
      map (drawEffect assets) effects
      
  in
    Pictures
      [ Translate (-camX) (-camY) worldPics
      , hudPic 
      ]

-- SỬA ĐỔI: drawOurPlayer (thêm Scale)
drawOurPlayer :: GameAssets -> PlayerState -> Animation -> Picture
drawOurPlayer assets ps anim =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    turretFrame = getCurrentFrame anim
    -- Sprite 128px -> Hiển thị 64px (khớp hitbox 32x32)
    tankScale = 0.5 
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $ 
          Scale tankScale tankScale (gaTankBody assets) -- <-- THÊM SCALE
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretFrame -- <-- THÊM SCALE
      ]

-- SỬA ĐỔI: drawOtherPlayer (thêm Scale)
drawOtherPlayer :: GameAssets -> PlayerState -> Picture
drawOtherPlayer assets ps =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    turretFrame = if null (gaTurretFrames assets)
                    then Blank
                    else head (gaTurretFrames assets)
    tankScale = 0.5 -- <-- THÊM
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $
          Scale tankScale tankScale (gaTankBody assets) -- <-- THÊM SCALE
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretFrame -- <-- THÊM SCALE
      ]

drawBullet :: GameAssets -> BulletState -> Picture
drawBullet assets bullet =
  let
    (x, y) = (vecX $ bsPosition bullet, vecY $ bsPosition bullet)
    correctAngle = atan2 (vecY $ bsVelocity bullet) (vecX $ bsVelocity bullet)
  in
    Translate x y $
    Rotate (90 - radToDeg correctAngle) $
    Scale 0.25 0.25 (gaBullet assets)

drawEnemy :: EnemyState -> Picture
drawEnemy enemy =
  let
    (x, y) = (vecX $ esPosition enemy, vecY $ esPosition enemy)
  in
    Translate x y $ Color red (Circle 10)

drawEffect :: GameAssets -> Effect -> Picture
drawEffect _ effect =
  let
    (x, y) = (vecX $ effPosition effect, vecY $ effPosition effect)
    frame = getCurrentFrame (effAnimation effect)
  in
    Translate x y $ Color white (Scale 0.25 0.25 frame)

radToDeg :: Float -> Float
radToDeg r = r * 180 / pi