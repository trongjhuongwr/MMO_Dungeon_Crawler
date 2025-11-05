module Core.Renderer (render) where 

import Graphics.Gloss
import Network.Packet (WorldSnapshot(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..)) 
import Core.Effect (Effect(..))
import Core.Animation (Animation, getCurrentFrame) 
import qualified Data.Map as Map 
import qualified Data.Array as Array 
import UI.HUD (renderHUD) 

import Renderer.Resources (Resources(..))


tileSize :: Float
tileSize = 32.0


drawMap :: Resources -> GameMap -> Picture
drawMap assets gmap =
  let
    tiles = gmapTiles gmap
    ((yMin, xMin), (yMax, xMax)) = Array.bounds tiles
    tileList = Array.assocs tiles
    
    drawTile ((gy, gx), tileType) =
      let
        wx = (fromIntegral gx) * tileSize
        wy = (fromIntegral gy) * tileSize
        -- SỬA: Dùng resTiles
        tilePic = Map.findWithDefault Blank tileType (resTiles assets) 
      in
        Translate wx wy (Scale 2 2 tilePic)
        
  in
    Pictures (map drawTile tileList)


render :: Resources -> GameMap -> WorldSnapshot -> [Effect] -> Animation -> Picture
render assets gameMap snapshot effects turretAnim =
  let
    (ourPlayer, otherPlayers) = case wsPlayers snapshot of
                                  (p:ps) -> (Just p, ps)
                                  []     -> (Nothing, [])
    
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
                     
    -- THÊM MỚI: Lấy góc thân xe của người chơi chính
    playerBodyAngle = case ourPlayer of
                        Just p -> psBodyAngle p
                        Nothing -> 0.0 -- Mặc định nếu không tìm thấy người chơi
                        
    -- LỚP 1: THẾ GIỚI GAME (Mọi thứ di chuyển theo camera)
    worldLayer = Pictures $
      [ mapPic ] ++
      ourPlayerPic ++ otherPlayerPics ++
      map drawEnemy (wsEnemies snapshot) ++
      map (drawBullet assets) (wsBullets snapshot) ++
      map (drawEffect assets) effects
      
    -- LỚP 2: LỚP PHỦ TẦM NHÌN (Vignette)
    -- Được vẽ ở (0, 0) (tâm màn hình)
    -- VÀ Xoay theo góc thân xe của người chơi
    visionLayer =
      Rotate (radToDeg playerBodyAngle) $ -- <-- THÊM PHÉP XOAY
      Scale 1.2 1.2 (resVignetteMask assets)
      
  in
    Pictures
      [ -- 1. Di chuyển thế giới game
        Translate (-camX) (-camY) worldLayer
        
        -- 2. Vẽ lớp phủ tầm nhìn (luôn ở tâm và xoay)
      , visionLayer
        
        -- 3. Vẽ HUD (thanh máu, v.v.)
      , hudPic 
      ]
  
-- SỬA ĐỔI: drawOurPlayer
drawOurPlayer :: Resources -> PlayerState -> Animation -> Picture
drawOurPlayer assets ps anim =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    turretFrame = getCurrentFrame anim
    tankScale = 0.5 
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $ 
          Scale tankScale tankScale (resTankBody assets) -- SỬA: resTankBody
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretFrame 
      ]

-- SỬA ĐỔI: drawOtherPlayer
drawOtherPlayer :: Resources -> PlayerState -> Picture
drawOtherPlayer assets ps =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    -- SỬA: resTurretFrames
    turretFrame = if null (resTurretFrames assets)
                    then Blank
                    else head (resTurretFrames assets)
    tankScale = 0.5
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $
          Scale tankScale tankScale (resTankBody assets) -- SỬA: resTankBody
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretFrame
      ]

-- SỬA ĐỔI: drawBullet
drawBullet :: Resources -> BulletState -> Picture
drawBullet assets bullet =
  let
    (x, y) = (vecX $ bsPosition bullet, vecY $ bsPosition bullet)
    correctAngle = atan2 (vecY $ bsVelocity bullet) (vecX $ bsVelocity bullet)
  in
    Translate x y $
    Rotate (90 - radToDeg correctAngle) $
    Scale 0.25 0.25 (resBullet assets) -- SỬA: resBullet

-- (drawEnemy không đổi)
drawEnemy :: EnemyState -> Picture
drawEnemy enemy =
  let
    (x, y) = (vecX $ esPosition enemy, vecY $ esPosition enemy)
  in
    Translate x y $ Color red (Circle 10)

-- SỬA ĐỔI: drawEffect
drawEffect :: Resources -> Effect -> Picture
drawEffect _ effect =
  let
    (x, y) = (vecX $ effPosition effect, vecY $ effPosition effect)
    frame = getCurrentFrame (effAnimation effect)
  in
    Translate x y $ Color white (Scale 0.25 0.25 frame)

-- (radToDeg không đổi)
radToDeg :: Float -> Float
radToDeg r = r * 180 / pi