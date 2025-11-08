module Core.Renderer (render) where 

import Graphics.Gloss
import Network.Packet (WorldSnapshot(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import qualified Types.Bullet as Bullet
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..)) 
import Types.Tank (TankType(..))
import qualified Types.Tank as Tank
import Core.Effect (Effect(..))
import Core.Animation (Animation, getCurrentFrame) 
import qualified Data.Map as Map 
import qualified Data.Array as Array 
import UI.HUD (renderHUD) 
import Data.Maybe (maybe, Maybe(..)) 
import qualified Data.List as List 
import Data.Ord (comparing) 

import Renderer.Resources (Resources(..))
import Types.MatchState (MatchState(..)) 


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
        wx = fromIntegral gx * tileSize
        wy = fromIntegral gy * tileSize
        tilePic = Map.findWithDefault Blank tileType (resTiles assets) 
      in
        Translate wx wy (Scale 2 2 tilePic)
        
  in
    Pictures (map drawTile tileList)


render :: Resources 
       -> GameMap 
       -> WorldSnapshot 
       -> [Effect] 
       -> Animation -- ^ animRapid
       -> Animation -- ^ animBlast
       -> Maybe Int -- ^ mMyId
       -> MatchState 
       -> Picture
render assets gameMap snapshot effects animRapid animBlast mMyId matchState =
  let
    (ourPlayer, otherPlayers) = 
      case mMyId of
        Nothing -> (Nothing, wsPlayers snapshot) 
        Just myId -> 
          ( List.find (\p -> psId p == myId) (wsPlayers snapshot)
          , List.filter (\p -> psId p /= myId) (wsPlayers snapshot)
          )
    
    mapPic = drawMap assets gameMap
    
    hudPic = case (ourPlayer, matchState) of
               (Just p, InProgress) -> renderHUD assets p
               _ -> Blank
               
    ourPlayerPic = case ourPlayer of
                     Just p  -> 
                       let 
                         ourAnim = if psTankType p == Tank.Blast
                                     then animBlast
                                     else animRapid
                       in [drawOurPlayer assets p ourAnim]
                     Nothing -> []
    
    otherPlayerPics = map (drawOtherPlayer assets) otherPlayers
        
    (camX, camY) = case ourPlayer of
                     Just p  -> (vecX $ psPosition p, vecY $ psPosition p)
                     Nothing -> (0, 0)
                     
    playerBodyAngle = maybe 0.0 psBodyAngle ourPlayer
    playerTurretAngle = maybe 0.0 psTurretAngle ourPlayer
                        
    worldLayer = Pictures $
      [ mapPic ] ++
      [ visionLayer ] ++
      ourPlayerPic ++ otherPlayerPics ++
      map drawEnemy (wsEnemies snapshot) ++
      map (drawBullet assets) (wsBullets snapshot) ++
      map (drawEffect assets) effects
      
    visionLayer =
      Translate camX camY $
      Rotate (radToDeg playerTurretAngle) $
      Scale 1.2 1.2 (resVignetteMask assets)
    
    -- *** ĐÂY LÀ PHẦN ĐÃ SỬA LỖI CÚ PHÁP ***
    uiOverlay = case matchState of
      Waiting -> centeredText white "Waiting for opponent..."
      InProgress -> Blank
      GameOver mWinnerId ->
        case (mMyId, mWinnerId) of
          (Just myId, Just winnerId) | myId == winnerId -> centeredText green "YOU WIN!"
          (Just _, Nothing) -> centeredText yellow "DRAW!"
          _ -> centeredText red "YOU LOSE!"

  in
    Pictures
      [ 
        Translate (-camX) (-camY) worldLayer
      , hudPic
      , uiOverlay 
      ]
  
drawOurPlayer :: Resources -> PlayerState -> Animation -> Picture
drawOurPlayer assets ps anim =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    
    (bodyPic, turretPic) = case psTankType ps of
      Tank.Rapid -> (resTankBodyRapid assets, getCurrentFrame anim)
      Tank.Blast -> (resTankBodyBlast assets, getCurrentFrame anim)
      
    tankScale = 0.5 
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $ 
          Scale tankScale tankScale bodyPic
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretPic 
      ]

drawOtherPlayer :: Resources -> PlayerState -> Picture
drawOtherPlayer assets ps =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    
    (bodyPic, turretPic) = case psTankType ps of
      Tank.Rapid -> (resTankBodyRapid assets, head $ resTurretFramesRapid assets)
      Tank.Blast -> (resTankBodyBlast assets, head $ resTurretFramesBlast assets)
      
    tankScale = 0.5
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) $
          Scale tankScale tankScale bodyPic
      , Rotate (radToDeg $ psTurretAngle ps) $
          Scale tankScale tankScale turretPic
      ]


drawBullet :: Resources -> BulletState -> Picture
drawBullet assets bullet =
  let
    (x, y) = (vecX $ bsPosition bullet, vecY $ bsPosition bullet)
    correctAngle = atan2 (vecY $ bsVelocity bullet) (vecX $ bsVelocity bullet)
    
    bulletPic = case bsBulletType bullet of
                  Bullet.Normal -> resBulletNormal assets
                  Bullet.Blast  -> resBulletBlast assets
  in
    Translate x y $
    Rotate (90 - radToDeg correctAngle) $
    Scale 0.25 0.25 bulletPic

drawEnemy :: EnemyState -> Picture
drawEnemy enemy =
  let
    (x, y) = (vecX $ esPosition enemy, vecY $ esPosition enemy)
  in
    Translate x y $ Color red (Circle 10)

drawEffect :: Resources -> Effect -> Picture
drawEffect _ effect =
  let
    (x, y) = (vecX $ effPosition effect, vecY $ effPosition effect)
    frame = getCurrentFrame (effAnimation effect)
  in
    Translate x y $ Color white (Scale 0.25 0.25 frame)

radToDeg :: Float -> Float
radToDeg r = r * 180 / pi

centeredText :: Color -> String -> Picture
centeredText col str =
  let
    textWidth = fromIntegral (length str) * 20 
  in
    Translate (- (textWidth / 2)) 0
    $ Scale 0.4 0.4
    $ Color col
    $ Text str