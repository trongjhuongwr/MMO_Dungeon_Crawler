module Core.Renderer (render, GameAssets(..), loadSpriteSheet) where

import Graphics.Gloss
import Network.Packet (WorldSnapshot(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Core.Effect (Effect(..))
import Core.Animation (Animation, getCurrentFrame) 
import Codec.Picture (generateImage, pixelAt, convertRGBA8, DynamicImage(ImageRGBA8))
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe (mapMaybe)

data GameAssets = GameAssets
  { gaTankBody        :: Picture
  , gaTurretFrames    :: [Picture]
  , gaBullet          :: Picture
  , gaExplosionFrames :: [Picture]
  }

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

render :: GameAssets -> WorldSnapshot -> [Effect] -> Animation -> Picture
render assets snapshot effects turretAnim =
  let
    (ourPlayer, otherPlayers) = case wsPlayers snapshot of
                                  (p:ps) -> (Just p, ps)
                                  []     -> (Nothing, [])
    
    ourPlayerPic = case ourPlayer of
                     Just p  -> [drawOurPlayer assets p turretAnim]
                     Nothing -> []
    
    otherPlayerPics = map (drawOtherPlayer assets) otherPlayers
  in
    Pictures $
      ourPlayerPic ++ otherPlayerPics ++
      map drawEnemy (wsEnemies snapshot) ++
      map (drawBullet assets) (wsBullets snapshot) ++
      map (drawEffect assets) effects

drawOurPlayer :: GameAssets -> PlayerState -> Animation -> Picture
drawOurPlayer assets ps anim =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    turretFrame = getCurrentFrame anim
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) (gaTankBody assets)
      , Rotate (radToDeg $ psTurretAngle ps) turretFrame
      ]

drawOtherPlayer :: GameAssets -> PlayerState -> Picture
drawOtherPlayer assets ps =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
    turretFrame = if null (gaTurretFrames assets)
                    then Blank
                    else head (gaTurretFrames assets)
  in
    Translate x y $ Pictures
      [ 
        Rotate (radToDeg $ psBodyAngle ps) (gaTankBody assets)
      , Rotate (radToDeg $ psTurretAngle ps) turretFrame
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