module Core.Renderer (render, GameAssets(..)) where

import Graphics.Gloss
import Network.Packet (WorldSnapshot(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))

-- | Định nghĩa cấu trúc cho assets
data GameAssets = GameAssets
  { gaTankBody   :: Picture
  , gaTankTurret :: Picture
  , gaBullet     :: Picture
  }

-- | Hàm render chính, nhận vào assets và snapshot
render :: GameAssets -> WorldSnapshot -> Picture
render assets snapshot =
  Pictures $
    map (drawPlayer assets) (wsPlayers snapshot) ++
    map drawEnemy (wsEnemies snapshot) ++
    map (drawBullet assets) (wsBullets snapshot)

-- | Vẽ một người chơi (xe tăng)
drawPlayer :: GameAssets -> PlayerState -> Picture
drawPlayer assets ps =
  let
    (x, y) = (vecX $ psPosition ps, vecY $ psPosition ps)
  in
    Translate x y $ Pictures
      [ -- Vẽ thân xe
        Rotate (radToDeg $ psBodyAngle ps) (gaTankBody assets)
        -- Vẽ nòng súng
      , Rotate (radToDeg $ psTurretAngle ps) (gaTankTurret assets)
      ]

-- | Vẽ một viên đạn
drawBullet :: GameAssets -> BulletState -> Picture
drawBullet assets bullet =
  let
    (x, y) = (vecX $ bsPosition bullet, vecY $ bsPosition bullet)
    -- Đạn bay theo hướng vận tốc, ta cần xoay ảnh
    angle = atan2 (vecY $ bsVelocity bullet) (vecX $ bsVelocity bullet)
  in
    Translate x y $
    -- psTurretAngle (độ) = (90 - radToDeg angle)
    Rotate (90 - radToDeg angle) $
    Scale 0.25 0.25 (gaBullet assets) -- Thu nhỏ ảnh đạn nếu cần

-- | Vẽ một quái (hiện tại là hình tròn đỏ)
drawEnemy :: EnemyState -> Picture
drawEnemy enemy =
  let
    (x, y) = (vecX $ esPosition enemy, vecY $ esPosition enemy)
  in
    Translate x y $ Color red (Circle 10) -- Bán kính 10

radToDeg :: Float -> Float
radToDeg r = r * 180 / pi