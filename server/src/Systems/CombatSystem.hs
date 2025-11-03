module Systems.CombatSystem (resolveCombat) where

import Core.Types (GameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Network.Socket (SockAddr)
import qualified Data.Map as Map

bulletSpeed :: Float
bulletSpeed = 300.0

bulletLifetime :: Float
bulletLifetime = 2.0 -- Đạn sống trong 2 giây

-- | Hàm chính xử lý combat: tạo đạn mới và kiểm tra va chạm
resolveCombat :: GameState -> GameState
resolveCombat gs =
  let
    -- 1. Tạo đạn mới từ các lệnh "bắn"
    (gsWithNewBullets, newNextId) = spawnNewBullets gs (gsCommands gs) (gsPlayers gs) (gsNextId gs)
    
    -- 2. Kiểm tra va chạm
    gsAfterCollisions = checkCollisions gsWithNewBullets
  in
    gsAfterCollisions { gsNextId = newNextId }

-- | Tạo đạn mới
spawnNewBullets :: GameState -> [Command] -> Map.Map SockAddr PlayerState -> Int -> (GameState, Int)
spawnNewBullets gs [] _ nextId = (gs, nextId) -- Hết lệnh
spawnNewBullets gs (Command addr (PlayerCommand _ _ False) : cmds) players nextId =
  spawnNewBullets gs cmds players nextId -- Lệnh này không bắn
spawnNewBullets gs (Command addr (PlayerCommand _ _ True) : cmds) players nextId =
  -- Lệnh này CÓ bắn
  case Map.lookup addr players of
    Nothing -> spawnNewBullets gs cmds players nextId -- Player không tồn tại?
    Just player ->
      let
        angle = psTurretAngle player
        vel = Vec2 (sin angle) (cos angle) *^ bulletSpeed
        pos = psPosition player + (vel *^ 0.05) -- Spawn hơi lệch về phía trước
        
        newBullet = BulletState
          { bsId = nextId
          , bsPosition = pos
          , bsVelocity = vel
          , bsLifetime = bulletLifetime
          }
        
        newGameState = gs { gsBullets = newBullet : gsBullets gs }
        newNextId = nextId + 1
      in
        spawnNewBullets newGameState cmds players newNextId

-- | Kiểm tra và xử lý va chạm
checkCollisions :: GameState -> GameState
checkCollisions gs =
  let
    bullets = gsBullets gs
    enemies = gsEnemies gs
    
    -- Tìm ID của đạn và quái đã va chạm
    (collidedBulletIds, collidedEnemyIds) = findCollisions bullets enemies
    
    -- Lọc bỏ đạn đã va chạm
    remainingBullets = filter (\b -> bsId b `notElem` collidedBulletIds) bullets
    
    -- Lọc bỏ (hoặc giảm máu) quái đã va chạm
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies
    -- (Trong Giai đoạn 2, chúng ta xóa luôn quái)
    -- remainingEnemies = filter (\e -> esId e `notElem` collidedEnemyIds) enemies

  in
    gs { gsBullets = remainingBullets, gsEnemies = remainingEnemies }

-- (Đây là logic giảm máu, sẽ dùng ở Giai đoạn 2.2)
damageEnemy :: [Int] -> EnemyState -> EnemyState
damageEnemy collidedIds enemy =
  if esId enemy `elem` collidedIds
    then enemy { esHealth = esHealth enemy - 1 } -- Giảm 1 máu
    else enemy

-- | Tìm các cặp va chạm (O(n*m) - không tối ưu nhưng đủ dùng)
findCollisions :: [BulletState] -> [EnemyState] -> ([Int], [Int])
findCollisions bullets enemies =
  let
    pairs = [(b, e) | b <- bullets, e <- enemies]
    collisions = filter (isColliding) pairs
    
    collidedBulletIds = map (bsId . fst) collisions
    collidedEnemyIds  = map (esId . snd) collisions
  in
    (collidedBulletIds, collidedEnemyIds)

-- | Kiểm tra va chạm AABB (hình chữ nhật) đơn giản
isColliding :: (BulletState, EnemyState) -> Bool
isColliding (bullet, enemy) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 ex ey) = esPosition enemy
    
    -- Giả sử quái có kích thước 20x20 (vì ta vẽ bán kính 10)
    enemyHalfWidth = 10.0
    -- Giả sử đạn có kích thước 4x4
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - ex) < (enemyHalfWidth + bulletHalfWidth)
    collidesY = abs (by - ey) < (enemyHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY