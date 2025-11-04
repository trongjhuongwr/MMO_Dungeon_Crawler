module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

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
bulletLifetime = 2.0

-- | (Tách ra từ resolveCombat): Chỉ xử lý va chạm
resolveCollisions :: GameState -> GameState
resolveCollisions = checkCollisions

-- | (Tách ra từ resolveCombat): Chỉ tạo đạn
spawnNewBullets :: GameState -> GameState
spawnNewBullets gs =
  let
    (gsWithNewBullets, newNextId) =
      go (gsCommands gs) (gsPlayers gs) (gsNextId gs) gs
  in
    gsWithNewBullets { gsNextId = newNextId }
  where
    go :: [Command] -> Map.Map SockAddr PlayerState -> Int -> GameState -> (GameState, Int)
    go [] _ nextId currentGs = (currentGs, nextId) -- Hết lệnh
    go (Command addr (PlayerCommand _ _ False) : cmds) players nextId currentGs =
      go cmds players nextId currentGs -- Lệnh này không bắn
    go (Command addr (PlayerCommand _ _ True) : cmds) players nextId currentGs =
      case Map.lookup addr players of
        Nothing -> go cmds players nextId currentGs -- Player không tồn tại?
        Just player ->
          let
            angle = psTurretAngle player
            vel = Vec2 (sin angle) (cos angle) *^ bulletSpeed
            pos = psPosition player + (vel *^ 0.05)
            
            newBullet = BulletState
              { bsId = nextId
              , bsPosition = pos
              , bsVelocity = vel
              , bsLifetime = bulletLifetime
              }
            
            newGameState = currentGs { gsBullets = newBullet : gsBullets currentGs }
            newNextId = nextId + 1
          in
            go cmds players newNextId newGameState

-- | Kiểm tra và xử lý va chạm
checkCollisions :: GameState -> GameState
checkCollisions gs =
  let
    bullets = gsBullets gs
    enemies = gsEnemies gs
    
    (collidedBulletIds, collidedEnemyIds) = findCollisions bullets enemies
    
    remainingBullets = filter (\b -> bsId b `notElem` collidedBulletIds) bullets
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies

  in
    gs { gsBullets = remainingBullets, gsEnemies = remainingEnemies }

damageEnemy :: [Int] -> EnemyState -> EnemyState
damageEnemy collidedIds enemy =
  if esId enemy `elem` collidedIds
    then enemy { esHealth = esHealth enemy - 1 }
    else enemy

findCollisions :: [BulletState] -> [EnemyState] -> ([Int], [Int])
findCollisions bullets enemies =
  let
    pairs = [(b, e) | b <- bullets, e <- enemies]
    collisions = filter isColliding pairs
    
    collidedBulletIds = map (bsId . fst) collisions
    collidedEnemyIds  = map (esId . snd) collisions
  in
    (collidedBulletIds, collidedEnemyIds)

isColliding :: (BulletState, EnemyState) -> Bool
isColliding (bullet, enemy) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 ex ey) = esPosition enemy
    
    enemyHalfWidth = 10.0
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - ex) < (enemyHalfWidth + bulletHalfWidth)
    collidesY = abs (by - ey) < (enemyHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY