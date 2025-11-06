module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

import Core.Types (GameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import Data.List (nub) -- <-- THÊM IMPORT

bulletSpeed :: Float
bulletSpeed = 300.0

bulletLifetime :: Float
bulletLifetime = 2.0

-- | (Tách ra từ resolveCombat): Chỉ xử lý va chạm
resolveCollisions :: GameState -> GameState
resolveCollisions = checkCollisions -- <-- Đổi tên từ checkEnemyCollisions

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
            
            -- SỬA ĐỔI: Thêm Owner ID
            newBullet = BulletState
              { bsId = nextId
              , bsOwnerId = psId player -- <-- GÁN ID CỦA NGƯỜI BẮN
              , bsPosition = pos
              , bsVelocity = vel
              , bsLifetime = bulletLifetime
              }
            
            newGameState = currentGs { gsBullets = newBullet : gsBullets currentGs }
            newNextId = nextId + 1
          in
            go cmds players newNextId newGameState

-- | KIỂM TRA VA CHẠM (Cả Quái và Player)
checkCollisions :: GameState -> GameState
checkCollisions gs =
  let
    bullets = gsBullets gs
    enemies = gsEnemies gs
    players = gsPlayers gs
    
    -- Va chạm với quái (giữ nguyên)
    (collidedBulletIds_Enemies, collidedEnemyIds) = findEnemyCollisions bullets enemies
    
    -- Va chạm với người chơi (MỚI)
    (collidedBulletIds_Players, updatedPlayersMap) = findPlayerCollisions bullets players
    
    -- Gộp 2 danh sách đạn đã va chạm
    allCollidedBulletIds = nub (collidedBulletIds_Enemies ++ collidedBulletIds_Players)
    
    -- Lọc
    remainingBullets = filter (\b -> bsId b `notElem` allCollidedBulletIds) bullets
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies -- Giữ nguyên

  in
    -- Cập nhật state với player đã bị trừ máu
    gs { gsBullets = remainingBullets
       , gsEnemies = remainingEnemies 
       , gsPlayers = updatedPlayersMap -- <-- CẬP NHẬT MAP PLAYER
       }

damageEnemy :: [Int] -> EnemyState -> EnemyState
damageEnemy collidedIds enemy =
  if esId enemy `elem` collidedIds
    then enemy { esHealth = esHealth enemy - 1 }
    else enemy

-- Đổi tên hàm
findEnemyCollisions :: [BulletState] -> [EnemyState] -> ([Int], [Int])
findEnemyCollisions bullets enemies =
  let
    pairs = [(b, e) | b <- bullets, e <- enemies]
    collisions = filter isEnemyColliding pairs
    
    collidedBulletIds = map (bsId . fst) collisions
    collidedEnemyIds  = map (esId . snd) collisions
  in
    (collidedBulletIds, collidedEnemyIds)

-- Đổi tên hàm
isEnemyColliding :: (BulletState, EnemyState) -> Bool
isEnemyColliding (bullet, enemy) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 ex ey) = esPosition enemy
    
    -- Kích thước hitbox (giữ nguyên)
    enemyHalfWidth = 10.0
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - ex) < (enemyHalfWidth + bulletHalfWidth)
    collidesY = abs (by - ey) < (enemyHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY

-- ===== CÁC HÀM MỚI CHO PVP =====

-- | Tìm va chạm giữa Đạn và Người chơi
findPlayerCollisions :: [BulletState] -> Map.Map SockAddr PlayerState -> ([Int], Map.Map SockAddr PlayerState)
findPlayerCollisions bullets playersMap =
  let
    playerList = Map.elems playersMap
    
    -- 1. Tìm tất cả các cặp (Đạn, Người chơi) CÓ THỂ va chạm
    --    (Loại trừ trường hợp đạn va chạm với chính người bắn ra nó)
    pairs = [ (b, p) | b <- bullets, p <- playerList, bsOwnerId b /= psId p ]
            
    -- 2. Lọc ra những cặp thực sự va chạm
    collisions = filter isPlayerColliding pairs
    
    -- 3. Lấy ID của đạn và người chơi
    collidedBulletIds = nub (map (bsId . fst) collisions)
    collidedPlayerIds = nub (map (psId . snd) collisions)
    
    -- 4. Trừ máu của người chơi bị va chạm
    updatedPlayersMap = damagePlayers collidedPlayerIds playersMap
    
  in
    (collidedBulletIds, updatedPlayersMap)

-- | Trừ máu người chơi (áp dụng trên Map)
damagePlayers :: [Int] -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
damagePlayers collidedPlayerIds playersMap =
  Map.map (applyDamage) playersMap
  where
    applyDamage :: PlayerState -> PlayerState
    applyDamage player =
      if psId player `elem` collidedPlayerIds
        -- Trừ 10 máu cho mỗi lần trúng đạn
        then player { psHealth = psHealth player - 10 }
        else player

-- | Kiểm tra va chạm AABB giữa (Đạn, Người chơi)
isPlayerColliding :: (BulletState, PlayerState) -> Bool
isPlayerColliding (bullet, player) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 px py) = psPosition player
    
    -- Kích thước hitbox (giống quái, nhưng có thể điều chỉnh)
    playerHalfWidth = 10.0 -- (Giả sử 20x20)
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - px) < (playerHalfWidth + bulletHalfWidth)
    collidesY = abs (by - py) < (playerHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY