module Systems.AISystem (updateAI) where

import Core.Types (GameState(..))
import Types.Player (PlayerState(..))
import Types.Enemy (EnemyState(..))
import Types.Common (Vec2(..), (*^), vecLength)
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

aiSpeed :: Float
aiSpeed = 30.0 -- Chậm hơn người chơi một chút

-- Hàm chính, cập nhật AI cho tất cả quái
updateAI :: Float -> GameState -> GameState
updateAI dt gs =
  let
    players = Map.elems (gsPlayers gs)
    enemies = gsEnemies gs
  in
    -- Nếu không có người chơi, quái đứng yên
    if null players
      then gs
      else
        let
          -- Cập nhật từng con quái
          updatedEnemies = map (updateEnemyAI dt players) enemies
        in
          gs { gsEnemies = updatedEnemies }

-- Cập nhật AI cho một con quái
updateEnemyAI :: Float -> [PlayerState] -> EnemyState -> EnemyState
updateEnemyAI dt players enemy =
  let
    -- 1. Tìm người chơi gần nhất
    closestPlayer = minimumBy (comparing (distanceToEnemy enemy)) players
    playerPos = psPosition closestPlayer
    enemyPos = esPosition enemy
    
    -- 2. Tính vector hướng tới người chơi
    dirVec = playerPos - enemyPos
    dist = vecLength dirVec
    
    -- 3. Chuẩn hóa vector (normalize)
    -- (Tránh chia cho 0 nếu ở quá gần)
    normDir = if dist < 1.0
                then Vec2 0 0
                else dirVec *^ (1.0 / dist)
                
    -- 4. Tính toán vị trí mới
    -- (Lưu ý: Logic này chưa xử lý va chạm tường cho AI)
    -- (Để xử lý, chúng ta cần tích hợp nó vào PhysicsSystem)
    newPos = enemyPos + (normDir *^ (aiSpeed * dt))
    
  in
    -- Tạm thời, chúng ta chỉ cập nhật vị trí
    -- Giai đoạn sau: nên để AI set "vận tốc" và để PhysicsSystem xử lý
    enemy { esPosition = newPos }

-- Tính khoảng cách bình phương (nhanh hơn)
distanceToEnemy :: EnemyState -> PlayerState -> Float
distanceToEnemy e p =
  let (Vec2 dx dy) = psPosition p - esPosition e
  in dx*dx + dy*dy