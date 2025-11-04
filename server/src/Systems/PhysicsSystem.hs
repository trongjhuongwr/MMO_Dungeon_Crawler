module Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) where

import Core.Types (GameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))   -- <--- THÊM IMPORT NÀY
import Network.Socket (SockAddr)
import qualified Data.Map as Map

playerSpeed :: Float
playerSpeed = 100.0

playerTurnSpeed :: Float
playerTurnSpeed = 1.0

-- | Cập nhật vật lý cho TẤT CẢ người chơi dựa trên lệnh
updatePlayerPhysics :: Float -> GameState -> GameState
updatePlayerPhysics dt gs =
  let
    -- Xây dựng Map các lệnh cho từng người chơi (lấy lệnh cuối cùng nếu có nhiều)
    cmdMap = Map.fromListWith (\new _ -> new) [(addr, cmd) | (Command addr cmd) <- gsCommands gs]
    
    -- Cập nhật từng player trong state
    updatedPlayers = Map.mapWithKey (applyCommand dt) (gsPlayers gs)
    
    -- Hàm áp dụng lệnh
    applyCommand :: Float -> SockAddr -> PlayerState -> PlayerState
    applyCommand dt addr ps =
      case Map.lookup addr cmdMap of
        Nothing -> ps -- Không có lệnh, giữ nguyên
        Just (PlayerCommand moveVec angle _) -> updatePlayerState dt ps moveVec angle
  in
    gs { gsPlayers = updatedPlayers }

-- | Cập nhật trạng thái của một người chơi
updatePlayerState :: Float -> PlayerState -> Vec2 -> Float -> PlayerState
updatePlayerState dt ps moveVec angle =
  let
    movedPlayer = updatePlayerMovement dt ps moveVec
  in
    movedPlayer { psTurretAngle = angle }

-- (Hàm updatePlayerMovement không đổi, giữ nguyên như cũ)
updatePlayerMovement :: Float -> PlayerState -> Vec2 -> PlayerState
updatePlayerMovement dt ps moveVec =
  let
    currentAngle = psBodyAngle ps
    rotInput = vecX moveVec
    turnAmount = rotInput * playerTurnSpeed * dt
    newBodyAngle = currentAngle + turnAmount
    throttle = vecY moveVec
    forwardVec = Vec2 (sin newBodyAngle) (cos newBodyAngle)
    effectiveSpeed = if throttle < 0 then playerSpeed * (1.0/3.0) else playerSpeed
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
  in ps { psPosition = newPos, psBodyAngle = newBodyAngle }


-- | HÀM MỚI: Cập nhật vật lý cho đạn
updateBulletPhysics :: Float -> GameState -> GameState
updateBulletPhysics dt gs =
  let
    updatedBullets = map (moveBullet dt) (gsBullets gs)
  in
    gs { gsBullets = updatedBullets }

moveBullet :: Float -> BulletState -> BulletState
moveBullet dt b = b
  { bsPosition = bsPosition b + (bsVelocity b *^ dt)
  , bsLifetime = bsLifetime b - dt -- Giảm thời gian sống
  }

-- | HÀM MỚI: Lọc bỏ các thực thể đã "chết" (ví dụ: đạn hết giờ)
filterDeadEntities :: GameState -> GameState
filterDeadEntities gs =
  let
    aliveBullets = filter ((> 0) . bsLifetime) (gsBullets gs)
    aliveEnemies = filter ((> 0) . esHealth) (gsEnemies gs) -- (Lỗi "esHealth" đã được fix bởi import)
  in
    gs { gsBullets = aliveBullets, gsEnemies = aliveEnemies }