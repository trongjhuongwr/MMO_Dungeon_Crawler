module Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) where

import Core.Types (GameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), isSolid)
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import qualified Data.Array as Array

playerSpeed :: Float
playerSpeed = 100.0

playerTurnSpeed :: Float
playerTurnSpeed = 1.0

tileSize :: Float
tileSize = 32.0

worldToGrid :: Vec2 -> (Int, Int)
worldToGrid (Vec2 x y) =
  ( floor (y / tileSize)
  , floor (x / tileSize)
  )

isPositionSolid :: GameMap -> Vec2 -> Bool
isPositionSolid gmap pos =
  let
    (gy, gx) = worldToGrid pos
    (yMin, xMin) = fst (Array.bounds (gmapTiles gmap))
    (yMax, xMax) = snd (Array.bounds (gmapTiles gmap))
    
    -- Kiểm tra xem có ngoài biên map không
    isOutOfBounds = gy < yMin || gy > yMax || gx < xMin || gx > xMax
  in
    if isOutOfBounds
      then True -- Ngoài biên coi như là tường
      else
        let tile = (gmapTiles gmap) Array.! (gy, gx)
        in isSolid tile

-- | Cập nhật vật lý cho TẤT CẢ người chơi dựa trên lệnh
updatePlayerPhysics :: Float -> GameState -> GameState
updatePlayerPhysics dt gs =
  let
    gameMap = gsMap gs -- Lấy map
    cmdMap = Map.fromListWith (\new _ -> new) [(addr, cmd) | (Command addr cmd) <- gsCommands gs]
    
    -- Cập nhật từng player trong state
    updatedPlayers = Map.mapWithKey (applyCommand dt gameMap) (gsPlayers gs)
    
    -- Hàm áp dụng lệnh (GIỜ CÓ THÊM gameMap)
    applyCommand :: Float -> GameMap -> SockAddr -> PlayerState -> PlayerState
    applyCommand dt gmap addr ps =
      case Map.lookup addr cmdMap of
        Nothing -> ps -- Không có lệnh, giữ nguyên
        Just (PlayerCommand moveVec angle _) -> updatePlayerState dt gmap ps moveVec angle
  in
    gs { gsPlayers = updatedPlayers }

-- | Cập nhật trạng thái của một người chơi
updatePlayerState :: Float -> GameMap -> PlayerState -> Vec2 -> Float -> PlayerState
updatePlayerState dt gmap ps moveVec angle =
  let
    -- Cập nhật di chuyển và va chạm
    movedPlayer = updatePlayerMovement dt gmap ps moveVec
  in
    -- Cập nhật góc nòng súng
    movedPlayer { psTurretAngle = angle }



updatePlayerMovement :: Float -> GameMap -> PlayerState -> Vec2 -> PlayerState
updatePlayerMovement dt gmap ps moveVec =
  let
    currentAngle = psBodyAngle ps
    rotInput = vecX moveVec
    turnAmount = rotInput * playerTurnSpeed * dt
    newBodyAngle = currentAngle + turnAmount
    
    throttle = vecY moveVec
    forwardVec = Vec2 (sin newBodyAngle) (cos newBodyAngle)
    effectiveSpeed = if throttle < 0 then playerSpeed * (1.0/3.0) else playerSpeed
    
    -- Tính toán vị trí mới
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
    
    -- KIỂM TRA VA CHẠM
    finalPos = if isPositionSolid gmap newPos
                 then psPosition ps -- Nếu vị trí mới bị chặn, giữ nguyên vị trí cũ
                 else newPos        -- Nếu không, di chuyển
                 
  in ps { psPosition = finalPos, psBodyAngle = newBodyAngle }


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
  , bsLifetime = bsLifetime b - dt
  }

filterDeadEntities :: GameState -> GameState
filterDeadEntities gs =
  let
    gameMap = gsMap gs
    -- Lọc đạn: hết giờ HOẶC va chạm tường
    aliveBullets = filter (\b -> bsLifetime b > 0 && not (isPositionSolid gameMap (bsPosition b))) (gsBullets gs)
    aliveEnemies = filter ((> 0) . esHealth) (gsEnemies gs)
    -- (Sau này thêm lọc người chơi chết)
  in
    gs { gsBullets = aliveBullets, gsEnemies = aliveEnemies }