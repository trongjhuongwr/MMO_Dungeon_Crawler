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
import Data.List (nub) 

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

-- ĐỔI TÊN HÀM (và giữ nguyên): Kiểm tra 1 ô tile CỤ THỂ
isTileSolidAtGrid :: GameMap -> (Int, Int) -> Bool
isTileSolidAtGrid gmap (gy, gx) =
  let
    (yMin, xMin) = fst (Array.bounds (gmapTiles gmap))
    (yMax, xMax) = snd (Array.bounds (gmapTiles gmap))
    
    isOutOfBounds = gy < yMin || gy > yMax || gx < xMin || gx > xMax
  in
    if isOutOfBounds
      then True 
      else
        let tile = (gmapTiles gmap) Array.! (gy, gx)
        in isSolid tile

-- HÀM CŨ (isPositionSolid): Giữ nguyên để check đạn (va chạm 1 điểm)
isPositionSolid :: GameMap -> Vec2 -> Bool
isPositionSolid gmap pos = isTileSolidAtGrid gmap (worldToGrid pos)

-- HÀM MỚI: Định nghĩa bán kính hitbox (32x32)
playerRadius :: Float
playerRadius = 16.0 -- (1/2 tileSize, vì tâm ở giữa)

-- HÀM MỚI: Kiểm tra va chạm Bounding Box
isPositionColliding :: GameMap -> Vec2 -> Bool
isPositionColliding gmap pos =
  let
    (Vec2 x y) = pos
    r = playerRadius
    
    -- Lấy 4 điểm ở 4 góc của bounding box
    posTopLeft  = Vec2 (x - r) (y + r)
    posTopRight = Vec2 (x + r) (y + r)
    posBotLeft  = Vec2 (x - r) (y - r)
    posBotRight = Vec2 (x + r) (y - r)

    -- Chuyển 4 điểm thành các ô grid (dùng nub để loại bỏ trùng lặp)
    gridCoords = nub 
      [ worldToGrid posTopLeft
      , worldToGrid posTopRight
      , worldToGrid posBotLeft
      , worldToGrid posBotRight
      ]
      
  in
    -- Nếu BẤT KỲ ô nào trong các ô đó là solid, thì có va chạm
    any (isTileSolidAtGrid gmap) gridCoords

-- ... (updatePlayerPhysics, applyCommand, updatePlayerState không đổi) ...
updatePlayerPhysics :: Float -> GameState -> GameState
updatePlayerPhysics dt gs =
  let
    gameMap = gsMap gs 
    cmdMap = Map.fromListWith (\new _ -> new) [(addr, cmd) | (Command addr cmd) <- gsCommands gs]
    updatedPlayers = Map.mapWithKey (applyCommand dt gameMap) (gsPlayers gs)
    
    applyCommand :: Float -> GameMap -> SockAddr -> PlayerState -> PlayerState
    applyCommand dt gmap addr ps =
      case Map.lookup addr cmdMap of
        Nothing -> ps
        Just (PlayerCommand moveVec angle _) -> updatePlayerState dt gmap ps moveVec angle
  in
    gs { gsPlayers = updatedPlayers }

updatePlayerState :: Float -> GameMap -> PlayerState -> Vec2 -> Float -> PlayerState
updatePlayerState dt gmap ps moveVec angle =
  let
    movedPlayer = updatePlayerMovement dt gmap ps moveVec
  in
    movedPlayer { psTurretAngle = angle }

-- SỬA ĐỔI: updatePlayerMovement
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
    
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
    
    -- SỬA ĐỔI VA CHẠM:
    finalPos = if isPositionColliding gmap newPos
                 then psPosition ps -- Nếu va chạm, giữ vị trí cũ
                 else newPos        -- Nếu không, cập nhật
                 
  in ps { psPosition = finalPos, psBodyAngle = newBodyAngle }

-- ... (updateBulletPhysics, moveBullet không đổi) ...
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

-- SỬA ĐỔI: filterDeadEntities (để đảm bảo nó dùng va chạm 1 điểm cho đạn)
filterDeadEntities :: GameState -> GameState
filterDeadEntities gs =
  let
    gameMap = gsMap gs
    -- Lọc đạn: hết giờ HOẶC va chạm tường (dùng isPositionSolid 1 điểm)
    aliveBullets = filter (\b -> bsLifetime b > 0 && not (isPositionSolid gameMap (bsPosition b))) (gsBullets gs)
    aliveEnemies = filter ((> 0) . esHealth) (gsEnemies gs)
  in
    gs { gsBullets = aliveBullets, gsEnemies = aliveEnemies }