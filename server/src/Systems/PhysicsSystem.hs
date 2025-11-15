{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
module Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) where

import Core.Types (RoomGameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Bullet (BulletState(..))
import Types.Map (GameMap(..), isSolid)
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.List (nub) 
import Types.Tank (TankType(..))
import qualified Types.Tank as Tank

-- Tốc độ quay của người chơi (radians per second)
playerTurnSpeed :: Float
playerTurnSpeed = 1.5

-- Kích thước của mỗi ô trong bản đồ
tileSize :: Float
tileSize = 32.0

-- Chuyển đổi từ tọa độ thế giới sang tọa độ lưới
worldToGrid :: Vec2 -> (Int, Int)
worldToGrid (Vec2 x y) =
  ( floor (y / tileSize)
  , floor (x / tileSize)
  )

-- Kiểm tra xem ô tại tọa độ lưới có phải là ô rắn không
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

-- Kiểm tra xem vị trí thế giới có phải là vị trí rắn không
isPositionSolid :: GameMap -> Vec2 -> Bool
isPositionSolid gmap pos = isTileSolidAtGrid gmap (worldToGrid pos)

-- Bán kính của người chơi
playerRadius :: Float
playerRadius = 16.0 

-- Kiểm tra va chạm tại vị trí người chơi
isPositionColliding :: GameMap -> Vec2 -> Bool
isPositionColliding gmap pos =
  let
    (Vec2 x y) = pos
    r = playerRadius
    
    posTopLeft  = Vec2 (x - r) (y + r)
    posTopRight = Vec2 (x + r) (y + r)
    posBotLeft  = Vec2 (x - r) (y - r)
    posBotRight = Vec2 (x + r) (y - r)

    gridCoords = nub 
      [ worldToGrid posTopLeft
      , worldToGrid posTopRight
      , worldToGrid posBotLeft
      , worldToGrid posBotRight
      ]
      
  in
    any (isTileSolidAtGrid gmap) gridCoords

-- Cập nhật vật lý người chơi dựa trên các lệnh đã nhận
updatePlayerPhysics :: Float -> RoomGameState -> RoomGameState
updatePlayerPhysics dt gs =
  let
    gameMap = rgsMap gs
    cmdMap = Map.fromListWith (\new _ -> new) [(addr, cmd) | (Command addr cmd) <- rgsCommands gs]
    updatedPlayers = Map.mapWithKey (applyCommand dt gameMap) (rgsPlayers gs)
    
    applyCommand :: Float -> GameMap -> SockAddr -> PlayerState -> PlayerState
    applyCommand dt gmap addr ps =
      case Map.lookup addr cmdMap of
        Nothing -> ps
        Just (PlayerCommand moveVec angle _) -> updatePlayerState dt gmap ps moveVec angle
  in
    gs { rgsPlayers = updatedPlayers }

-- Cập nhật trạng thái người chơi dựa trên lệnh di chuyển và nòng súng
updatePlayerState :: Float -> GameMap -> PlayerState -> Vec2 -> Float -> PlayerState
updatePlayerState dt gmap ps moveVec angle =
  let
    movedPlayer = updatePlayerMovement dt gmap ps moveVec
  in
    movedPlayer { psTurretAngle = angle }

-- Cập nhật vị trí và góc của người chơi dựa trên lệnh di chuyển
updatePlayerMovement :: Float -> GameMap -> PlayerState -> Vec2 -> PlayerState
updatePlayerMovement dt gmap ps moveVec =
  let
    currentAngle = psBodyAngle ps
    rotInput = vecX moveVec
    turnAmount = rotInput * playerTurnSpeed * dt
    newBodyAngle = currentAngle + turnAmount
    
    throttle = vecY moveVec
    forwardVec = Vec2 (sin newBodyAngle) (cos newBodyAngle)

    baseSpeed = if psTankType ps == Tank.Blast
                  then 70.0  -- Tốc độ Tank Blast
                  else 100.0 -- Tốc độ Tank Rapid
                  
    effectiveSpeed = if throttle < 0 then baseSpeed * (1.0/3.0) else baseSpeed
    
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
    
    finalPos = if isPositionColliding gmap newPos
                 then psPosition ps 
                 else newPos        
                 
  in ps { psPosition = finalPos, psBodyAngle = newBodyAngle }

-- Cập nhật vật lý đạn
updateBulletPhysics :: Float -> RoomGameState -> RoomGameState
updateBulletPhysics dt gs =
  let
    updatedBullets = map (moveBullet dt) (rgsBullets gs)
  in
    gs { rgsBullets = updatedBullets }

-- Di chuyển đạn dựa trên vận tốc và thời gian sống còn lại
moveBullet :: Float -> BulletState -> BulletState
moveBullet dt b = b
  { bsPosition = bsPosition b + (bsVelocity b *^ dt)
  , bsLifetime = bsLifetime b - dt
  }

-- Lọc bỏ các thực thể đã chết khỏi trạng thái trò chơi
filterDeadEntities :: RoomGameState -> RoomGameState
filterDeadEntities gs =
  let
    gameMap = rgsMap gs
    aliveBullets = filter (\b -> bsLifetime b > 0 && not (isPositionSolid gameMap (bsPosition b))) (rgsBullets gs)
  in
    gs { rgsBullets = aliveBullets }