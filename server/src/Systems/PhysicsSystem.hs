{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
module Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) where

import Core.Types (RoomGameState(..), Command(..)) -- << SỬA DÒNG NÀY
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
playerTurnSpeed = 1.5

tileSize :: Float
tileSize = 32.0

worldToGrid :: Vec2 -> (Int, Int)
worldToGrid (Vec2 x y) =
  ( floor (y / tileSize)
  , floor (x / tileSize)
  )

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

isPositionSolid :: GameMap -> Vec2 -> Bool
isPositionSolid gmap pos = isTileSolidAtGrid gmap (worldToGrid pos)

playerRadius :: Float
playerRadius = 16.0 

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

-- << SỬA CHỮ KÝ HÀM
updatePlayerPhysics :: Float -> RoomGameState -> RoomGameState
updatePlayerPhysics dt gs =
  let
    gameMap = rgsMap gs -- << SỬA
    cmdMap = Map.fromListWith (\new _ -> new) [(addr, cmd) | (Command addr cmd) <- rgsCommands gs] -- << SỬA
    updatedPlayers = Map.mapWithKey (applyCommand dt gameMap) (rgsPlayers gs) -- << SỬA
    
    applyCommand :: Float -> GameMap -> SockAddr -> PlayerState -> PlayerState
    applyCommand dt gmap addr ps =
      case Map.lookup addr cmdMap of
        Nothing -> ps
        Just (PlayerCommand moveVec angle _) -> updatePlayerState dt gmap ps moveVec angle
  in
    gs { rgsPlayers = updatedPlayers }

updatePlayerState :: Float -> GameMap -> PlayerState -> Vec2 -> Float -> PlayerState
updatePlayerState dt gmap ps moveVec angle =
  let
    movedPlayer = updatePlayerMovement dt gmap ps moveVec
  in
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
    
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
    
    finalPos = if isPositionColliding gmap newPos
                 then psPosition ps 
                 else newPos        
                 
  in ps { psPosition = finalPos, psBodyAngle = newBodyAngle }

-- << SỬA CHỮ KÝ HÀM
updateBulletPhysics :: Float -> RoomGameState -> RoomGameState
updateBulletPhysics dt gs =
  let
    updatedBullets = map (moveBullet dt) (rgsBullets gs) -- << SỬA
  in
    gs { rgsBullets = updatedBullets }

moveBullet :: Float -> BulletState -> BulletState
moveBullet dt b = b
  { bsPosition = bsPosition b + (bsVelocity b *^ dt)
  , bsLifetime = bsLifetime b - dt
  }

-- << SỬA CHỮ KÝ HÀM
filterDeadEntities :: RoomGameState -> RoomGameState
filterDeadEntities gs =
  let
    gameMap = rgsMap gs -- << SỬA
    aliveBullets = filter (\b -> bsLifetime b > 0 && not (isPositionSolid gameMap (bsPosition b))) (rgsBullets gs) -- << SỬA
    aliveEnemies = filter ((> 0) . esHealth) (rgsEnemies gs) -- << SỬA
  in
    gs { rgsBullets = aliveBullets, rgsEnemies = aliveEnemies }