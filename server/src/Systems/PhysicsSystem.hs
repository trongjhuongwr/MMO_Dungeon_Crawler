{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use const" #-}
module Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) where

import Core.Types (RoomGameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), isSolid)
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import qualified Data.Array as Array
import Data.List (nub) 
import Types.Tank (TankType(..))
import qualified Types.Tank as Tank

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

    baseSpeed = if psTankType ps == Tank.Blast
                  then 70.0  -- Tốc độ Tank Blast
                  else 100.0 -- Tốc độ Tank Rapid
                  
    effectiveSpeed = if throttle < 0 then baseSpeed * (1.0/3.0) else baseSpeed
    
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))
    
    finalPos = if isPositionColliding gmap newPos
                 then psPosition ps 
                 else newPos        
                 
  in ps { psPosition = finalPos, psBodyAngle = newBodyAngle }

updateBulletPhysics :: Float -> RoomGameState -> RoomGameState
updateBulletPhysics dt gs =
  let
    updatedBullets = map (moveBullet dt) (rgsBullets gs)
  in
    gs { rgsBullets = updatedBullets }

moveBullet :: Float -> BulletState -> BulletState
moveBullet dt b = b
  { bsPosition = bsPosition b + (bsVelocity b *^ dt)
  , bsLifetime = bsLifetime b - dt
  }

filterDeadEntities :: RoomGameState -> RoomGameState
filterDeadEntities gs =
  let
    gameMap = rgsMap gs
    aliveBullets = filter (\b -> bsLifetime b > 0 && not (isPositionSolid gameMap (bsPosition b))) (rgsBullets gs)
    aliveEnemies = filter ((> 0) . esHealth) (rgsEnemies gs)
  in
    gs { rgsBullets = aliveBullets, rgsEnemies = aliveEnemies }