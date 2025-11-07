{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

import Core.Types (RoomGameState(..), Command(..)) -- << SỬA DÒNG NÀY
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Bullet (BulletState(..))
import qualified Types.Bullet as Bullet
import Types.Enemy (EnemyState(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Tank (TankType(..))
import qualified Types.Tank as Tank
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import Data.List (nub) 

bulletSpeed :: Float
bulletSpeed = 300.0

bulletLifetime :: Float
bulletLifetime = 1.0

-- << SỬA CHỮ KÝ HÀM
resolveCollisions :: RoomGameState -> RoomGameState
resolveCollisions = checkCollisions

-- << SỬA CHỮ KÝ HÀM
spawnNewBullets :: RoomGameState -> RoomGameState
spawnNewBullets gs =
  let
    (gsWithNewBullets, newNextId) =
      go (rgsCommands gs) (rgsPlayers gs) (rgsNextId gs) gs -- << SỬA
  in
    gsWithNewBullets { rgsNextId = newNextId }
  where
    go :: [Command] -> Map.Map SockAddr PlayerState -> Int -> RoomGameState -> (RoomGameState, Int) -- << SỬA
    go [] _ nextId currentGs = (currentGs, nextId)
    go (Command addr (PlayerCommand _ _ False) : cmds) players nextId currentGs =
      go cmds players nextId currentGs
    go (Command addr (PlayerCommand _ _ True) : cmds) players nextId currentGs =
      case Map.lookup addr players of
        Nothing -> go cmds players nextId currentGs
        Just player ->
          let
            angle = psTurretAngle player
            vel = Vec2 (sin angle) (cos angle) *^ bulletSpeed
            pos = psPosition player + (vel *^ 0.05)
            
            newBulletType = case psTankType player of
                              Tank.Rapid -> Bullet.Normal
                              Tank.Blast -> Bullet.Blast
            
            newBullet = BulletState
              { bsId = nextId
              , bsOwnerId = psId player
              , bsBulletType = newBulletType 
              , bsPosition = pos
              , bsVelocity = vel
              , bsLifetime = bulletLifetime
              }
            
            newGameState = currentGs { rgsBullets = newBullet : rgsBullets currentGs } -- << SỬA
            newNextId = nextId + 1
          in
            go cmds players newNextId newGameState

-- << SỬA CHỮ KÝ HÀM
checkCollisions :: RoomGameState -> RoomGameState
checkCollisions gs =
  let
    bullets = rgsBullets gs -- << SỬA
    enemies = rgsEnemies gs -- << SỬA
    players = rgsPlayers gs -- << SỬA
    
    (collidedBulletIds_Enemies, collidedEnemyIds) = findEnemyCollisions bullets enemies
    
    (collidedBulletIds_Players, updatedPlayersMap) = findPlayerCollisions bullets players
    
    allCollidedBulletIds = nub (collidedBulletIds_Enemies ++ collidedBulletIds_Players)
    
    remainingBullets = filter (\b -> bsId b `notElem` allCollidedBulletIds) bullets
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies

  in
    gs { rgsBullets = remainingBullets -- << SỬA
       , rgsEnemies = remainingEnemies -- << SỬA
       , rgsPlayers = updatedPlayersMap -- << SỬA
       }

damageEnemy :: [Int] -> EnemyState -> EnemyState
damageEnemy collidedIds enemy =
  if esId enemy `elem` collidedIds
    then enemy { esHealth = esHealth enemy - 1 }
    else enemy

findEnemyCollisions :: [BulletState] -> [EnemyState] -> ([Int], [Int])
findEnemyCollisions bullets enemies =
  let
    pairs = [(b, e) | b <- bullets, e <- enemies]
    collisions = filter isEnemyColliding pairs
    
    collidedBulletIds = map (bsId . fst) collisions
    collidedEnemyIds  = map (esId . snd) collisions
  in
    (collidedBulletIds, collidedEnemyIds)

isEnemyColliding :: (BulletState, EnemyState) -> Bool
isEnemyColliding (bullet, enemy) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 ex ey) = esPosition enemy
    
    enemyHalfWidth = 10.0
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - ex) < (enemyHalfWidth + bulletHalfWidth)
    collidesY = abs (by - ey) < (enemyHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY

findPlayerCollisions :: [BulletState] -> Map.Map SockAddr PlayerState -> ([Int], Map.Map SockAddr PlayerState)
findPlayerCollisions bullets playersMap =
  let
    playerList = Map.elems playersMap
    
    pairs = [ (b, p) | b <- bullets, p <- playerList, bsOwnerId b /= psId p ]
            
    collisions = filter isPlayerColliding pairs
    
    collidedBulletIds = nub (map (bsId . fst) collisions)
    
    playerDamageList :: [(Int, Bullet.BulletType)]
    playerDamageList = map (\(b, p) -> (psId p, bsBulletType b)) collisions
    
    updatedPlayersMap = damagePlayers playerDamageList playersMap
    
  in
    (collidedBulletIds, updatedPlayersMap)

damagePlayers :: [(Int, Bullet.BulletType)] -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
damagePlayers damageList playersMap =
  foldl (flip applyDamage) playersMap damageList
  where
    applyDamage :: (Int, Bullet.BulletType) -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
    applyDamage (attackedPlayerId, bulletType) pMap =
      Map.map (damagePlayer attackedPlayerId bulletType) pMap
      
    damagePlayer :: Int -> Bullet.BulletType -> PlayerState -> PlayerState
    damagePlayer attackedPlayerId bulletType player =
      if psId player /= attackedPlayerId || psHealth player <= 0 
        then player 
        else
          let damage = case bulletType of
                         Bullet.Normal -> 5
                         Bullet.Blast  -> 15
              newHealth = psHealth player - damage
          in 
            if newHealth <= 0
              then player { psHealth = 0, psLives = psLives player - 1 }
              else player { psHealth = newHealth }

isPlayerColliding :: (BulletState, PlayerState) -> Bool
isPlayerColliding (bullet, player) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 px py) = psPosition player
    
    playerHalfWidth = 10.0 
    bulletHalfWidth = 2.0
    
    collidesX = abs (bx - px) < (playerHalfWidth + bulletHalfWidth)
    collidesY = abs (by - py) < (playerHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY