{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

import Core.Types (RoomGameState(..), Command(..))
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
bulletLifetime = 2.0

rapidCooldown :: Float
rapidCooldown = 0.2 -- 0.2 giây (5 viên/giây)

blastCooldown :: Float
blastCooldown = 1.0 -- 1.0 giây (1 viên/giây)

resolveCollisions :: RoomGameState -> RoomGameState
resolveCollisions = checkCollisions

spawnNewBullets :: Float -> RoomGameState -> RoomGameState
spawnNewBullets currentTime gs =
  let
    (gsWithNewBullets, newNextId) =
      go (rgsCommands gs) (rgsNextId gs) gs
  in
    gsWithNewBullets { rgsNextId = newNextId }
  where
    go :: [Command] -> Int -> RoomGameState -> (RoomGameState, Int)
    go [] nextId currentGs = (currentGs, nextId)
    go (Command addr (PlayerCommand _ _ False) : cmds) nextId currentGs =
      go cmds nextId currentGs
    go (Command addr (PlayerCommand _ _ True) : cmds) nextId currentGs =
      let
        players = rgsPlayers currentGs
      in
        case Map.lookup addr players of
          Nothing -> go cmds nextId currentGs -- Player không tồn tại
          Just player ->
            let
              (cooldown, bulletType) = case psTankType player of
                                         Tank.Rapid -> (rapidCooldown, Bullet.Normal)
                                         Tank.Blast -> (blastCooldown, Bullet.Blast)
              
              canFire = (currentTime - psLastFireTime player) > cooldown
            in
              if not canFire
                then go cmds nextId currentGs -- Vẫn đang cooldown
                else
                  -- Bắn!
                  let
                    angle = psTurretAngle player
                    vel = Vec2 (sin angle) (cos angle) *^ bulletSpeed
                    pos = psPosition player + (vel *^ 0.05)
                    
                    newBullet = BulletState
                      { bsId = nextId
                      , bsOwnerId = psId player
                      , bsBulletType = bulletType
                      , bsPosition = pos
                      , bsVelocity = vel
                      , bsLifetime = bulletLifetime
                      }
                    
                    -- Cập nhật thời gian bắn cuối cùng
                    updatedPlayer = player { psLastFireTime = currentTime }
                    newPlayers = Map.insert addr updatedPlayer players
                    newGameState = currentGs { rgsBullets = newBullet : rgsBullets currentGs, rgsPlayers = newPlayers }
                    newNextId = nextId + 1
                  in
                    go cmds newNextId newGameState

checkCollisions :: RoomGameState -> RoomGameState
checkCollisions gs =
  let
    bullets = rgsBullets gs
    enemies = rgsEnemies gs
    players = rgsPlayers gs
    
    (collidedBulletIds_Enemies, collidedEnemyIds) = findEnemyCollisions bullets enemies
    
    (collidedBulletIds_Players, updatedPlayersMap) = findPlayerCollisions bullets players
    
    allCollidedBulletIds = nub (collidedBulletIds_Enemies ++ collidedBulletIds_Players)
    
    remainingBullets = filter (\b -> bsId b `notElem` allCollidedBulletIds) bullets
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies

  in
    gs { rgsBullets = remainingBullets
       , rgsEnemies = remainingEnemies
       , rgsPlayers = updatedPlayersMap
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
                         Bullet.Normal -> 4
                         Bullet.Blast  -> 25
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