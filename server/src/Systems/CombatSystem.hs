module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

import Core.Types (GameState(..), Command(..))
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

resolveCollisions :: GameState -> GameState
resolveCollisions = checkCollisions

spawnNewBullets :: GameState -> GameState
spawnNewBullets gs =
  let
    (gsWithNewBullets, newNextId) =
      go (gsCommands gs) (gsPlayers gs) (gsNextId gs) gs
  in
    gsWithNewBullets { gsNextId = newNextId }
  where
    go :: [Command] -> Map.Map SockAddr PlayerState -> Int -> GameState -> (GameState, Int)
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
            
            newGameState = currentGs { gsBullets = newBullet : gsBullets currentGs }
            newNextId = nextId + 1
          in
            go cmds players newNextId newGameState

checkCollisions :: GameState -> GameState
checkCollisions gs =
  let
    bullets = gsBullets gs
    enemies = gsEnemies gs
    players = gsPlayers gs
    
    (collidedBulletIds_Enemies, collidedEnemyIds) = findEnemyCollisions bullets enemies
    
    (collidedBulletIds_Players, updatedPlayersMap) = findPlayerCollisions bullets players
    
    allCollidedBulletIds = nub (collidedBulletIds_Enemies ++ collidedBulletIds_Players)
    
    remainingBullets = filter (\b -> bsId b `notElem` allCollidedBulletIds) bullets
    remainingEnemies = map (damageEnemy collidedEnemyIds) enemies

  in
    gs { gsBullets = remainingBullets
       , gsEnemies = remainingEnemies 
       , gsPlayers = updatedPlayersMap 
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

-- SỬA HÀM NÀY (Đã sửa từ Bước 2.3)
damagePlayers :: [(Int, Bullet.BulletType)] -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
damagePlayers damageList playersMap =
  foldl (flip applyDamage) playersMap damageList
  where
    applyDamage :: (Int, Bullet.BulletType) -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
    applyDamage (attackedPlayerId, bulletType) pMap =
      Map.map (damagePlayer attackedPlayerId bulletType) pMap
      
    damagePlayer :: Int -> Bullet.BulletType -> PlayerState -> PlayerState
    damagePlayer attackedPlayerId bulletType player =
      if psId player /= attackedPlayerId || psHealth player <= 0 -- Nếu không phải player bị bắn, HOẶC player đã chết, bỏ qua
        then player 
        else
          let damage = case bulletType of
                         Bullet.Normal -> 10
                         Bullet.Blast  -> 30
              newHealth = psHealth player - damage
          in 
            if newHealth <= 0
              -- SỬA LOGIC: Hết máu -> Trừ 1 mạng, không để máu âm
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