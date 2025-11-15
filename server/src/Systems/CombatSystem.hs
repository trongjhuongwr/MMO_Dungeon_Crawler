{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module Systems.CombatSystem (spawnNewBullets, resolveCollisions) where

import Core.Types (RoomGameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Bullet (BulletState(..))
import qualified Types.Bullet as Bullet
import Types.Common (Vec2(..), (*^), vecLength)
import Types.Tank (TankType(..))
import qualified Types.Tank as Tank
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import Data.List (nub) 

bulletSpeed :: Float
bulletSpeed = 300.0  -- Tốc độ viên đạn

bulletLifetime :: Float
bulletLifetime = 2.0 -- Viên đạn tồn tại trong 2 giây

rapidCooldown :: Float
rapidCooldown = 0.2  -- 0.2 giây (5 viên/giây)

blastCooldown :: Float
blastCooldown = 1.0  -- 1.0 giây (1 viên/giây)

-- Xử lý va chạm giữa đạn và người chơi
resolveCollisions :: RoomGameState -> RoomGameState
resolveCollisions = checkCollisions

-- Tạo đạn mới dựa trên lệnh bắn từ người chơi
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
                  -- Bắn đạn mới
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

-- Kiểm tra va chạm giữa đạn và người chơi
checkCollisions :: RoomGameState -> RoomGameState
checkCollisions gs =
  let
    bullets = rgsBullets gs
    players = rgsPlayers gs
    
    (collidedBulletIds_Players, updatedPlayersMap) = findPlayerCollisions bullets players
    
    allCollidedBulletIds = nub collidedBulletIds_Players
    
    remainingBullets = filter (\b -> bsId b `notElem` allCollidedBulletIds) bullets

  in
    gs { rgsBullets = remainingBullets
       , rgsPlayers = updatedPlayersMap
       }

-- Tìm và xử lý va chạm giữa đạn và người chơi
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

-- Áp dụng sát thương cho người chơi dựa trên danh sách sát thương
damagePlayers :: [(Int, Bullet.BulletType)] -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
damagePlayers damageList playersMap =
  foldl (flip applyDamage) playersMap damageList
  where
    -- Áp dụng sát thương cho tất cả người chơi
    applyDamage :: (Int, Bullet.BulletType) -> Map.Map SockAddr PlayerState -> Map.Map SockAddr PlayerState
    applyDamage (attackedPlayerId, bulletType) pMap =
      Map.map (damagePlayer attackedPlayerId bulletType) pMap
    
    -- Áp dụng sát thương cho một người chơi cụ thể
    damagePlayer :: Int -> Bullet.BulletType -> PlayerState -> PlayerState
    damagePlayer attackedPlayerId bulletType player =
      if psId player /= attackedPlayerId || psHealth player <= 0 
        then player
        else
          let damage = case bulletType of
                         Bullet.Normal -> 4  -- Sát thương của đạn thường
                         Bullet.Blast  -> 25 -- Sát thương của đạn nổ
              newHealth = psHealth player - damage
          in
            -- Cập nhật máu và số mạng
            if newHealth <= 0
              then player { psHealth = 0, psLives = psLives player - 1 }
              else player { psHealth = newHealth }

-- Kiểm tra va chạm giữa đạn và người chơi
isPlayerColliding :: (BulletState, PlayerState) -> Bool
isPlayerColliding (bullet, player) =
  let
    (Vec2 bx by) = bsPosition bullet
    (Vec2 px py) = psPosition player
    
    playerHalfWidth = 10.0 -- Nửa kích thước người chơi
    bulletHalfWidth = 2.0  -- Nửa kích thước viên đạn
    
    collidesX = abs (bx - px) < (playerHalfWidth + bulletHalfWidth)
    collidesY = abs (by - py) < (playerHalfWidth + bulletHalfWidth)
  in
    collidesX && collidesY