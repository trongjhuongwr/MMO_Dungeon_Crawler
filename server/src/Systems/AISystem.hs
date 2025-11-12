{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use find" #-}
module Systems.AISystem (updateBotAI, botPlayerId) where

import Core.Types
import Types.Player
import Types.Common
import Types.GameMode
import Types.Tank
import qualified Types.Tank as Tank
import qualified Types.Bullet as Bullet
import Network.Socket (SockAddr)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.List (find)
import Data.Ord (comparing)
import Types.Map (GameMap(..))
import GHC.Float (float2Double, double2Float)

-- ID đặc biệt cho Bot
botPlayerId :: Int
botPlayerId = -1

-- Kích thước tile (để tính trung tâm map)
serverTileSize :: Float
serverTileSize = 32.0

-- | Hàm chính: được gọi mỗi tick trong GameLoop
updateBotAI :: Float -> RoomGameState -> RoomGameState
updateBotAI dt gs
  | rgsMode gs /= PvE = gs -- Chỉ chạy ở chế độ PvE
  | otherwise =
      case (findBot gs, findHuman gs) of
        (Just (botFakeAddr, botState), Just (humanFakeAddr, humanState)) ->
          let
            -- Tính toán trung tâm bản đồ
            gameMap = rgsMap gs
            GameMap { gmapWidth = gw, gmapHeight = gh } = gameMap
            mapCenter = Vec2 (fromIntegral gw * serverTileSize / 2.0)
                             (fromIntegral gh * serverTileSize / 2.0)

            -- 1. Tính toán hành động (chỉ trả về Command)
            botCmd = calculateBotAction dt (rgsCurrentTime gs) botState humanState mapCenter

            -- 2. "Tiêm" command vào danh sách
            newCommands = Command botFakeAddr botCmd : rgsCommands gs
            
          in
            -- 3. Để CombatSystem xử lý việc cập nhật psLastFireTime
            gs { rgsCommands = newCommands }
            
        _ -> gs -- Không tìm thấy bot hoặc người

-- | Tính toán hành động (trả về PlayerCommand)
calculateBotAction :: Float -> Float -> PlayerState -> PlayerState -> Vec2 -> PlayerCommand
calculateBotAction dt currentTime botState humanState mapCenter =
  let
    -- --- 1. Tính toán mục tiêu ---
    targetVec = psPosition humanState - psPosition botState
    targetDist = vecLength targetVec
    
    sightRange = 400.0 -- Tầm nhìn của Bot (pixel)
    keepDistance_Max = 300.0 -- Giữ khoảng cách (xa nhất)
    keepDistance_Min = 200.0 -- Giữ khoảng cách (gần nhất)
    
    isEngaging = targetDist < sightRange

  in
    if isEngaging
      then -- === 2. LOGIC CHIẾN ĐẤU (Engaging) ===
        let
          -- 2a. Nòng súng (Turret)
          turretAngle = atan2 (vecX targetVec) (vecY targetVec)

          -- 2b. Thân xe (Body) - Di chuyển và giữ khoảng cách
          targetBodyAngle = turretAngle 
          currentBodyAngle = psBodyAngle botState
          
          deltaAngle = targetBodyAngle - currentBodyAngle
          normDelta = atan2 (sin deltaAngle) (cos deltaAngle)

          turn = if normDelta > 0.1
                   then 1.0
                   else if normDelta < -0.1
                     then -1.0
                     else 0.0
          
          throttle = if targetDist > keepDistance_Max
                       then 1.0
                       else if targetDist < keepDistance_Min
                         then -0.5
                         else 0.0
          
          moveVec = Vec2 turn throttle

          -- === 2c. Bắn (LOGIC BURST FIRE MỚI) ===
          
          -- Tổng thời gian 1 đợt bắn (burst cycle) và thời gian bắn trong đợt đó
          burstCycleTime = 3.5
          burstFireDuration = 0.75

          -- Tính toán thời gian trong chu kỳ hiện tại (dùng fmod)
          -- fmod yêu cầu Double, nên chúng ta cần chuyển đổi
          timeInCycle = double2Float $ snd $ properFraction (float2Double currentTime / float2Double burstCycleTime)
          isBurstFiring = (timeInCycle * burstCycleTime) < burstFireDuration

          -- Kiểm tra cooldown vũ khí (như cũ)
          (cooldown, _) = getTankStats botState
          isWeaponReady = (currentTime - psLastFireTime botState) >= cooldown
          
          -- Điều kiện bắn cuối cùng
          canFire = isEngaging && isBurstFiring && isWeaponReady
          
        in
          PlayerCommand moveVec turretAngle canFire
      
      else -- === 3. LOGIC TUẦN TRA (Patrolling) ===
        let
          -- 3a. Nòng súng (Turret)
          turretAngle = psBodyAngle botState

          -- 3b. Thân xe (Body) - Hướng về trung tâm + "lắc lư"
          vecToCenter = mapCenter - (psPosition botState)
          angleToCenter = atan2 (vecX vecToCenter) (vecY vecToCenter)
          randomPerturbation = sin (currentTime * 0.3) * (pi / 3.0) 
          targetBodyAngle = angleToCenter + randomPerturbation
          
          currentBodyAngle = psBodyAngle botState
          deltaAngle = targetBodyAngle - currentBodyAngle
          normDelta = atan2 (sin deltaAngle) (cos deltaAngle)

          turn = if normDelta > 0.1
                   then 1.0
                   else if normDelta < -0.1
                     then -1.0 
                     else 0.0
          
          distToCenter = vecLength vecToCenter
          throttle = if distToCenter > 100.0 then 0.5 else 0.0
          
          moveVec = Vec2 turn throttle

          -- 3c. Bắn
          canFire = False -- Không bắn khi tuần tra
          
        in
          PlayerCommand moveVec turretAngle canFire

-- Helper: Tìm bot
findBot :: RoomGameState -> Maybe (SockAddr, PlayerState)
findBot gs = listToMaybe $ filter (\(_, p) -> psId p == botPlayerId) (Map.assocs (rgsPlayers gs))

-- Helper: Tìm người (không phải bot)
findHuman :: RoomGameState -> Maybe (SockAddr, PlayerState)
findHuman gs = listToMaybe $ filter (\(_, p) -> psId p /= botPlayerId) (Map.assocs (rgsPlayers gs))

-- Helper: Lấy thông số tank (giống CombatSystem)
getTankStats :: PlayerState -> (Float, Bullet.BulletType)
getTankStats p = case psTankType p of
                    Tank.Rapid -> (rapidCooldown, Bullet.Normal)
                    Tank.Blast -> (blastCooldown, Bullet.Blast)

rapidCooldown :: Float
rapidCooldown = 0.2

blastCooldown :: Float
blastCooldown = 1.0