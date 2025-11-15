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

-- ID dành cho bot
botPlayerId :: Int
botPlayerId = -1

-- Kích thước ô trong server (pixel)
serverTileSize :: Float
serverTileSize = 32.0

-- Cập nhật AI của bot
updateBotAI :: Float -> RoomGameState -> RoomGameState
updateBotAI dt gs
  | rgsMode gs /= PvE = gs 
  | otherwise =
      case (findBot gs, findHuman gs) of
        (Just (botFakeAddr, botState), Just (humanFakeAddr, humanState)) ->
          let
            gameMap = rgsMap gs
            GameMap { gmapWidth = gw, gmapHeight = gh } = gameMap
            mapCenter = Vec2 (fromIntegral gw * serverTileSize / 2.0)
                             (fromIntegral gh * serverTileSize / 2.0)

            botCmd = calculateBotAction dt (rgsCurrentTime gs) botState humanState mapCenter

            newCommands = Command botFakeAddr botCmd : rgsCommands gs
            
          in
            gs { rgsCommands = newCommands }
            
        _ -> gs 

-- Tính toán hành động của bot dựa trên trạng thái hiện tại
calculateBotAction :: Float -> Float -> PlayerState -> PlayerState -> Vec2 -> PlayerCommand
calculateBotAction dt currentTime botState humanState mapCenter =
  let
    targetVec = psPosition humanState - psPosition botState
    targetDist = vecLength targetVec
    
    sightRange = 400.0       -- Tầm nhìn của Bot (pixel)
    keepDistance_Max = 300.0 -- Giữ khoảng cách (xa nhất)
    keepDistance_Min = 200.0 -- Giữ khoảng cách (gần nhất)
    
    isEngaging = targetDist < sightRange

  in
    if isEngaging
      then
        let
          -- Tính góc nòng súng hướng về mục tiêu
          turretAngle = atan2 (vecX targetVec) (vecY targetVec)

          -- Tính góc thân xe để hướng về mục tiêu
          targetBodyAngle = turretAngle 
          currentBodyAngle = psBodyAngle botState
          
          -- Tính sự khác biệt góc và chuẩn hóa
          deltaAngle = targetBodyAngle - currentBodyAngle
          normDelta = atan2 (sin deltaAngle) (cos deltaAngle)
          
          -- Xoay về phía mục tiêu
          turn = if normDelta > 0.1
                   then 1.0
                   else if normDelta < -0.1
                     then -1.0
                     else 0.0
          
          -- Giữ khoảng cách với mục tiêu
          throttle = if targetDist > keepDistance_Max
                       then 1.0
                       else if targetDist < keepDistance_Min
                         then -0.5
                         else 0.0
          
          -- Tạo vector di chuyển
          moveVec = Vec2 turn throttle

          -- Cơ chế bắn theo đợt
          burstCycleTime = 3.5
          burstFireDuration = 0.75

          -- Tính thời gian trong chu kỳ bắn
          timeInCycle = double2Float $ snd $ properFraction (float2Double currentTime / float2Double burstCycleTime)
          isBurstFiring = (timeInCycle * burstCycleTime) < burstFireDuration
          
          -- Kiểm tra thời gian hồi chiêu
          (cooldown, _) = getTankStats botState
          isWeaponReady = (currentTime - psLastFireTime botState) >= cooldown
          
          -- Quyết định có bắn hay không
          canFire = isEngaging && isBurstFiring && isWeaponReady
          
        in
          -- Tạo lệnh cho bot
          PlayerCommand moveVec turretAngle canFire
      
      else 
        let
          turretAngle = psBodyAngle botState

          --  Hướng về trung tâm 
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

          canFire = False 
          
        in
          PlayerCommand moveVec turretAngle canFire


-- HÀM TIỆN ÍCH
-- Tìm bot
findBot :: RoomGameState -> Maybe (SockAddr, PlayerState)
findBot gs = listToMaybe $ filter (\(_, p) -> psId p == botPlayerId) (Map.assocs (rgsPlayers gs))

-- Tìm người (không phải bot)
findHuman :: RoomGameState -> Maybe (SockAddr, PlayerState)
findHuman gs = listToMaybe $ filter (\(_, p) -> psId p /= botPlayerId) (Map.assocs (rgsPlayers gs))

-- Lấy thông số tank (giống CombatSystem)
getTankStats :: PlayerState -> (Float, Bullet.BulletType)
getTankStats p = case psTankType p of
                    Tank.Rapid -> (rapidCooldown, Bullet.Normal)
                    Tank.Blast -> (blastCooldown, Bullet.Blast)

-- Thời gian hồi chiêu cho từng loại tank
rapidCooldown :: Float
rapidCooldown = 0.2 -- 0.2 giây (5 viên/giây)

blastCooldown :: Float
blastCooldown = 1.0 -- 1 giây (1 viên/giây)