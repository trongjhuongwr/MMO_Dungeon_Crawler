{-# LANGUAGE ScopedTypeVariables #-}
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

-- ID đặc biệt cho Bot
botPlayerId :: Int
botPlayerId = -1

-- | Hàm chính: được gọi mỗi tick trong GameLoop
updateBotAI :: Float -> RoomGameState -> RoomGameState
updateBotAI dt gs
  | rgsMode gs /= PvE = gs -- Chỉ chạy ở chế độ PvE
  | otherwise =
      case (findBot gs, findHuman gs) of
        (Just (botFakeAddr, botState), Just (humanFakeAddr, humanState)) ->
          let
            -- 1. Tính toán hành động
            (botCmd, updatedBotState) = calculateBotAction dt (rgsCurrentTime gs) botState humanState

            -- 2. "Tiêm" command vào danh sách
            newCommands = Command botFakeAddr botCmd : rgsCommands gs
            
            -- 3. Cập nhật trực tiếp state của bot (ví dụ: lastFireTime)
            newPlayers = Map.insert botFakeAddr updatedBotState (rgsPlayers gs)
            
          in
            gs { rgsCommands = newCommands, rgsPlayers = newPlayers }
            
        _ -> gs -- Không tìm thấy bot hoặc người

-- | Tính toán hành động (ĐÃ CẬP NHẬT)
calculateBotAction :: Float -> Float -> PlayerState -> PlayerState -> (PlayerCommand, PlayerState)
calculateBotAction dt currentTime botState humanState =
  let
    -- 1. Tính toán vector và khoảng cách
    targetVec = psPosition humanState - psPosition botState
    targetDist = vecLength targetVec
    
    -- 2. Tính toán góc (Turret)
    -- Bot luôn nhắm nòng súng (turret) vào người chơi
    turretAngle = atan2 (vecX targetVec) (vecY targetVec)
    
    -- 3. Tính toán di chuyển (Thân xe)
    
    -- Góc mà thân xe *nên* hướng tới (giống hệt góc nòng súng)
    targetBodyAngle = turretAngle
    currentBodyAngle = psBodyAngle botState
    
    -- Tính toán góc delta ngắn nhất (ví dụ: -30 độ thay vì +330 độ)
    deltaAngle = targetBodyAngle - currentBodyAngle
    normDelta = atan2 (sin deltaAngle) (cos deltaAngle)

    -- Quyết định xoay (turn) dựa trên góc delta
    -- Thêm một khoảng đệm (0.1 rad) để tránh bot "rung lắc" khi đã nhắm trúng
    turn = if normDelta > 0.1
             then 1.0  -- Xoay phải
             else if normDelta < -0.1
               then -1.0 -- Xoay trái
               else 0.0  -- Đứng yên

    -- Quyết định di chuyển (throttle)
    -- Cố gắng giữ khoảng cách (ví dụ: 150-250 units)
    throttle = if targetDist > 250.0
                 then 1.0  -- Tiến tới
                 else if targetDist < 150.0
                   then -0.5 -- Lùi lại
                   else 0.0  -- Giữ khoảng cách

    moveVec = Vec2 turn throttle
    
    -- 4. Bắn (nếu cooldown)
    (cooldown, _) = getTankStats botState
    canFire = (currentTime - psLastFireTime botState) >= cooldown
    
    -- 5. Cập nhật state nếu bắn
    updatedBotState = if canFire
                        then botState { psLastFireTime = currentTime }
                        else botState

    command = PlayerCommand moveVec turretAngle canFire
  in
    (command, updatedBotState)

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