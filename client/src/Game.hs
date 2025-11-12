module Game
  ( updateGame
  , updateSnapshot
  , initialWorldSnapshot
  , dummyAnim
  ) where

import Types (InGameState(..))
import Network.Packet (WorldSnapshot(..), PlayerCommand(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Bullet (BulletState(..))
import Types.Common (Vec2(..))
import qualified Data.Set as Set
import Data.Maybe (Maybe(..))

import Core.Effect (Effect(..), makeExplosion, updateEffect, isEffectFinished)
import Core.Animation (Animation(..), updateAnimation, startAnimation)
import Input (KeyMap, calculateMoveVector)
import Data.List (find)
import Renderer.Resources (Resources(..))
import qualified Types.Tank as Tank

-- Hàm helper
initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot [] []

dummyAnim :: Resources -> Animation
dummyAnim assets = Animation (resTurretFramesRapid assets) 0.05 0 8 False

-- Xử lý snapshot
updateSnapshot :: Resources -> InGameState -> WorldSnapshot -> InGameState
updateSnapshot assets gdata newSnapshot =
  let
    -- --- Logic cũ (giữ nguyên) ---
    oldWorld = igsWorld gdata
    oldBullets = wsBullets oldWorld
    newBulletIds = Set.fromList (map bsId (wsBullets newSnapshot))
    disappearedBullets = filter (\b -> bsId b `Set.notMember` newBulletIds) oldBullets
    
    (newNextId, newEffects) = foldl makeEffect (igsNextEffectId gdata, []) disappearedBullets
      where
        makeEffect :: (Int, [Effect]) -> BulletState -> (Int, [Effect])
        makeEffect (nextId, effects) bullet =
          let effect = makeExplosion nextId (resExplosionFrames assets) (bsPosition bullet)
          in (nextId + 1, effect : effects)
          
    -- --- LOGIC MỚI: Kích hoạt animation dựa trên state từ server ---
    
    -- 1. Tìm state của chính mình trong snapshot mới
    mMyPlayerState = find (\p -> psId p == igsMyId gdata) (wsPlayers newSnapshot)
    
    -- 2. Lấy thời gian bắn mới từ server (nếu không tìm thấy, giữ thời gian cũ)
    newServerFireTime = maybe (igsLastFireTime gdata) psLastFireTime mMyPlayerState
    
    -- 3. Kiểm tra xem server đã ghi nhận cú bắn mới chưa
    shotFired = newServerFireTime > igsLastFireTime gdata
    
    -- 4. Lấy loại tank (để biết chạy animation nào)
    myTankType = maybe Tank.Rapid psTankType mMyPlayerState
    
    -- 5. Cập nhật animation nếu `shotFired` là True
    (newAnimRapid, newAnimBlast) =
      if shotFired
        then case myTankType of
               Tank.Rapid -> (startAnimation (igsTurretAnimRapid gdata), igsTurretAnimBlast gdata)
               Tank.Blast -> (igsTurretAnimRapid gdata, startAnimation (igsTurretAnimBlast gdata))
        else
               (igsTurretAnimRapid gdata, igsTurretAnimBlast gdata) -- Giữ nguyên animation cũ    
  in
    gdata { igsWorld = newSnapshot
          , igsEffects = igsEffects gdata ++ newEffects
          , igsNextEffectId = newNextId
          -- [THAY ĐỔI] Cập nhật các trường mới
          , igsLastFireTime = newServerFireTime -- Lưu lại thời gian mới nhất
          , igsTurretAnimRapid = newAnimRapid
          , igsTurretAnimBlast = newAnimBlast
          }

-- Update trong game (code cũ)
updateGame :: Float -> InGameState -> (InGameState, Maybe PlayerCommand)
updateGame dt gdata =
  let
    updatedEffects = map (updateEffect dt) (igsEffects gdata)
    activeEffects = filter (not . isEffectFinished) updatedEffects
    newTurretAnimRapid = updateAnimation dt (igsTurretAnimRapid gdata)
    newTurretAnimBlast = updateAnimation dt (igsTurretAnimBlast gdata)
    
    -- Tạo command
    moveVec = calculateMoveVector (igsKeys gdata)
    (mouseX, mouseY) = igsMousePos gdata

    -- === SỬA LỖI TÍNH TOÁN GÓC ===
    -- Vì camera không còn xoay, (mouseX, mouseY) là tọa độ tương đối
    -- so với người chơi (ở tâm màn hình).
    (pcAngle, _) = 
        let 
          -- Tính góc trực tiếp từ tâm (0,0) đến chuột
          mathAngle = atan2 mouseY mouseX
          -- Chuyển đổi từ hệ tọa độ (0 = +X) sang Gloss (0 = +Y)
          glossAngle = mathAngle - (pi / 2)
        in
          -- Gửi góc đã đảo ngược (theo logic render của Gloss)
          (-glossAngle, True)

    command = PlayerCommand
      { pcMoveVec     = moveVec
      , pcTurretAngle = pcAngle
      , pcDidFire     = igsDidFire gdata
      }
      
    newState = gdata 
      { igsEffects = activeEffects
      , igsDidFire = False
      , igsTurretAnimRapid = newTurretAnimRapid
      , igsTurretAnimBlast = newTurretAnimBlast
      }
  in
    (newState, Just command)