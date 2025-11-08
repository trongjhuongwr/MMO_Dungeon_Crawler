module Game
  ( updateGame
  , updateSnapshot
  , initialWorldSnapshot
  , dummyAnim
  ) where

import Types (InGameState(..))
import Network.Packet (WorldSnapshot(..), PlayerCommand(..))
import Types.Player (PlayerCommand(..))
import Types.Bullet (BulletState(..))
import Types.Common (Vec2(..))
import qualified Data.Set as Set
import Data.Maybe (Maybe(..))

import Core.Effect (Effect(..), makeExplosion, updateEffect, isEffectFinished)
import Core.Animation (Animation(..), updateAnimation, startAnimation)
import Input (KeyMap, calculateMoveVector)
import Renderer.Resources (Resources(..))

-- Hàm helper
initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot [] [] []

dummyAnim :: Resources -> Animation
dummyAnim assets = Animation (resTurretFramesRapid assets) 0.05 0 8 False

-- Xử lý snapshot
updateSnapshot :: Resources -> InGameState -> WorldSnapshot -> InGameState
updateSnapshot assets gdata newSnapshot =
  let
    oldWorld = igsWorld gdata
    oldBullets = wsBullets oldWorld
    newBulletIds = Set.fromList (map bsId (wsBullets newSnapshot))
    disappearedBullets = filter (\b -> bsId b `Set.notMember` newBulletIds) oldBullets
    
    (newNextId, newEffects) = foldl makeEffect (igsNextEffectId gdata, []) disappearedBullets
      where
        makeEffect :: (Int, [Effect]) -> BulletState -> (Int, [Effect])
        makeEffect (nextId, effects) bullet =
          let effect = makeExplosion nextId (resExplosionFrames assets) (bsPosition bullet) --
          in (nextId + 1, effect : effects)
        
  in
    gdata { igsWorld = newSnapshot, igsEffects = igsEffects gdata ++ newEffects, igsNextEffectId = newNextId }

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
    mathAngle = atan2 mouseY mouseX 
    glossAngle = mathAngle - (pi / 2)
    
    command = PlayerCommand
      { pcMoveVec     = moveVec
      , pcTurretAngle = -glossAngle 
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