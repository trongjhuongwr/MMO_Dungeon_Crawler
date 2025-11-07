module Systems.AISystem (updateAI) where

import Core.Types (RoomGameState(..)) -- << SỬA DÒNG NÀY
import Types.Player (PlayerState(..))
import Types.Enemy (EnemyState(..))
import Types.Common (Vec2(..), (*^), vecLength)
import qualified Data.Map as Map
import Data.List (minimumBy)
import Data.Ord (comparing)

aiSpeed :: Float
aiSpeed = 30.0 

-- << SỬA CHỮ KÝ HÀM
updateAI :: Float -> RoomGameState -> RoomGameState
updateAI dt gs =
  let
    players = Map.elems (rgsPlayers gs) -- << SỬA
    enemies = rgsEnemies gs -- << SỬA
  in
    if null players
      then gs
      else
        let
          updatedEnemies = map (updateEnemyAI dt players) enemies
        in
          gs { rgsEnemies = updatedEnemies }

updateEnemyAI :: Float -> [PlayerState] -> EnemyState -> EnemyState
updateEnemyAI dt players enemy =
  let
    closestPlayer = minimumBy (comparing (distanceToEnemy enemy)) players
    playerPos = psPosition closestPlayer
    enemyPos = esPosition enemy
    
    dirVec = playerPos - enemyPos
    dist = vecLength dirVec
    
    normDir = if dist < 1.0
                then Vec2 0 0
                else dirVec *^ (1.0 / dist)
                
    newPos = enemyPos + (normDir *^ (aiSpeed * dt))
    
  in
    enemy { esPosition = newPos }

distanceToEnemy :: EnemyState -> PlayerState -> Float
distanceToEnemy e p =
  let (Vec2 dx dy) = psPosition p - esPosition e
  in dx*dx + dy*dy