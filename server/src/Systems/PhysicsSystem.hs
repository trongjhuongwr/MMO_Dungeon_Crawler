module Systems.PhysicsSystem (updatePhysics) where

import Core.Types (GameState(..), Command(..))
import Types.Player (PlayerState(..), PlayerCommand(..))
import Types.Common (Vec2(..), (*^), vecLength)
import Network.Socket (SockAddr)

playerSpeed :: Float
playerSpeed = 100.0 -- pixels/second

playerTurnSpeed :: Float
playerTurnSpeed = 1.0 -- radians/second

updatePhysics :: Float -> GameState -> GameState
updatePhysics dt gs =
  let
    updatedPlayers = foldr (applyCommand dt) (gsPlayers gs) (gsCommands gs)
  in
    gs { gsPlayers = updatedPlayers }

-- Áp dụng một lên danh sách người chơi
applyCommand :: Float -> Command -> [PlayerState] -> [PlayerState]
applyCommand dt (Command addr pcmd) players =
  -- Hiện tại, chúng ta chỉ có một người chơi, nên cứ cập nhật người chơi đó
  -- Sau này, khi có nhiều người chơi, chúng ta sẽ cần tìm đúng người chơi dựa trên `addr`
  case players of
    [] -> []
    (p:ps) -> updatePlayerState dt p pcmd : ps

-- Cập nhật trạng thái của một người
updatePlayerState :: Float -> PlayerState -> PlayerCommand -> PlayerState
updatePlayerState dt ps (Move moveVec) =
  updatePlayerMovement dt ps moveVec
updatePlayerState _ ps (Aim angle) =
  ps { psTurretAngle = angle }
updatePlayerState dt ps (MoveAndAim moveVec angle) =
  let movedPlayer = updatePlayerMovement dt ps moveVec
  in movedPlayer { psTurretAngle = angle }

updatePlayerMovement :: Float -> PlayerState -> Vec2 -> PlayerState
updatePlayerMovement dt ps moveVec =
  let
    --  W/S = forward/back throttle (y axis)
    --  A/D = continuous rotation left/right (x axis magnitude)

    currentAngle = psBodyAngle ps

    -- Rotation input comes from x component of moveVec. Positive x -> rotate right, negative -> rotate left
    rotInput = vecX moveVec
    turnAmount = rotInput * playerTurnSpeed * dt
    newBodyAngle = currentAngle + turnAmount

    -- Throttle (forward/back) comes from y component of moveVec
    throttle = vecY moveVec -- in range -1..1

    -- Body forward vector (assuming angle 0 points upwards)
    forwardVec = Vec2 (sin newBodyAngle) (cos newBodyAngle)

    -- Apply forward/back movement along the body's forward vector
    effectiveSpeed = if throttle < 0 then playerSpeed * (1.0/3.0) else playerSpeed
    newPos = psPosition ps + (forwardVec *^ (throttle * effectiveSpeed * dt))

  in ps { psPosition = newPos, psBodyAngle = newBodyAngle }
