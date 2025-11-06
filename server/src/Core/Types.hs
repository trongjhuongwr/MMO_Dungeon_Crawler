module Core.Types where

import Network.Socket (SockAddr)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap)
import Types.Tank (TankType) 
import qualified Data.Map as Map  


data GameState = GameState
  { gsTick     :: Int
  , gsCommands :: [Command]
  , gsPlayers  :: Map.Map SockAddr PlayerState
  , gsEnemies  :: [EnemyState]
  , gsBullets  :: [BulletState]
  , gsNextId   :: Int 
  , gsMap      :: GameMap
  , gsSpawns   :: [Vec2]
  }

data Command = Command SockAddr PlayerCommand

initialGameState :: GameMap -> [Vec2] -> GameState
initialGameState loadedMap spawnPoints = GameState
  { gsTick = 0
  , gsCommands = []
  , gsPlayers = Map.empty
  , gsEnemies = [] 
  , gsBullets = []
  , gsNextId = 1
  , gsMap = loadedMap     
  , gsSpawns = spawnPoints 
  }

initialPlayerState :: Vec2 -> Int -> TankType -> PlayerState
initialPlayerState spawnPos playerId tankType = PlayerState
  { 
    psId = playerId
  , psPosition = spawnPos
  , psBodyAngle = 0.0
  , psTurretAngle = 0.0
  , psHealth = 100
  , psTankType = tankType
  , psLives = 3
  }