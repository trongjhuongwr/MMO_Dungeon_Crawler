module Core.Types where

import Network.Socket (SockAddr)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap)
import qualified Data.Map as Map  -- Dùng Map để quản lý Player


data GameState = GameState
  { gsTick     :: Int
  , gsCommands :: [Command]
  , gsPlayers  :: Map.Map SockAddr PlayerState -- Dùng Map (SockAddr làm key)
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
  , gsEnemies = -- TODO: Tải enemy spawns từ metadata của map
      [ EnemyState { esId = 1, esPosition = Vec2 (25 * 32) (25 * 32), esHealth = 10 }
      , EnemyState { esId = 2, esPosition = Vec2 (48 * 32) (24 * 32), esHealth = 10 }
      , EnemyState { esId = 3, esPosition = Vec2 (48 * 32) (26 * 32), esHealth = 10 }
      , EnemyState { esId = 4, esPosition = Vec2 (85 * 32) (25 * 32), esHealth = 50 }
      ]
  , gsBullets = []
  , gsNextId = 5
  , gsMap = loadedMap     
  , gsSpawns = spawnPoints 
  }

initialPlayerState :: Vec2 -> PlayerState
initialPlayerState spawnPos = PlayerState
  { 
    psPosition = spawnPos
  , psBodyAngle = 0.0
  , psTurretAngle = 0.0
  , psHealth = 100
  }