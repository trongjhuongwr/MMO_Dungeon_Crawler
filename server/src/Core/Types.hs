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
  , gsEnemies = [] -- <-- TẠM THỜI TẮT QUÁI
  , gsBullets = []
  , gsNextId = 1 -- <-- BẮT ĐẦU ID TỪ 1
  , gsMap = loadedMap     
  , gsSpawns = spawnPoints 
  }

-- SỬA HÀM NÀY
initialPlayerState :: Vec2 -> Int -> PlayerState
initialPlayerState spawnPos playerId = PlayerState
  { 
    psId = playerId -- <-- GÁN ID
  , psPosition = spawnPos
  , psBodyAngle = 0.0
  , psTurretAngle = 0.0
  , psHealth = 100
  }