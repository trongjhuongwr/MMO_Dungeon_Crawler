module Core.Types
  ( module Core.Types -- Export tất cả mọi thứ
  ) where

import Network.Socket (SockAddr, Socket)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap)
import Types.Tank (TankType) 
import qualified Data.Map as Map  
import Types.MatchState (MatchState(..))
import Network.Packet (PlayerInfo) 
import System.IO (Handle)
import Control.Concurrent.MVar (MVar)
import Types.GameMode (GameMode(..))

data RoomGameState = RoomGameState
  { rgsTick     :: Int
  , rgsCommands :: [Command]
  , rgsPlayers  :: Map.Map SockAddr PlayerState 
  , rgsEnemies  :: [EnemyState]
  , rgsBullets  :: [BulletState]
  , rgsNextId   :: Int 
  , rgsMap      :: GameMap
  , rgsSpawns   :: [Vec2]
  , rgsMatchState :: MatchState
  , rgsMode     :: GameMode
  , rgsIsPaused :: Bool
  }

data Command = Command SockAddr PlayerCommand

initialRoomGameState :: GameMap -> [Vec2] -> GameMode -> RoomGameState
initialRoomGameState loadedMap spawnPoints mode = RoomGameState
  { rgsTick = 0
  , rgsCommands = []
  , rgsPlayers = Map.empty
  , rgsEnemies = [] 
  , rgsBullets = []
  , rgsNextId = 1
  , rgsMap = loadedMap     
  , rgsSpawns = spawnPoints
  , rgsMatchState = Waiting
  , rgsMode = mode
  , rgsIsPaused = False
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

-- ================================================================
-- TRẠNG THÁI TOÀN CỤC CỦA SERVER (QUẢN LÝ LOBBY)
-- ================================================================

data PlayerClient = PlayerClient
  { pcHandle :: Handle     
  , pcInfo   :: PlayerInfo 
  , pcUdpAddr :: Maybe SockAddr 
  }

data Room = Room
  { roomMsgId   :: String 
  , roomPlayers :: Map.Map Int PlayerClient 
  , roomGame    :: Maybe (MVar RoomGameState) 
  }

data ServerState = ServerState
  { ssClients :: Map.Map Int PlayerClient 
  , ssRooms   :: Map.Map String Room      
  , ssNextPlayerId :: Int
  , ssUdpSocket :: Socket 
  , ssMap       :: GameMap 
  , ssSpawns    :: [Vec2]    
  }

initialServerState :: Socket -> GameMap -> [Vec2] -> ServerState
initialServerState sock gmap spawns = ServerState
  { ssClients = Map.empty
  , ssRooms = Map.empty
  , ssNextPlayerId = 1
  , ssUdpSocket = sock
  , ssMap = gmap
  , ssSpawns = spawns
  }