module Core.Types
  ( module Core.Types
  ) where

import Network.Socket (SockAddr, Socket)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Map (GameMap)
import Types.Tank (TankType) 
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types.MatchState (MatchState(..))
import Network.Packet (PlayerInfo) 
import System.IO (Handle)
import Control.Concurrent.MVar (MVar)
import Types.GameMode (GameMode(..))
import Database.SQLite.Simple (Connection)

-- Khai báo trạng thái trò chơi trong một phòng
data RoomGameState = RoomGameState
  { rgsTick     :: Int
  , rgsCommands :: [Command]
  , rgsPlayers  :: Map.Map SockAddr PlayerState 
  , rgsBullets  :: [BulletState]
  , rgsNextId   :: Int 
  , rgsMap      :: GameMap
  , rgsSpawns   :: [Vec2]
  , rgsMatchState :: MatchState
  , rgsMode     :: GameMode
  , rgsIsPaused :: Bool
  , rgsCurrentTime :: Float
  }

-- Đại diện cho một lệnh từ người chơi
data Command = Command SockAddr PlayerCommand

-- Hàm khởi tạo trạng thái ban đầu cho một phòng chơi
initialRoomGameState :: GameMap -> [Vec2] -> GameMode -> RoomGameState
initialRoomGameState loadedMap spawnPoints mode = RoomGameState
  { rgsTick = 0
  , rgsCommands = []
  , rgsPlayers = Map.empty
  , rgsBullets = []
  , rgsNextId = 1
  , rgsMap = loadedMap     
  , rgsSpawns = spawnPoints
  , rgsMatchState = Waiting
  , rgsMode = mode
  , rgsIsPaused = False
  , rgsCurrentTime = 0.0
  }

-- Hàm khởi tạo trạng thái ban đầu cho một người chơi
initialPlayerState :: Vec2 -> Int -> TankType -> Float -> PlayerState
initialPlayerState spawnPos playerId tankType initialAngle = PlayerState
  { 
    psId = playerId
  , psPosition = spawnPos
  , psBodyAngle = initialAngle
  , psTurretAngle = initialAngle
  , psHealth = 100
  , psTankType = tankType
  , psLives = 3
  , psLastFireTime = 0.0
  }


-- ================================================================
-- TRẠNG THÁI TOÀN CỤC CỦA SERVER (QUẢN LÝ LOBBY)
-- ================================================================

-- Đại diện cho một client người chơi kết nối tới server
data PlayerClient = PlayerClient
  { pcHandle :: Handle     
  , pcInfo   :: PlayerInfo 
  , pcUdpAddr :: Maybe SockAddr 
  }

-- Đại diện cho một phòng chơi
data Room = Room
  { roomMsgId   :: String 
  , roomPlayers :: Map.Map Int PlayerClient 
  , roomGame    :: Maybe (MVar RoomGameState)
  , roomRematchRequests :: Set.Set Int
  }

-- Trạng thái toàn cục của server
data ServerState = ServerState
  { ssClients :: Map.Map Int PlayerClient 
  , ssRooms   :: Map.Map String Room      
  , ssNextPlayerId :: Int
  , ssUdpSocket :: Socket 
  , ssMap       :: GameMap 
  , ssSpawns    :: [Vec2]
  , ssDbConn    :: Connection
  }

-- Hàm khởi tạo trạng thái ban đầu cho server
initialServerState :: Socket -> GameMap -> [Vec2] -> Connection -> ServerState
initialServerState sock gmap spawns dbConn = ServerState
  { ssClients = Map.empty
  , ssRooms = Map.empty
  , ssNextPlayerId = 1
  , ssUdpSocket = sock
  , ssMap = gmap
  , ssSpawns = spawns
  , ssDbConn = dbConn
  }