{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Network.Packet
  ( -- Client -> Server
    ClientTcpPacket(..)
  , ClientUdpPacket(..)
    -- Server -> Client
  , ServerTcpPacket(..)
  , ServerUdpPacket(..)
    -- Shared types used in packets
  , WorldSnapshot(..)
  , PlayerInfo(..)
  , PlayerCommand(..)
  ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Player (PlayerState, PlayerCommand)
import Types.Bullet (BulletState)
import Types.Enemy (EnemyState)
import Types.MatchState (MatchState)
import Types.Tank (TankType)
import Data.Maybe (Maybe)
import Types.GameMode (GameMode)

-- ===================================================================
-- GÓI TIN TỪ CLIENT GỬI LÊN SERVER
-- ===================================================================

-- Gói tin TCP (Dùng cho Login, Lobby, Quản lý phòng)
data ClientTcpPacket
  = CTP_Login String String -- (username, password) - Tạm thời chỉ dùng username
  | CTP_CreateRoom
  | CTP_JoinRoom String     -- (roomId)
  | CTP_UpdateLobbyState (Maybe TankType) Bool -- (Maybe TankType, IsReady)
  | CTP_LeaveRoom
  | CTP_RequestRematch
  | CTP_StartDungeon (Maybe TankType)
  | CTP_PauseGame Bool
  deriving (Show, Generic)

instance Binary ClientTcpPacket

-- Gói tin UDP (Dùng trong game)
data ClientUdpPacket
  = CUP_Handshake Int     -- (myPlayerId) - Gói tin đầu tiên để đăng ký UDP
  | CUP_Command PlayerCommand -- Gói tin input (di chuyển, bắn)
  deriving (Show, Generic)

instance Binary ClientUdpPacket

-- ===================================================================
-- GÓI TIN TỪ SERVER GỬI XUỐNG CLIENT
-- ===================================================================

-- Gói tin TCP (Dùng cho Login, Lobby, Quản lý phòng)
data ServerTcpPacket
  = STP_LoginResult Bool Int String -- (success, yourPlayerId, message)
  | STP_RoomUpdate String [PlayerInfo] -- (roomId, list of players in room)
  | STP_GameStarting GameMode
  | STP_ShowMenu -- Yêu cầu client quay về menu
  | STP_Kicked String                -- (reason: "Room not found", "Room full", etc.)
  deriving (Show, Generic)

instance Binary ServerTcpPacket

-- Gói tin UDP (Dùng trong game)
data ServerUdpPacket
  = SUP_Snapshot WorldSnapshot
  | SUP_MatchStateUpdate MatchState
  deriving (Show, Generic)

instance Binary ServerUdpPacket

-- ===================================================================
-- CÁC KIỂU DỮ LIỆU HỖ TRỢ (Dùng trong gói tin)
-- ===================================================================

-- Gói tin trạng thái thế giới (Giữ nguyên)
data WorldSnapshot = WorldSnapshot
  { wsPlayers :: [PlayerState]
  , wsEnemies :: [EnemyState]
  , wsBullets :: [BulletState]
  } deriving (Show, Generic)

instance Binary WorldSnapshot

-- Thông tin rút gọn của người chơi trong sảnh chờ
data PlayerInfo = PlayerInfo
  { piId :: Int
  , piName :: String
  , piSelectedTank :: Maybe TankType
  , piIsReady :: Bool
  } deriving (Show, Generic)

instance Binary PlayerInfo