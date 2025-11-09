{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Types
  ( InGameState(..)
  , LoginData(..)
  , LobbyData(..)
  , PostGameData(..)
  , AppState(..)
  , ClientState(..)
  , MatchState(..)
  ) where

import Network.Socket (Socket, SockAddr)
import System.IO (Handle)
import Control.Concurrent (MVar)
import qualified Data.Set as Set
import Data.Maybe (Maybe)

import Network.Packet (WorldSnapshot, PlayerInfo)
import Types.Map (GameMap)
import Types.Common (Vec2)
import Types.Tank (TankType)
import Types.MatchState (MatchState(..))
import Core.Effect (Effect)
import Core.Animation (Animation)
import Input (KeyMap)
import Renderer.Resources (Resources)
import Types.GameMode (GameMode)

-- ===================================================================
-- KIỂU DỮ LIỆU STATE MÁY
-- ===================================================================

-- Trạng thái khi đang trong game (ClientState cũ)
data InGameState = InGameState
  { igsKeys              :: KeyMap
  , igsMousePos          :: (Float, Float)
  , igsWorld             :: WorldSnapshot
  , igsGameMap           :: GameMap 
  , igsDidFire           :: Bool
  , igsEffects           :: [Effect]
  , igsNextEffectId      :: Int
  , igsTurretAnimRapid   :: Animation 
  , igsTurretAnimBlast   :: Animation 
  , igsMyId              :: Int
  , igsMatchState        :: MatchState
  , igsMode              :: GameMode
  }

-- Dữ liệu cho màn hình Login
data LoginData = LoginData { ldUsername :: String, ldStatus :: String }

-- Dữ liệu cho sảnh chờ
data LobbyData = LobbyData 
  { ldRoomId :: String
  , ldPlayers :: [PlayerInfo] -- Lấy từ server (chứa lựa chọn của đối thủ)
  , ldMyTank :: Maybe TankType -- Lựa chọn của bản thân
  , ldMyReady :: Bool         -- Trạng thái sẵn sàng của bản thân
  }

data PostGameData = PostGameData { pgStatus :: String }

-- Trạng thái của toàn bộ ứng dụng
data AppState
  = S_Login    LoginData    -- Màn hình đăng nhập
  | S_Menu                  -- Menu chính (Nút Start)
  | S_RoomSelection String  -- (String là RoomID đang nhập)
  | S_Lobby    LobbyData    -- Sảnh chờ (Tạo/Vào phòng)
  | S_DungeonLobby (Maybe TankType)
  | S_InGame   InGameState  -- Trạng thái game (ClientState cũ)
  | S_PostGame PostGameData -- Màn hình kết thúc (Chơi lại/Thoát)
  | S_Paused   InGameState Bool

-- TRẠNG THÁI CLIENT TOÀN CỤC
data ClientState = ClientState
  { csTcpHandle  :: Handle     -- Kết nối TCP vĩnh viễn
  , csUdpSocket  :: Socket     -- Socket UDP
  , csServerAddr :: SockAddr   -- Địa chỉ UDP của server
  , csMyId       :: Int        -- ID của mình (lấy sau khi login)
  , csState      :: AppState
  , csResources  :: Resources
  }