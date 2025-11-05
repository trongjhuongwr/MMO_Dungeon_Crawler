module Core.Types where

import Network.Socket (SockAddr)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap)
import qualified Data.Map as Map  -- Dùng Map để quản lý Player

import Systems.DungeonSystem (generateTestLevel)

data GameState = GameState
  { gsTick     :: Int
  , gsCommands :: [Command]
  , gsPlayers  :: Map.Map SockAddr PlayerState -- Dùng Map (SockAddr làm key)
  , gsEnemies  :: [EnemyState]
  , gsBullets  :: [BulletState]
  , gsNextId   :: Int 
  , gsMap      :: GameMap
  }

data Command = Command SockAddr PlayerCommand

initialGameState :: GameState
initialGameState = GameState
  { gsTick = 0
  , gsCommands = []
  , gsPlayers = Map.empty
  , gsEnemies = -- Đặt quái bên trong các phòng mới
      -- Quái phòng 1.1 (25, 25)
      [ EnemyState { esId = 1, esPosition = Vec2 (25 * 32) (25 * 32), esHealth = 10 }
      -- Quái phòng 1.2 (25, 48)
      , EnemyState { esId = 2, esPosition = Vec2 (48 * 32) (24 * 32), esHealth = 10 }
      , EnemyState { esId = 3, esPosition = Vec2 (48 * 32) (26 * 32), esHealth = 10 }
      -- Quái phòng Boss (25, 85)
      , EnemyState { esId = 4, esPosition = Vec2 (85 * 32) (25 * 32), esHealth = 50 }
      ]
  , gsBullets = []
  , gsNextId = 5 -- Cập nhật NextId
  , gsMap = generateTestLevel -- SỬ DỤNG MAP MỚI
  }

initialPlayerState :: PlayerState
initialPlayerState = PlayerState
  { 
    -- Đặt vị trí spawn của người chơi (25, 5) theo tọa độ thế giới
    psPosition = Vec2 (5 * 32) (25 * 32) 
  , psBodyAngle = 0.0
  , psTurretAngle = 0.0
  , psHealth = 100
  }