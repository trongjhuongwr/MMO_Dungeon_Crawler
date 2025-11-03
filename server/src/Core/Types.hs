module Core.Types where

import Network.Socket (SockAddr)
import Types.Player (PlayerCommand, PlayerState(..))
import Types.Common (Vec2(..))
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import qualified Data.Map as Map  -- Dùng Map để quản lý Player

data GameState = GameState
  { gsTick     :: Int
  , gsCommands :: [Command]
  , gsPlayers  :: Map.Map SockAddr PlayerState -- Dùng Map (SockAddr làm key)
  , gsEnemies  :: [EnemyState]
  , gsBullets  :: [BulletState]
  , gsNextId   :: Int -- Bộ đếm ID cho các thực thể mới
  }

data Command = Command SockAddr PlayerCommand

initialGameState :: GameState
initialGameState = GameState
  { gsTick = 0
  , gsCommands = []
  , gsPlayers = Map.empty -- Khởi tạo rỗng
  , gsEnemies = -- Thêm 2 quái tĩnh để test
      [ EnemyState { esId = 1, esPosition = Vec2 100 100, esHealth = 10 }
      , EnemyState { esId = 2, esPosition = Vec2 (-100) 50, esHealth = 10 }
      ]
  , gsBullets = []
  , gsNextId = 3 -- Bắt đầu từ 3 (vì đã dùng 1, 2)
  }

initialPlayerState :: PlayerState
initialPlayerState = PlayerState
  { psPosition = Vec2 0 0
  , psBodyAngle = 0.0
  , psTurretAngle = 0.0
  }