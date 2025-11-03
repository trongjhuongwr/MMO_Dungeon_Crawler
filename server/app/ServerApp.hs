module ServerApp (runServer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Network.Socket
import qualified Data.Map as Map

import Core.Types (initialGameState, GameState(..), Command(..))
import Network.UDPServer (udpListenLoop)
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities) -- Import
import Systems.CombatSystem (resolveCombat) -- Import
import Network.Packet (WorldSnapshot(..))
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (toStrict)

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

runServer :: IO ()
runServer = withSocketsDo $ do
  -- ... (Phần khởi tạo không đổi) ...
  putStrLn "Starting MMO Dungeon Crawler server..."
  gameStateRef <- newMVar initialGameState
  sock <- socket AF_INET Datagram defaultProtocol
  bind sock (SockAddrInet 8888 0)
  _ <- forkIO $ udpListenLoop sock gameStateRef
  putStrLn "UDP Server is listening on port 8888"
  putStrLn $ "Starting game loop with tick rate: " ++ show tickRate
  gameLoop sock gameStateRef

gameLoop :: Socket -> MVar GameState -> IO ()
gameLoop sock gameStateRef = forever $ do
  gs <- takeMVar gameStateRef
  let dt = fromIntegral tickInterval / 1000000.0

  -- 1. Cập nhật di chuyển của Player (từ PhysicsSystem)
  let gs' = updatePlayerPhysics dt gs
  
  -- 2. Xử lý logic combat (tạo đạn, va chạm) (từ CombatSystem)
  let gs'' = resolveCombat gs'
  
  -- 3. Cập nhật di chuyển của đạn (từ PhysicsSystem)
  let gs''' = updateBulletPhysics dt gs''
  
  -- 4. Lọc bỏ các thực thể đã chết (đạn hết giờ, quái hết máu)
  let finalGameState = filterDeadEntities gs'''

  -- 5. Tạo snapshot
  let snapshot = WorldSnapshot
        { wsPlayers = Map.elems (gsPlayers finalGameState) -- Chuyển Map thành List
        , wsEnemies = gsEnemies finalGameState
        , wsBullets = gsBullets finalGameState
        }
  let lazySnapshot = encode snapshot
  let strictSnapshot = toStrict lazySnapshot

  -- 6. Gửi snapshot tới tất cả client đã kết nối
  let clientAddrs = Map.keys (gsPlayers finalGameState)
  mapM_ (\addr -> BS.sendTo sock strictSnapshot addr) clientAddrs

  -- 7. Reset commands và tăng tick
  let cleanGameState = finalGameState { gsTick = gsTick gs + 1, gsCommands = [] }
  putMVar gameStateRef cleanGameState
  
  threadDelay tickInterval