module ServerApp (runServer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Network.Socket
import qualified Data.Map as Map

import Core.Types (initialGameState, GameState(..), Command(..))
import Network.UDPServer (udpListenLoop)
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities)
import Systems.CombatSystem (spawnNewBullets, resolveCollisions)
import Systems.AISystem (updateAI)
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

  -- 1. Cập nhật di chuyển của Player (Đã có va chạm map)
  let gs' = updatePlayerPhysics dt gs
  
  -- 2. THÊM MỚI: Cập nhật AI (Tính toán di chuyển cho quái)
  let gs_ai = updateAI dt gs'
  
  -- 3. Cập nhật di chuyển của đạn (từ tick trước)
  let gs'' = updateBulletPhysics dt gs_ai
  
  -- 4. Xử lý va chạm của đạn (đã di chuyển)
  let gs''' = resolveCollisions gs''
  
  -- 5. Tạo đạn mới (từ input tick này)
  let gs'''' = spawnNewBullets gs'''
  
  -- 6. Lọc bỏ các thực thể đã chết (đạn va chạm tường/hết giờ, quái hết máu)
  let finalGameState = filterDeadEntities gs''''

  -- 7. Tạo snapshot
  let snapshot = WorldSnapshot
        { wsPlayers = Map.elems (gsPlayers finalGameState)
        , wsEnemies = gsEnemies finalGameState
        , wsBullets = gsBullets finalGameState
        , wsMap     = gsMap finalGameState -- THÊM MỚI
        }
  -- ... (gửi snapshot và kết thúc loop)
  let lazySnapshot = encode snapshot
  let strictSnapshot = toStrict lazySnapshot

  let clientAddrs = Map.keys (gsPlayers finalGameState)
  -- Gửi snapshot tới mọi client đã đăng ký
  mapM_ (BS.sendTo sock strictSnapshot) clientAddrs
  putStrLn $ "[Server] Sent snapshot to " ++ show (length clientAddrs) ++ " client(s)"

  let cleanGameState = finalGameState { gsTick = gsTick gs + 1, gsCommands = [] }
  putMVar gameStateRef cleanGameState
  
  threadDelay tickInterval