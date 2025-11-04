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

  -- 1. Cập nhật di chuyển của Player
  let gs' = updatePlayerPhysics dt gs
  
  -- 2. Cập nhật di chuyển của đạn (từ tick trước)
  let gs'' = updateBulletPhysics dt gs'
  
  -- 3. Xử lý va chạm của đạn (đã di chuyển)
  let gs''' = resolveCollisions gs''
  
  -- 4. Tạo đạn mới (từ input tick này)
  let gs'''' = spawnNewBullets gs'''
  
  -- 5. Lọc bỏ các thực thể đã chết
  let finalGameState = filterDeadEntities gs''''

  -- 6. Tạo snapshot
  let snapshot = WorldSnapshot
        { wsPlayers = Map.elems (gsPlayers finalGameState)
        , wsEnemies = gsEnemies finalGameState
        , wsBullets = gsBullets finalGameState
        }
  let lazySnapshot = encode snapshot
  let strictSnapshot = toStrict lazySnapshot

  -- 7. Gửi snapshot tới tất cả client đã kết nối
  let clientAddrs = Map.keys (gsPlayers finalGameState)
  -- SỬA LỖI (hlint): "Avoid lambda"
  mapM_ (BS.sendTo sock strictSnapshot) clientAddrs

  -- 8. Reset commands và tăng tick
  let cleanGameState = finalGameState { gsTick = gsTick gs + 1, gsCommands = [] }
  putMVar gameStateRef cleanGameState
  
  threadDelay tickInterval