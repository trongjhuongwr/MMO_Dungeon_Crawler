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

import Systems.MapLoader (loadMapFromFile) -- <-- THÊM IMPORT

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

runServer :: IO ()
runServer = withSocketsDo $ do
  putStrLn "Starting MMO Dungeon Crawler server..."
  
  -- ===== BƯỚC TẢI MAP MỚI =====
  -- (Sau này, logic này sẽ do PartySystem gọi)
  let mapToLoad = "server/assets/maps/pvp.json" 
  -- (Để test PvP, đổi file này thành "pvp_arena_1.json")
  
  putStrLn $ "Loading map: " ++ mapToLoad
  eMapData <- loadMapFromFile mapToLoad
  
  case eMapData of
    Left err -> putStrLn $ "FATAL: Không thể tải map: " ++ err
    Right (loadedMap, spawnPoints) -> do
      putStrLn $ "Map loaded. Spawn points: " ++ show (length spawnPoints)
      
      -- SỬA ĐỔI: Khởi tạo state với map đã tải
      gameStateRef <- newMVar (initialGameState loadedMap spawnPoints)
      
      sock <- socket AF_INET Datagram defaultProtocol
      bind sock (SockAddrInet 8888 0)
      _ <- forkIO $ udpListenLoop sock gameStateRef
      putStrLn "UDP Server is listening on port 8888"
      putStrLn $ "Starting game loop with tick rate: " ++ show tickRate
      gameLoop sock gameStateRef

gameLoop :: Socket -> MVar GameState -> IO ()
gameLoop sock gameStateRef = forever $ do
  -- ... (logic tick game giữ nguyên) ...
  gs <- takeMVar gameStateRef
  let dt = fromIntegral tickInterval / 1000000.0
  let gs' = updatePlayerPhysics dt gs
  let gs_ai = updateAI dt gs'
  let gs'' = updateBulletPhysics dt gs_ai
  let gs''' = resolveCollisions gs''
  let gs'''' = spawnNewBullets gs'''
  let finalGameState = filterDeadEntities gs''''

  -- SỬA ĐỒI: Tạo snapshot KHÔNG CÓ MAP
  let snapshot = WorldSnapshot
        { wsPlayers = Map.elems (gsPlayers finalGameState)
        , wsEnemies = gsEnemies finalGameState
        , wsBullets = gsBullets finalGameState
        }
  
  -- ... (gửi snapshot và kết thúc loop giữ nguyên) ...
  let lazySnapshot = encode snapshot
  let strictSnapshot = toStrict lazySnapshot
  let clientAddrs = Map.keys (gsPlayers finalGameState)
  mapM_ (BS.sendTo sock strictSnapshot) clientAddrs
  putStrLn $ "[Server] Sent snapshot to " ++ show (length clientAddrs) ++ " client(s)"

  let cleanGameState = finalGameState { gsTick = gsTick gs + 1, gsCommands = [] }
  putMVar gameStateRef cleanGameState
  
  threadDelay tickInterval