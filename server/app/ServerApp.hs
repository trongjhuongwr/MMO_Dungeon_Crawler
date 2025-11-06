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
import Network.Packet (WorldSnapshot(..), ServerPacket(..)) -- <-- SỬA IMPORT
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (toStrict)

import Systems.MapLoader (loadMapFromFile) 
import Types.Player (PlayerState(..)) 
import Types.Common (Vec2(..)) -- <-- THÊM IMPORT NÀY

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

runServer :: IO ()
runServer = withSocketsDo $ do
  putStrLn "Starting MMO Dungeon Crawler server..."
  
  let mapToLoad = "server/assets/maps/pvp.json" 
  
  putStrLn $ "Loading map: " ++ mapToLoad
  eMapData <- loadMapFromFile mapToLoad
  
  case eMapData of
    Left err -> putStrLn $ "FATAL: Không thể tải map: " ++ err
    Right (loadedMap, spawnPoints) -> do
      putStrLn $ "Map loaded. Spawn points: " ++ show (length spawnPoints)
      
      gameStateRef <- newMVar (initialGameState loadedMap spawnPoints)
      
      sock <- socket AF_INET Datagram defaultProtocol
      bind sock (SockAddrInet 8888 0)
      _ <- forkIO $ udpListenLoop sock gameStateRef
      putStrLn "UDP Server is listening on port 8888"
      putStrLn $ "Starting game loop with tick rate: " ++ show tickRate
      gameLoop sock gameStateRef

-- HÀM MỚI ĐỂ SỬA LỖI HỒI SINH
respawnDeadPlayers :: GameState -> GameState
respawnDeadPlayers gs =
  let
    players = gsPlayers gs
    spawnPoints = gsSpawns gs
    
    respawnPlayer :: PlayerState -> PlayerState
    respawnPlayer p =
      if psHealth p <= 0 && psLives p > 0
        then 
          let
            -- Chọn điểm spawn dựa trên ID (để không bị trùng)
            spawnPos = if null spawnPoints
                         then Vec2 0 0 -- Fallback
                         else spawnPoints !! (psId p `mod` length spawnPoints)
          in
            p { psHealth = 100, psPosition = spawnPos } -- Hồi sinh
        else 
          p -- Vẫn còn sống
          
  in
    gs { gsPlayers = Map.map respawnPlayer players }


gameLoop :: Socket -> MVar GameState -> IO ()
gameLoop sock gameStateRef = forever $ do
  gs <- takeMVar gameStateRef
  let dt = fromIntegral tickInterval / 1000000.0
  let gs' = updatePlayerPhysics dt gs
  
  let gs_ai = gs'               
  
  let gs'' = updateBulletPhysics dt gs_ai
  let gs''' = resolveCollisions gs''
  let gs'''' = spawnNewBullets gs'''
  
  -- SỬA LOGIC:
  -- BƯỚC 1: Lọc đạn/quái
  let gs_filtered_entities = filterDeadEntities gs''''
  
  -- BƯỚC 2: Hồi sinh người chơi (nếu cần)
  let gs_respawned = respawnDeadPlayers gs_filtered_entities

  -- BƯỚC 3: Lọc người chơi đã HẾT MẠNG (psLives <= 0)
  let alivePlayers = Map.filter (\p -> psLives p > 0) (gsPlayers gs_respawned)

  -- BƯỚC 4: State cuối cùng
  let finalGameState = gs_respawned { gsPlayers = alivePlayers }
  
  -- Tạo snapshot
  let snapshot = WorldSnapshot
        { wsPlayers = Map.elems (gsPlayers finalGameState)
        , wsEnemies = gsEnemies finalGameState
        , wsBullets = gsBullets finalGameState
        }
  
  -- SỬA ĐỔI: Gói snapshot vào ServerPacket
  let serverPkt = SPSnapshot snapshot
  let lazySnapshot = encode serverPkt
  let strictSnapshot = toStrict lazySnapshot
  
  -- Gửi cho tất cả người chơi còn sống
  let clientAddrs = Map.keys (gsPlayers finalGameState)
  mapM_ (BS.sendTo sock strictSnapshot) clientAddrs

  let cleanGameState = finalGameState { gsTick = gsTick gs + 1, gsCommands = [] }
  putMVar gameStateRef cleanGameState
  
  threadDelay tickInterval