{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module ServerApp (runServer) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find) 
import Data.Maybe (Maybe(..)) 
import System.IO (hSetEncoding, stdout, stderr, utf8) 

import Core.Types (initialGameState, GameState(..), Command(..))
import Network.UDPServer (udpListenLoop)
import Network.TCPServer (startTcpServer) -- <--- THÊM IMPORT NÀY
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities)
import Systems.CombatSystem (spawnNewBullets, resolveCollisions)
import Systems.AISystem (updateAI)
import Network.Packet (WorldSnapshot(..), ServerPacket(..))
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (toStrict)

import Systems.MapLoader (loadMapFromFile) 
import Types.Player (PlayerState(..)) 
import Types.Common (Vec2(..))
import Types.MatchState (MatchState(..)) 

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

runServer :: IO ()
runServer = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Starting MMO Dungeon Crawler server..."
  
  let mapToLoad = "server/assets/maps/pvp.json" 
  
  putStrLn $ "Loading map: " ++ mapToLoad
  eMapData <- loadMapFromFile mapToLoad
  
  case eMapData of
    Left err -> putStrLn $ "FATAL: Không thể tải map: " ++ err
    Right (loadedMap, spawnPoints) -> do
      putStrLn $ "Map loaded. Spawn points: " ++ show (length spawnPoints)
      
      gameStateRef <- newMVar (initialGameState loadedMap spawnPoints)
      
      -- Khởi động UDP Server (cho game real-time)
      sock <- socket AF_INET Datagram defaultProtocol
      bind sock (SockAddrInet 8888 0)
      _ <- forkIO $ udpListenLoop sock gameStateRef
      putStrLn "UDP Server is listening on port 8888"

      -- *** THÊM DÒNG NÀY: Khởi động TCP Server (cho Lobby/Chat) ***
      _ <- forkIO $ startTcpServer
      
      putStrLn $ "Starting game loop with tick rate: " ++ show tickRate
      gameLoop sock gameStateRef

-- (Toàn bộ phần code còn lại của file này giữ nguyên: respawnDeadPlayers, gameLoop)
-- ...
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
  
  (finalGameState, packetsToSend) <- case gsMatchState gs of
    
    Waiting -> do
      let playerCount = Map.size (gsPlayers gs)
      if playerCount < 2
        then do
          let packet = SPMatchStateUpdate Waiting
          pure (gs { gsCommands = [] }, [(packet, Map.keys (gsPlayers gs))])
        else do
          putStrLn "[GameLoop] Enough players. Start the match!"
          let newState = gs { gsMatchState = InProgress, gsCommands = [] }
          let packet = SPMatchStateUpdate InProgress
          pure (newState, [(packet, Map.keys (gsPlayers newState))])

    InProgress -> do
      let gs' = updatePlayerPhysics dt gs
      let gs_ai = gs' 
      let gs'' = updateBulletPhysics dt gs_ai
      let gs''' = resolveCollisions gs''
      let gs'''' = spawnNewBullets gs'''
      let gs_filtered_entities = filterDeadEntities gs''''
      let gs_respawned = respawnDeadPlayers gs_filtered_entities
      let alivePlayers = Map.filter (\p -> psLives p > 0) (gsPlayers gs_respawned)
      
      if Map.size alivePlayers <= 1
        then do
          let mWinner = find (const True) (Map.elems alivePlayers) 
          let winnerId = fmap psId mWinner
          putStrLn $ "[GameLoop] The match is over. The winner: " ++ show winnerId
          
          let newState = gs_respawned { gsMatchState = GameOver winnerId, gsCommands = [], gsPlayers = Map.filter (\p -> psLives p > 0) (gsPlayers gs_respawned) }
          let packet = SPMatchStateUpdate (GameOver winnerId)
          pure (newState, [(packet, Map.keys (gsPlayers gs_respawned))])
        else do
          let snapshot = WorldSnapshot
                { wsPlayers = Map.elems (gsPlayers gs_respawned) 
                , wsEnemies = gsEnemies gs_respawned
                , wsBullets = gsBullets gs_respawned
                }
          let snapshotPacket = SPSnapshot snapshot
          let finalState = gs_respawned { gsPlayers = alivePlayers, gsCommands = [], gsTick = gsTick gs + 1 }
          pure (finalState, [(snapshotPacket, Map.keys alivePlayers)])

    GameOver winnerId -> do
      let packet = SPMatchStateUpdate (GameOver winnerId)
      pure (gs { gsCommands = [] }, [(packet, Map.keys (gsPlayers gs))])
      
  mapM_ (\(pkt, addrs) -> do
    let lazyPkt = encode pkt
    let strictPkt = toStrict lazyPkt
    mapM_ (BS.sendTo sock strictPkt) addrs
    ) packetsToSend

  putMVar gameStateRef finalGameState
  threadDelay tickInterval