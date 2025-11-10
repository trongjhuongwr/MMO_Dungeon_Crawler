{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}
module ServerApp (runServer) where 

import Control.Concurrent (forkIO, threadDelay, MVar, newMVar)
import Control.Monad (forever)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find) 
import Data.Maybe (Maybe(..)) 
import System.IO (hSetEncoding, stdout, stderr, utf8) 

import Core.Types 
import Core.Config (AppConfig(..), loadConfig)
import Network.UDPServer (udpListenLoop)
import Network.TCPServer (startTcpServer) 
import Systems.MapLoader (loadMapFromFile) 
import Types.Common (Vec2(..))
import Data.Database (connectDb)

runServer :: IO ()
runServer = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Starting MMO Dungeon Crawler server..."

  config <- loadConfig "server/config/server.yaml"
  putStrLn $ "Config loaded: " ++ show config

  -- Kết nối DB
  putStrLn "Connecting to database..."
  dbConn <- connectDb
  putStrLn "Database connected."

  let mapToLoad = mapFile config
  
  putStrLn $ "Loading map: " ++ mapToLoad
  eMapData <- loadMapFromFile mapToLoad
  
  case eMapData of
    Left err -> fail $ "FATAL: Cann't load map: " ++ err
    Right (loadedMap, spawnPoints) -> do
      
      putStrLn $ "Map loaded. Spawn points: " ++ show (length spawnPoints)
      
      -- Khởi động UDP Server (dùng port từ config)
      sockUDP <- socket AF_INET Datagram defaultProtocol
      bind sockUDP (SockAddrInet (fromIntegral $ udpPort config) 0)
      putStrLn $ "[UDP Server] Listening on port " ++ show (udpPort config)

      -- Tạo ServerState
      let sState = initialServerState sockUDP loadedMap spawnPoints dbConn
      serverStateRef <- newMVar sState

      -- Khởi động TCP Server (truyền config vào)
      _ <- forkIO $ startTcpServer config serverStateRef
      
      -- Khởi động UDP Listener
      _ <- forkIO $ udpListenLoop sockUDP serverStateRef
      
      putStrLn "Server is running. Main thread is sleeping."
      forever $ threadDelay 1000000