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
import Network.UDPServer (udpListenLoop)
import Network.TCPServer (startTcpServer) 
import Systems.MapLoader (loadMapFromFile) 
import Types.Common (Vec2(..))
import qualified Core.Settings as Settings

runServer :: IO ()
runServer = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn "Starting MMO Dungeon Crawler server..."
  
  mapToLoad <- Settings.getMapPath
  
  putStrLn $ "Loading map: " ++ mapToLoad
  eMapData <- loadMapFromFile mapToLoad
  
  case eMapData of
    Left err -> putStrLn $ "FATAL: Không thể tải map: " ++ err
    Right (loadedMap, spawnPoints) -> do
      
      -- SỬA LỖI TYPO (XÓA CHỮ 'S')
      putStrLn $ "Map loaded. Spawn points: " ++ show (length spawnPoints)
      
      -- Khởi động UDP Server (chỉ Socket)
      sockUDP <- socket AF_INET Datagram defaultProtocol
      bind sockUDP (SockAddrInet 8888 0)
      putStrLn "[UDP Server] Listening on port 8888"

      -- Tạo ServerState
      let sState = initialServerState sockUDP loadedMap spawnPoints
      serverStateRef <- newMVar sState

      -- Khởi động TCP Server (cho Lobby/Chat)
      _ <- forkIO $ startTcpServer serverStateRef
      
      -- Khởi động UDP Listener
      _ <- forkIO $ udpListenLoop sockUDP serverStateRef
      
      putStrLn "Server is running. Main thread is sleeping."
      -- Luồng chính ngủ, các luồng con (TCP, UDP) sẽ xử lý
      forever $ threadDelay 1000000