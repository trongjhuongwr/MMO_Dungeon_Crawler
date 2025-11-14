{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module GameLoop (gameLoop) where

import Control.Concurrent (threadDelay, MVar, newMVar, takeMVar, putMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever, when, void)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find) 
import Data.Maybe (Maybe(..)) 
import System.IO (hSetEncoding, stdout, stderr, utf8) 
import Control.Exception (catch, SomeException, try, SomeException(..))

import Core.Types
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities)
import Systems.CombatSystem (spawnNewBullets, resolveCollisions)
import qualified Systems.AISystem as AISystem
import Network.Packet
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS (ByteString)
import qualified Network.Socket.ByteString as BS (sendTo)
import Data.ByteString.Lazy.Internal (toStrict)

import Types.Player (PlayerState(..)) 
import Types.Common (Vec2(..))
import Types.MatchState (MatchState(..))
import Types.GameMode (GameMode(..))

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

respawnDeadPlayers :: RoomGameState -> RoomGameState
respawnDeadPlayers gs =
  let
    players = rgsPlayers gs
    spawnPoints = rgsSpawns gs
    
    respawnPlayer :: PlayerState -> PlayerState
    respawnPlayer p =
      if psHealth p <= 0 && psLives p > 0
        then 
          let
            spawnPos = if null spawnPoints
                         then Vec2 0 0 -- Fallback
                         else spawnPoints !! (psId p `mod` length spawnPoints)
          in
            p { psHealth = 100, psPosition = spawnPos }
        else 
          p
          
  in
    gs { rgsPlayers = Map.map respawnPlayer players }


gameLoop :: Socket -> String -> MVar RoomGameState -> MVar ServerState -> IO ()
gameLoop sock roomId roomStateRef serverStateRef = (forever $ do
  -- KIỂM TRA TRẠNG THÁI SERVER (KHÔNG GIỮ KHÓA ROOM)
  sState <- readMVar serverStateRef
  case Map.lookup roomId (ssRooms sState) of
    Nothing -> do
      putStrLn $ "[GameLoop " ++ roomId ++ "] Room deleted. Stopping thread."
      fail "Room cleanup." 
    
    Just currentRoom ->
      case roomGame currentRoom of
        Nothing -> do
          putStrLn $ "[GameLoop " ++ roomId ++ "] Room reverted to lobby. Stopping thread."
          fail "Room reverted to lobby." 
        
        Just _ -> do
          pure ()

  -- BẮT ĐẦU KHÓA ROOM VÀ XỬ LÝ TICK
  gs <- takeMVar roomStateRef
  
  -- KIỂM TRA PAUSE
  if rgsIsPaused gs
    then do
      -- Game đang pause, chỉ đặt state lại và ngủ
      putMVar roomStateRef gs
      threadDelay tickInterval
    else do
      -- 2. GAME KHÔNG PAUSE, CHẠY LOGIC
      let dt = fromIntegral tickInterval / 1000000.0
      
      -- Logic game chính
      (finalGameState, packetsToSend) <- case rgsMatchState gs of
        
        Waiting -> do
          -- Logic này không nên chạy, TCPServer sẽ chuyển sang InProgress
          putStrLn $ "[GameLoop " ++ roomId ++ "] State was Waiting, forcing InProgress."
          pure (gs { rgsMatchState = InProgress }, [])

        InProgress -> do
          let gs_with_time = gs { rgsCurrentTime = rgsCurrentTime gs + dt }
          let gs_with_bot_cmd = AISystem.updateBotAI dt gs_with_time

          let gs' = updatePlayerPhysics dt gs_with_bot_cmd
          
          let gs'' = updateBulletPhysics dt gs'
          let gs''' = resolveCollisions gs'' 
          let gs'''' = spawnNewBullets (rgsCurrentTime gs_with_time) gs'''
          let gs_filtered_entities = filterDeadEntities gs''''
          let gs_respawned = respawnDeadPlayers gs_filtered_entities
          
          let (isGameOver, mWinnerId) = case rgsMode gs_respawned of
                PvP -> 
                  let alivePlayers_PvP = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
                  in if Map.size alivePlayers_PvP <= 1
                       then (True, fmap psId (find (const True) (Map.elems alivePlayers_PvP)))
                       else (False, Nothing)
                
                PvE ->
                  let alivePlayers_PvE = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
                  in if Map.size alivePlayers_PvE <= 1
                       then (True, fmap psId (find (const True) (Map.elems alivePlayers_PvE)))
                       else (False, Nothing)
          
          if isGameOver
            then do
              -- GAME OVER
              putStrLn $ "[GameLoop " ++ roomId ++ "] Match Over. Winner: " ++ show mWinnerId
              let newState = gs_respawned { rgsMatchState = GameOver mWinnerId, rgsCommands = [] }
              let packet = SUP_MatchStateUpdate (GameOver mWinnerId)
              -- Gửi gói tin cho TẤT CẢ player addrs (kể cả người đã chết)
              pure (newState, [(packet, Map.keys (rgsPlayers gs_respawned))])
            else do
              -- GAME TIẾP TỤC
              let snapshot = WorldSnapshot
                    { wsPlayers = Map.elems (rgsPlayers gs_respawned)
                    , wsBullets = rgsBullets gs_respawned
                    }
              let snapshotPacket = SUP_Snapshot snapshot
              let finalState = gs_respawned { rgsCommands = [], rgsTick = rgsTick gs + 1 }
              -- Gửi snapshot cho TẤT CẢ player addrs
              pure (finalState, [(snapshotPacket, Map.keys (rgsPlayers gs_respawned))])

        GameOver winnerId -> do
          -- Game đã kết thúc, chỉ giữ state và chờ TCP xử lý
          let packet = SUP_MatchStateUpdate (GameOver winnerId)
          pure (gs, [(packet, Map.keys (rgsPlayers gs))])
      
      -- 3. GỬI PACKETS (BÊN NGOÀI 'case')
      sendPackets sock packetsToSend
      
      -- 4. CẬP NHẬT STATE VÀ NGỦ
      let cleanGameState = finalGameState { rgsCommands = [] }
      putMVar roomStateRef cleanGameState

      threadDelay tickInterval -- <-- HÀNH ĐỘNG CUỐI CÙNG CỦA 'else'

  ) `catch` \(e :: SomeException) -> do
    putStrLn $ "[GameLoop " ++ roomId ++ "] Loop exited."
    pure ()

  -- === HÀM HELPER (ĐỊNH NGHĨA TRONG 'where') ===
  where
    -- Gửi gói tin
    sendPackets :: Socket -> [(ServerUdpPacket, [SockAddr])] -> IO ()
    sendPackets sock packetsToSend = do
      mapM_ (\(pkt, targetAddrs) -> do
        let lazyPkt = encode pkt
        let strictPkt = toStrict lazyPkt
        -- Gửi đến các địa chỉ UDP đã đăng ký
        mapM_ (sendPacketInternal sock strictPkt) targetAddrs
        ) packetsToSend

    -- Hàm con của sendPackets, bắt lỗi
    sendPacketInternal :: Socket -> BS.ByteString -> SockAddr -> IO ()
    sendPacketInternal sock strictPkt addr = 
      (void $ BS.sendTo sock strictPkt addr) `catch` \(e :: SomeException) -> do
        putStrLn $ "[UDP] Failed to send packet to " ++ show addr ++ ": " ++ show e
        pure () -- Bỏ qua lỗi và tiếp tục