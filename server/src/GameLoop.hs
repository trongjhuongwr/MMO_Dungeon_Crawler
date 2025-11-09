{-# LANGUAGE ScopedTypeVariables #-}

module GameLoop (gameLoop) where

import Control.Concurrent (threadDelay, MVar, newMVar, takeMVar, putMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever, when, void)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find) 
import Data.Maybe (Maybe(..)) 
import System.IO (hSetEncoding, stdout, stderr, utf8) 
import Control.Exception (catch, SomeException, try, SomeException(..))

import Core.Types -- Import tất cả type
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities)
import Systems.CombatSystem (spawnNewBullets, resolveCollisions)
import Systems.AISystem (updateAI) -- <-- Đã BỎ COMMENT
import Network.Packet
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS (ByteString) -- <--- SỬA 1: Import kiểu ByteString (strict)
import qualified Network.Socket.ByteString as BS (sendTo) -- <--- SỬA 2: Giữ nguyên import sendTo
import Data.ByteString.Lazy.Internal (toStrict)

import Types.Player (PlayerState(..)) 
import Types.Common (Vec2(..))
import Types.MatchState (MatchState(..))
import Types.GameMode (GameMode(..))

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

-- (Hàm respawnDeadPlayers của anh đã đúng, giữ nguyên)
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


-- VÒNG LẶP GAME CỦA MỘT PHÒNG
gameLoop :: MVar ServerState -> String -> MVar RoomGameState -> IO ()
gameLoop serverStateRef roomId roomStateRef = (forever $ do
  gs <- takeMVar roomStateRef
  
  -- 1. KIỂM TRA PAUSE
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
          let gs' = updatePlayerPhysics dt gs 
          let gs_ai = if (rgsMode gs' == PvE) then updateAI dt gs' else gs'
          let gs'' = updateBulletPhysics dt gs_ai
          let gs''' = resolveCollisions gs'' 
          let gs'''' = spawnNewBullets gs''' 
          let gs_filtered_entities = filterDeadEntities gs''''
          let gs_respawned = respawnDeadPlayers gs_filtered_entities
          
          let (isGameOver, mWinnerId) = case rgsMode gs_respawned of
                PvP -> 
                  let alivePlayers_PvP = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
                  in if Map.size alivePlayers_PvP <= 1
                       then (True, fmap psId (find (const True) (Map.elems alivePlayers_PvP)))
                       else (False, Nothing)
                
                PvE -> 
                  let alivePlayers_Dungeon = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
                  in if Map.null alivePlayers_Dungeon
                       then (True, Nothing)
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
                    , wsEnemies = rgsEnemies gs_respawned
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
      sState <- readMVar serverStateRef -- <-- SỬA LỖI: Lấy sState
      sendPackets sState packetsToSend
      
      -- 4. CẬP NHẬT STATE VÀ NGỦ
      let cleanGameState = finalGameState { rgsCommands = [] }
      putMVar roomStateRef cleanGameState
      
      -- 5. KIỂM TRA THOÁT LOOP
      when (isMatchOver cleanGameState) (breakLoop serverStateRef roomId)
      
      threadDelay tickInterval -- <-- HÀNH ĐỘNG CUỐI CÙNG CỦA 'else'

  ) `catch` \(e :: SomeException) -> do
    -- Bắt lỗi "fail" từ breakLoop để thoát
    putStrLn $ "[GameLoop " ++ roomId ++ "] Loop exited."
    pure ()

  -- === HÀM HELPER (ĐỊNH NGHĨA TRONG 'where') ===
  where
    -- HÀM MỚI: Gửi gói tin
    sendPackets :: ServerState -> [(ServerUdpPacket, [SockAddr])] -> IO ()
    sendPackets sState packetsToSend = do
      let sock = ssUdpSocket sState
      mapM_ (\(pkt, targetAddrs) -> do
        let lazyPkt = encode pkt
        let strictPkt = toStrict lazyPkt
        -- Gửi đến các địa chỉ UDP đã đăng ký
        mapM_ (sendPacketInternal sock strictPkt) targetAddrs
        ) packetsToSend

    -- HÀM MỚI: Hàm con của sendPackets, bắt lỗi
    sendPacketInternal :: Socket -> BS.ByteString -> SockAddr -> IO () -- <--- SỬA 3: Đổi BS.ByteString -> ByteString
    sendPacketInternal sock strictPkt addr = 
      (void $ BS.sendTo sock strictPkt addr) `catch` \(e :: SomeException) -> do
        putStrLn $ "[UDP] Failed to send packet to " ++ show addr ++ ": " ++ show e
        pure () -- Bỏ qua lỗi và tiếp tục

    -- HÀM MỚI: Kiểm tra game over
    isMatchOver :: RoomGameState -> Bool
    isMatchOver gs = case rgsMatchState gs of
      GameOver _ -> True
      _          -> False

    -- HÀM MỚI: Dừng vòng lặp
    breakLoop :: MVar ServerState -> String -> IO ()
    breakLoop serverStateRef roomId = do
      putStrLn $ "[GameLoop " ++ roomId ++ "] Match finished. Stopping game loop."
      -- Xóa phòng khỏi ServerState
      modifyMVar_ serverStateRef $ \sState ->
        pure sState { ssRooms = Map.delete roomId (ssRooms sState) }
      -- Thoát khỏi `forever`
      fail "Game loop finished."