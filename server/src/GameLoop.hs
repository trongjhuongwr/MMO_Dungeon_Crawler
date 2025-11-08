{-# LANGUAGE ScopedTypeVariables #-}

module GameLoop (gameLoop) where

import Control.Concurrent (threadDelay, MVar, newMVar, takeMVar, putMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever, when)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find) 
import Data.Maybe (Maybe(..)) 
import System.IO (hSetEncoding, stdout, stderr, utf8) 
import Control.Exception (catch, SomeException) -- <<<<<< THÊM IMPORT NÀY

import Core.Types -- Import tất cả type
import Systems.PhysicsSystem (updatePlayerPhysics, updateBulletPhysics, filterDeadEntities)
import Systems.CombatSystem (spawnNewBullets, resolveCollisions)
-- import Systems.AISystem (updateAI) -- Tắt AI cho PvP
import Network.Packet
import Data.Binary (encode)

import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (toStrict)

import Types.Player (PlayerState(..)) 
import Types.Common (Vec2(..))
import Types.MatchState (MatchState(..)) 

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

-- HÀM NÀY ĐƯỢC CHUYỂN VÀO ĐÂY TỪ SERVERAPP
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
                         -- Lấy spawn point dựa trên ID
                         else spawnPoints !! (psId p `mod` length spawnPoints)
          in
            p { psHealth = 100, psPosition = spawnPos } -- Hồi sinh
        else 
          p -- Vẫn còn sống
          
  in
    gs { rgsPlayers = Map.map respawnPlayer players }


-- VÒNG LẶP GAME CỦA MỘT PHÒNG
gameLoop :: MVar ServerState -> String -> MVar RoomGameState -> IO ()
gameLoop serverStateRef roomId roomStateRef = (forever $ do
  gs <- takeMVar roomStateRef
  let dt = fromIntegral tickInterval / 1000000.0
  
  (finalGameState, packetsToSend) <- case rgsMatchState gs of
    
    Waiting -> do
      -- Trạng thái này không nên xảy ra ở đây (đã bị logic TCP xử lý)
      -- Nhưng để an toàn, chúng ta chuyển nó sang InProgress
      putStrLn $ "[GameLoop " ++ roomId ++ "] State was Waiting, forcing InProgress."
      pure (gs { rgsMatchState = InProgress }, [])

    InProgress -> do
      let gs' = updatePlayerPhysics dt gs --
      let gs_ai = gs' -- Tắt AI: không gọi updateAI
      let gs'' = updateBulletPhysics dt gs_ai
      let gs''' = resolveCollisions gs'' --
      let gs'''' = spawnNewBullets gs''' --
      let gs_filtered_entities = filterDeadEntities gs''''
      let gs_respawned = respawnDeadPlayers gs_filtered_entities
      
      -- Chỉ giữ lại người chơi còn mạng
      let (isGameOver, mWinnerId) = case rgsMode gs_respawned of
            -- Logic PvP: Game over khi còn 1 hoặc 0 người
            PvP -> 
              let alivePlayers_PvP = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
              in if Map.size alivePlayers_PvP <= 1
                   then (True, fmap psId (find (const True) (Map.elems alivePlayers_PvP)))
                   else (False, Nothing)
            
            -- Logic Dungeon: Game over chỉ khi 0 người
            Dungeon ->
              let alivePlayers_Dungeon = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
              in if Map.null alivePlayers_Dungeon
                   then (True, Nothing) -- (Không có người thắng)
                   else (False, Nothing)
      
      if isGameOver
        then do
          -- GAME OVER
          putStrLn $ "[GameLoop " ++ roomId ++ "] Match Over. Winner: " ++ show mWinnerId
          
          let newState = gs_respawned { rgsMatchState = GameOver mWinnerId, rgsCommands = [] }
          let packet = SUP_MatchStateUpdate (GameOver mWinnerId)
          -- Gửi gói tin cho tất cả người chơi (kể cả người đã chết)
          pure (newState, [(packet, Map.keys (rgsPlayers gs_respawned))])
        else do
          -- GAME TIẾP TỤC
          let alivePlayers = Map.filter (\p -> psLives p > 0) (rgsPlayers gs_respawned)
          let snapshot = WorldSnapshot
                { wsPlayers = Map.elems alivePlayers -- Chỉ gửi người còn sống
                , wsEnemies = rgsEnemies gs_respawned
                , wsBullets = rgsBullets gs_respawned
                }
          let snapshotPacket = SUP_Snapshot snapshot
          let finalState = gs_respawned { rgsPlayers = alivePlayers, rgsCommands = [], rgsTick = rgsTick gs + 1 }
          -- Gửi snapshot cho những người còn sống
          pure (finalState, [(snapshotPacket, Map.keys alivePlayers)])

    GameOver winnerId -> do
      -- Game đã kết thúc, chỉ giữ state và chờ TCP xử lý (ví dụ: Rematch)
      -- Chúng ta sẽ ngắt vòng lặp này
      putStrLn $ "[GameLoop " ++ roomId ++ "] Loop ended."
      putMVar roomStateRef gs { rgsCommands = [] } -- Đặt lại MVar
      
      -- Gửi 1 lần cuối
      let packet = SUP_MatchStateUpdate (GameOver winnerId)
      pure (gs, [(packet, Map.keys (rgsPlayers gs))])
  
  -- Gửi các gói tin UDP
  when (not (null packetsToSend)) $ do
    sState <- readMVar serverStateRef
    let sock = ssUdpSocket sState
    mapM_ (\(pkt, addrs) -> do
      let lazyPkt = encode pkt
      let strictPkt = toStrict lazyPkt
      -- Gửi đến các địa chỉ UDP đã đăng ký
      mapM_ (BS.sendTo sock strictPkt) addrs
      ) packetsToSend

  putMVar roomStateRef finalGameState
  
  -- Dừng vòng lặp nếu game over
  when (rgsMatchState finalGameState /= InProgress) $ do
    putStrLn $ "[GameLoop " ++ roomId ++ "] Exiting loop."
    -- Xóa phòng khỏi ServerState
    modifyMVar_ serverStateRef $ \sState ->
      pure sState { ssRooms = Map.delete roomId (ssRooms sState) }
    -- Thoát khỏi `forever`
    fail "Game loop finished." 
    
  threadDelay tickInterval
  ) `catch` \(e :: SomeException) -> do -- <<<<<< DÒNG NÀY GÂY LỖI, GIỜ ĐÃ SỬA
    -- Bắt lỗi "fail" để thoát vòng lặp
    putStrLn $ "[GameLoop " ++ roomId ++ "] Loop exited cleanly."
    pure ()