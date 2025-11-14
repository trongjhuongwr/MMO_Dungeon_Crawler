{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Move brackets to avoid $" #-}

module GameLoop (gameLoop) where

import Control.Concurrent (threadDelay, MVar, newMVar, takeMVar, putMVar, readMVar, modifyMVar, modifyMVar_)
import Control.Monad (forever, when, void)
import Network.Socket
import qualified Data.Map as Map
import Data.List (find, nub)
import Data.Maybe (Maybe(..), listToMaybe)
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
import Types.Common (Vec2(..), (*^))
import Types.MatchState (MatchState(..))
import Types.GameMode (GameMode(..))
import qualified Utils.Random as Rnd
import Types.Map (GameMap(..), isSolid)
import qualified Data.Array as Array

tickRate :: Int
tickRate = 30

tickInterval :: Int
tickInterval = 1000000 `div` tickRate

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
          
          -- [THAY ĐỔI] Gọi hàm IO mới để respawn
          gs_respawned <- respawnDeadPlayersIO gs_filtered_entities
          
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

-- ================================================================
-- LOGIC RESPAWN NGẪU NHIÊN
-- ================================================================
-- | Hàm IO chính để respawn người chơi, thay thế cho hàm thuần túy cũ
respawnDeadPlayersIO :: RoomGameState -> IO RoomGameState
respawnDeadPlayersIO gs = do
  -- Dùng traverseWithKey (tương đương mapM) để áp dụng hàm IO
  newPlayersMap <- Map.traverseWithKey (respawnPlayerIO gs) (rgsPlayers gs)
  pure $ gs { rgsPlayers = newPlayersMap }

-- [THÊM MỚI]
-- | Logic respawn IO cho từng người chơi
respawnPlayerIO :: RoomGameState -> SockAddr -> PlayerState -> IO PlayerState
respawnPlayerIO gs addr p = do
  if psHealth p <= 0 && psLives p > 0
    then do
      -- 1. Tìm một đối thủ (bất kỳ ai không phải 'p')
      -- Logic này hoạt động cho cả PvP (tìm người kia) và PvE (người tìm bot / bot tìm người)
      let mOpponent = listToMaybe $ Map.elems $ Map.delete addr (rgsPlayers gs)
      
      newPos <- case mOpponent of
        Nothing -> do 
          -- Không tìm thấy ai khác, hoặc là người cuối cùng
          -- Dùng lại logic spawn cũ
          let spawnPoints = rgsSpawns gs
          let fallbackPos = if null spawnPoints
                              then Vec2 100 100 -- Fallback an toàn
                              else spawnPoints !! (psId p `mod` length spawnPoints)
          pure fallbackPos
        
        Just opponent -> do
          -- 2. Tìm vị trí hồi sinh hợp lệ, cách xa đối thủ
          -- Thử 10 lần, nếu không được sẽ dùng fallback
          findValidRespawnPos (rgsMap gs) (psPosition opponent) 10

      -- 3. Hồi sinh người chơi
      pure $ p { psHealth = 100, psPosition = newPos }
      
    else 
      pure p -- Không cần respawn

-- [THÊM MỚI]
-- | Đệ quy tìm vị trí hồi sinh hợp lệ, giới hạn số lần thử
findValidRespawnPos :: GameMap -> Vec2 -> Int -> IO Vec2
findValidRespawnPos gameMap opponentPos attemptsLeft = do
  if attemptsLeft <= 0
    then do
      putStrLn $ "[GameLoop] Không tìm thấy vị trí respawn hợp lệ. Dùng fallback (100, 100)."
      pure (Vec2 100 100) -- Vị trí fallback an toàn
    else do
      -- 1. Lấy góc ngẫu nhiên (0 -> 2*PI)
      angle <- Rnd.getRandomFloat (0, 2 * pi)
      
      -- 2. Lấy khoảng cách ngẫu nhiên (ví dụ: 200-400 units)
      let minSpawnDist = 600.0
      let maxSpawnDist = 800.0
      dist <- Rnd.getRandomFloat (minSpawnDist, maxSpawnDist)
      
      -- 3. Tính vị trí mới
      let offsetVec = Vec2 (sin angle) (cos angle) *^ dist
      let newPos = opponentPos + offsetVec
      
      -- 4. Kiểm tra va chạm
      if not (isPositionColliding gameMap newPos)
        then pure newPos -- Vị trí hợp lệ
        else findValidRespawnPos gameMap opponentPos (attemptsLeft - 1) -- Thử lại

-- ================================================================
-- [THÊM MỚI] CÁC HÀM HELPER KIỂM TRA VA CHẠM
-- (Sao chép từ PhysicsSystem.hs để tránh circular dependency)
-- ================================================================

playerRadius :: Float
playerRadius = 16.0 

tileSize :: Float
tileSize = 32.0

worldToGrid :: Vec2 -> (Int, Int)
worldToGrid (Vec2 x y) =
  ( floor (y / tileSize)
  , floor (x / tileSize)
  )

isTileSolidAtGrid :: GameMap -> (Int, Int) -> Bool
isTileSolidAtGrid gmap (gy, gx) =
  let
    (yMin, xMin) = fst (Array.bounds (gmapTiles gmap))
    (yMax, xMax) = snd (Array.bounds (gmapTiles gmap))
    
    isOutOfBounds = gy < yMin || gy > yMax || gx < xMin || gx > xMax
  in
    if isOutOfBounds
      then True 
      else
        let tile = (gmapTiles gmap) Array.! (gy, gx)
        in isSolid tile

isPositionColliding :: GameMap -> Vec2 -> Bool
isPositionColliding gmap pos =
  let
    (Vec2 x y) = pos
    r = playerRadius
    
    posTopLeft  = Vec2 (x - r) (y + r)
    posTopRight = Vec2 (x + r) (y + r)
    posBotLeft  = Vec2 (x - r) (y - r)
    posBotRight = Vec2 (x + r) (y - r)

    gridCoords = nub 
      [ worldToGrid posTopLeft
      , worldToGrid posTopRight
      , worldToGrid posBotLeft
      , worldToGrid posBotRight
      ]
      
  in
    any (isTileSolidAtGrid gmap) gridCoords