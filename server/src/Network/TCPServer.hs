{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Network.TCPServer (startTcpServer) where

import Control.Monad (forever, when, void) -- << SỬA: Thêm 'void'
import Control.Concurrent (forkIO, MVar, modifyMVar, readMVar, newMVar, modifyMVar_)
import Network.Socket
import System.IO
import qualified Data.Map as Map
import Data.Binary (encode, decode, decodeOrFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (isJust, fromMaybe)
-- Sửa: Xóa 'fromJust' không an toàn, chúng ta sẽ dùng pattern matching
import Data.List (find)
import Control.Exception (catch, SomeException, bracket)

import Core.Types
import Types.Tank 
import qualified Types.Tank as Tank
import Network.Packet
import GameLoop (gameLoop) 
import Types.MatchState (MatchState(..)) 
import Types.Common (Vec2(..)) 
import qualified Core.Settings as Settings
import qualified Utils.Random as Rnd (getRandomNumber)
import qualified Data.ByteString as BS 
import qualified Data.ByteString as BS (hPut)
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Types.GameMode (GameMode(..))

-- Hàm tiện ích để gửi gói tin TCP (an toàn)
sendTcpPacket :: Handle -> ServerTcpPacket -> IO ()
sendTcpPacket h pkt = (do
  let lazyMsg = encode pkt
  let strictMsg = toStrict lazyMsg -- Ép thành strict
  BS.hPut h strictMsg               -- Gửi strict
  hFlush h
  ) `catch` \(e :: SomeException) ->
    putStrLn $ "[TCP] Failed to send packet to handle: " ++ show e

-- Hàm tiện ích để tạo Room ID
generateRoomId :: IO String
generateRoomId = do
  num <- Rnd.getRandomNumber
  -- Đảm bảo Room ID có 4 chữ số
  pure $ take 4 (show (abs num `mod` 10000 + 10000))

tcpPort :: PortNumber
tcpPort = Settings.getServerTcpPort

startTcpServer :: MVar ServerState -> IO ()
startTcpServer serverStateRef = withSocketsDo $ do
  addr <- resolve (show tcpPort)
  -- Dùng bracket để đảm bảo socket chính được đóng khi server tắt
  bracket (open addr) close $ \sock -> do
    putStrLn $ "[TCP Server] Listening on port " ++ show tcpPort
    forever $ do
      (conn, clientAddr) <- accept sock
      putStrLn $ "[TCP Server] Client connected from: " ++ show clientAddr
      -- forkIO cho mỗi client mới
      _ <- forkIO $ handleClient conn clientAddr serverStateRef
      return ()
  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream, addrFamily = AF_INET }
      head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

handleClient :: Socket -> SockAddr -> MVar ServerState -> IO ()
handleClient sock addr serverStateRef = do
  setSocketOption sock NoDelay 1 -- <--- THÊM DÒNG NÀY
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering -- Rất quan trọng
  
  -- Vòng lặp đọc-xử lý, bắt exception (như disconnect)
  loop h Nothing LBS.empty `catch` \(e :: SomeException) -> do
    putStrLn $ "[TCP] Terminating client loop for " ++ show addr ++ ". Reason: " ++ show e
    -- Đảm bảo ngắt kết nối an toàn nếu vòng lặp bị crash
    handleDisconnect h Nothing serverStateRef

  where
    -- Vòng lặp chính đọc dữ liệu từ Handle
    loop :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    loop h mPlayerId buffer = do
  -- 1. ĐỌC CHUNK NGHIÊM NGẶT (STRICT)
      strictChunk <- (BS.hGetSome h 8192) `catch` \(e :: SomeException) -> do
        putStrLn $ "[TCP] hGetSome Error (" ++ show addr ++ "): " ++ show e
        pure BS.empty -- Coi như client ngắt kết nối

  -- 2. KIỂM TRA NGẮT KẾT NỐI
      if BS.null strictChunk && LBS.null buffer
      then handleDisconnect h mPlayerId serverStateRef -- Client đóng kết nối
      else do
    -- 3. NỐI BUFFER LƯỜI (LAZY)
        let fullBuffer = buffer <> fromStrict strictChunk
    -- Xử lý buffer (hàm processBuffer không đổi)
        processBuffer h mPlayerId fullBuffer

    -- Xử lý buffer một cách đệ quy
    processBuffer :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    processBuffer h mPlayerId buffer = do
      -- Thử decode một gói tin từ đầu buffer
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ClientTcpPacket)
      
      case decodeResult of
        -- 1. Không đủ dữ liệu -> Quay lại vòng lặp 'loop' để đọc thêm
        Left (_, _, _) -> do
          loop h mPlayerId buffer
            
        -- 2. Decode thành công -> Xử lý gói tin và buffer còn lại
        Right (remainingBuffer, _, pkt) -> do
          
          -- BƯỚC QUAN TRỌNG:
          -- Toàn bộ logic thay đổi ServerState phải nằm trong MỘT khối modifyMVar
          -- để đảm bảo tính nguyên tử (atomicity).
          (new_mPlayerId, actions) <- modifyMVar serverStateRef $ \sState -> do
            
            -- 2a. Lấy/Tạo PID và State mới
            (pid, sState_after_pid) <- case mPlayerId of
              Just pid -> pure (pid, sState) -- Client đã login, dùng pid cũ
              Nothing -> case pkt of
                -- Gói tin đầu tiên phải là Login
                CTP_Login _ _ -> do
                  let newId = ssNextPlayerId sState
                  let newName = "Player" ++ show newId
                  let newPlayerInfo = PlayerInfo newId newName Nothing False
                  let newClient = PlayerClient h newPlayerInfo Nothing
                  let newClients = Map.insert newId newClient (ssClients sState)
                  let newState = sState { ssClients = newClients, ssNextPlayerId = newId + 1 }
                  putStrLn $ "[TCP] Client assigned ID: " ++ show newId
                  pure (newId, newState)
                -- Nếu không, đó là lỗi
                _ -> fail $ "Received packet before login from " ++ show addr ++ ": " ++ show pkt
            
            putStrLn $ "[TCP] Received from " ++ show pid ++ ": " ++ show pkt

            -- 2b. Xử lý Packet (PURE logic), trả về State mới và [IO Action]
            (sState_final, ioActions) <- processPacket pid h pkt sState_after_pid serverStateRef
            
            -- 2c. Trả về state cuối cùng và các hành động
            pure (sState_final, (Just pid, ioActions))
          
          -- 3. Thực thi các hành động IO (bên ngoài MVar)
          sequence_ actions
          
          -- 4. Đệ quy: Xử lý phần buffer còn dư
          if LBS.null remainingBuffer
            then loop h new_mPlayerId remainingBuffer -- Hết buffer, quay lại chờ
            else do
              processBuffer h new_mPlayerId remainingBuffer -- Xử lý ngay phần còn lại

    -- Xử lý ngắt kết nối
    handleDisconnect :: Handle -> Maybe Int -> MVar ServerState -> IO ()
    handleDisconnect h mPid serverStateRef = do
      putStrLn $ "[TCP] Client disconnected: " ++ show mPid
      hClose h
      case mPid of
        Nothing -> pure () -- Chưa login, không cần làm gì
        Just pid -> do
          -- Lấy ra các hành động (như broadcast) cần làm KHI xóa client
          actionsToRun <- modifyMVar serverStateRef $ \sState -> do
            let newClients = Map.delete pid (ssClients sState)
            let (mRoom, mRoomId) = findRoomByPlayerId pid sState
            
            case (mRoom, mRoomId) of
              (Just room, Just roomId) -> do
                let newPlayers = Map.delete pid (roomPlayers room)
                if Map.null newPlayers
                then do
                  -- Phòng rỗng, xóa phòng
                  putStrLn $ "[TCP] Room " ++ roomId ++ " is empty. Deleting."
                  let newState = sState { ssClients = newClients, ssRooms = Map.delete roomId (ssRooms sState) }
                  pure (newState, []) -- Không ai ở lại, không cần broadcast
                else do
                  -- Phòng còn người, cập nhật
                  let updatedRoom = room { roomPlayers = newPlayers }
                  let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
                  let newState = sState { ssClients = newClients, ssRooms = newRooms }
                  let actions = broadcastRoomUpdate updatedRoom -- Lấy [IO ()]
                  pure (newState, actions)
              _ -> pure (sState { ssClients = newClients }, []) -- Không ở trong phòng
          
          -- Chạy các hành động (broadcast) bên ngoài MVar
          sequence_ actionsToRun

-- | Hàm PURE xử lý gói tin, trả về State mới và [IO Action]
-- | Hàm này CHẠY BÊN TRONG modifyMVar
processPacket :: Int -> Handle -> ClientTcpPacket -> ServerState -> MVar ServerState -> IO (ServerState, [IO ()])
processPacket pid h pkt sState serverStateRef =
  case pkt of
    CTP_Login name _ -> do
      let client = ssClients sState Map.! pid
      let newPlayerInfo = (pcInfo client) { piName = name }
      let newClient = client { pcInfo = newPlayerInfo }
      let newClients = Map.insert pid newClient (ssClients sState)
      let action = sendTcpPacket h (STP_LoginResult True pid "Login successful")
      pure (sState { ssClients = newClients }, [action])

    CTP_CreateRoom -> do
      newRoomId <- generateRoomId
      let client = ssClients sState Map.! pid
      let newRoom = Room newRoomId (Map.singleton pid client) Nothing
      let newRooms = Map.insert newRoomId newRoom (ssRooms sState)
      let action = sendTcpPacket h (STP_RoomUpdate newRoomId [pcInfo client])
      pure (sState { ssRooms = newRooms }, [action])

    CTP_JoinRoom roomId -> do
      case Map.lookup roomId (ssRooms sState) of
        Nothing -> do
          let action = sendTcpPacket h (STP_Kicked "Room not found")
          pure (sState, [action])
        Just room -> do
          let client = ssClients sState Map.! pid
          let newPlayers = Map.insert pid client (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers }
          let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
          let actions = broadcastRoomUpdate updatedRoom -- Lấy list [IO ()]
          pure (sState { ssRooms = newRooms }, actions)
      
    CTP_StartDungeon mTank -> do
      putStrLn $ "[TCP] Client " ++ show pid ++ " requested Dungeon start."
      -- 1. Get client
      let client = ssClients sState Map.! pid

      -- 2. Tạo Room ID solo
      newRoomId <- generateRoomId
      putStrLn $ "[TCP] Creating solo dungeon " ++ newRoomId ++ " for " ++ show pid

      -- 3. Get spawn point
      let spawns = ssSpawns sState
      let spawnPos = if not (null spawns) then spawns !! 0 else Vec2 100 100

      -- 4. TẠO PLAYERSTATE (SỬ DỤNG TANK ĐÃ CHỌN)
      let selectedTank = fromMaybe Tank.Rapid mTank
      let playerState = initialPlayerState spawnPos pid selectedTank

      -- 5. Tạo "fake" addr
      let fakeAddr = SockAddrInet (fromIntegral pid) (tupleToHostAddress (127,0,0,1))
      let playerStates = Map.singleton fakeAddr playerState

      -- 6. Tạo GameState (với mode PvE)
      let newRoomGame = initialRoomGameState (ssMap sState) spawns PvE
      let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }
      roomGameMVar <- newMVar newRoomGame'

      -- 7. Tạo và lưu Room
      let newRoom = Room newRoomId (Map.singleton pid client) (Just roomGameMVar)
      let newRooms = Map.insert newRoomId newRoom (ssRooms sState)
      let newState = sState { ssRooms = newRooms }

      -- 8. Chuẩn bị actions: fork loop + gửi "starting"
      let gameLoopAction = forkIO $ gameLoop serverStateRef newRoomId roomGameMVar
      let clientAction = sendTcpPacket h (STP_GameStarting PvE)

      pure (newState, [void gameLoopAction, clientAction])

    CTP_UpdateLobbyState mTank isReady -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId) of
        (Just room, Just roomId) -> do
          let client = roomPlayers room Map.! pid
          let newPlayerInfo = (pcInfo client) { piSelectedTank = mTank, piIsReady = isReady }
          let newClient = client { pcInfo = newPlayerInfo }
          let newPlayers = Map.insert pid newClient (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers }
          let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
          
          let (gameCanStart, pInfos) = checkGameStart updatedRoom
          if gameCanStart
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is starting game!"
              let p1_info = head pInfos
              let p2_info = pInfos !! 1
              
              -- Lấy spawn points từ state
              let spawns = ssSpawns sState
              let p1_spawn = if not (null spawns) then spawns !! 0 else Vec2 100 100
              let p2_spawn = if length spawns > 1 then spawns !! 1 else Vec2 700 500

              -- Đảm bảo chỉ lấy TankType nếu nó thực sự tồn tại
              case (piSelectedTank p1_info, piSelectedTank p2_info) of
                (Just tank1, Just tank2) -> do
                  let p1_state = initialPlayerState p1_spawn (piId p1_info) tank1
                  let p2_state = initialPlayerState p2_spawn (piId p2_info) tank2
                  
                  -- Địa chỉ "giả" để làm key, sẽ được UDP handshake cập nhật
                  let fakeAddr1 = SockAddrInet 1 (tupleToHostAddress (127,0,0,1))
                  let fakeAddr2 = SockAddrInet 2 (tupleToHostAddress (127,0,0,2))
                  
                  let playerStates = Map.fromList [(fakeAddr1, p1_state), (fakeAddr2, p2_state)]
                  
                  let newRoomGame = initialRoomGameState (ssMap sState) (ssSpawns sState) PvP
                  let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }
                  
                  roomGameMVar <- newMVar newRoomGame'
                  
                  let finalRoom = updatedRoom { roomGame = Just roomGameMVar }
                  let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
                  
                  -- << SỬA LỖI 1 & 2 >>
                  -- 1. Tạo IO Action để fork game loop
                  -- 2. Truyền MVar state gốc, không phải (pure sState)
                  let gameLoopAction = forkIO $ gameLoop serverStateRef roomId roomGameMVar
                  
                  -- 3. Lấy actions broadcast
                  let broadcastActions = broadcastToRoom finalRoom (STP_GameStarting PvP)
                  
                  -- 4. Gộp actions, chuyển `forkIO` thành `IO ()` bằng `void`
                  let allActions = void gameLoopAction : broadcastActions
                  
                  -- 5. Trả về state MỚI và [IO ()]
                  pure (sState { ssRooms = finalRooms }, allActions)

                _ -> do
                  -- Trường hợp lỗi (không thể xảy ra nếu checkGameStart đúng)
                  putStrLn "[TCP] Lỗi: Game start check-pass nhưng tank type là Nothing."
                  let actions = broadcastRoomUpdate updatedRoom
                  pure (sState { ssRooms = newRooms }, actions)
            else
              -- Chỉ broadcast
              let actions = broadcastRoomUpdate updatedRoom
              in pure (sState { ssRooms = newRooms }, actions)
        _ -> pure (sState, []) -- Không tìm thấy phòng

    CTP_PauseGame isPaused -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId) of
        (Just room, Just roomId) ->
          case roomGame room of
            Just gameMVar -> do
              -- Tạo một IO action để cập nhật MVar của game
              let action = modifyMVar_ gameMVar $ \rgs -> do
                    if (rgsMode rgs == PvE)
                      then do
                        putStrLn $ "[GameLoop " ++ roomId ++ "] Setting Paused: " ++ show isPaused
                        pure $ rgs { rgsIsPaused = isPaused }
                      else 
                        pure rgs -- Không cho phép pause PvP
              pure (sState, [action])
            _ -> pure (sState, []) -- Game không chạy
        _ -> pure (sState, []) -- Không tìm thấy phòng

    CTP_LeaveRoom -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId) of
        (Just room, Just roomId) -> do
          -- TỰ ĐỘNG UNPAUSE KHI RỜI PHÒNG
          let unpauseAction = case roomGame room of
                Just gameMVar -> [modifyMVar_ gameMVar (\rgs -> pure rgs { rgsIsPaused = False })]
                Nothing -> []

          let newPlayers = Map.delete pid (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers }

          if Map.null newPlayers
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is empty. Deleting."
              let newState = sState { ssClients = Map.delete pid (ssClients sState), ssRooms = Map.delete roomId (ssRooms sState) }
              pure (newState, unpauseAction) -- Chạy unpause
            else do
              let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
              let newState = sState { ssClients = Map.delete pid (ssClients sState), ssRooms = newRooms }
              let actions = broadcastRoomUpdate updatedRoom ++ unpauseAction -- Gộp actions
              pure (newState, actions)
        _ -> pure (sState { ssClients = Map.delete pid (ssClients sState) }, []) -- Rời khỏi sảnh (không ở trong phòng) 

    CTP_RequestRematch -> do
      -- TODO: Xử lý logic rematch (hiện tại chưa làm gì)
      pure (sState, [])

-- === HÀM TIỆN ÍCH ===

-- | Tìm phòng của người chơi (PURE)
findRoomByPlayerId :: Int -> ServerState -> (Maybe Room, Maybe String)
findRoomByPlayerId pid sState =
  let found = find (\(_, room) -> Map.member pid (roomPlayers room)) (Map.assocs (ssRooms sState))
  in case found of
    Just (roomId, room) -> (Just room, Just roomId)
    Nothing -> (Nothing, Nothing)

-- | Tạo [IO ()] để broadcast
broadcastRoomUpdate :: Room -> [IO ()]
broadcastRoomUpdate room =
  let playerInfos = map pcInfo (Map.elems (roomPlayers room))
      packet = STP_RoomUpdate (roomMsgId room) playerInfos
  in broadcastToRoom room packet

-- | Tạo [IO ()] để broadcast
broadcastToRoom :: Room -> ServerTcpPacket -> [IO ()]
broadcastToRoom room packet =
  map (\client -> sendTcpPacket (pcHandle client) packet) (Map.elems (roomPlayers room))

-- | Kiểm tra xem game có thể bắt đầu (PURE)
checkGameStart :: Room -> (Bool, [PlayerInfo])
checkGameStart room =
  let infos = map pcInfo (Map.elems (roomPlayers room))
      allReady = all piIsReady infos
      allSelected = all (isJust . piSelectedTank) infos
      playerCount = length infos
  in (playerCount == 2 && allReady && allSelected, infos)