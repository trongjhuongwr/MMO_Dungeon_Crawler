{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Network.TCPServer (startTcpServer) where

import Control.Monad (forever, when, void)
import Control.Concurrent (forkIO, MVar, modifyMVar, readMVar, newMVar, modifyMVar_, tryReadMVar)
import Network.Socket
import System.IO
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Binary (encode, decode, decodeOrFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.List (find)
import Control.Exception (catch, SomeException, bracket, try)
import Database.SQLite.Simple (Connection)
import Data.Queries.PlayerQuery (registerUser, authenticateUser)
import qualified Data.Text as T 

import Core.Types
import Types.Tank 
import qualified Types.Tank as Tank
import Network.Packet
import GameLoop (gameLoop) 
import Types.MatchState (MatchState(..)) 
import Core.Config (AppConfig(..))
import qualified Utils.Random as Rnd (getRandomNumber)
import qualified Data.ByteString as BS 
import qualified Data.ByteString as BS (hPut)
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Types.GameMode (GameMode(..))
import Types.Map (GameMap(..))
import Types.Player (PlayerState(..))
import Types.Common (Vec2(..))
import qualified Systems.AISystem as AISystem 

serverTileSize :: Float
serverTileSize = 32.0

-- Hàm tiện ích để gửi gói tin TCP (an toàn)
sendTcpPacket :: Handle -> ServerTcpPacket -> IO ()
sendTcpPacket h pkt = (do
  let lazyMsg = encode pkt
  let strictMsg = toStrict lazyMsg  -- Ép thành strict
  BS.hPut h strictMsg               -- Gửi strict
  hFlush h
  ) `catch` \(e :: SomeException) ->
    putStrLn $ "[TCP] Failed to send packet to handle: " ++ show e

-- Hàm tiện ích để tạo Room ID
generateRoomId :: IO String
generateRoomId = do
  num <- Rnd.getRandomNumber
  pure $ take 4 (show (abs num `mod` 10000 + 10000)) -- Đảm bảo Room ID có 4 chữ số


startTcpServer :: AppConfig -> MVar ServerState -> IO ()
startTcpServer config serverStateRef = withSocketsDo $ do
  let tcpPort = port config
  addr <- resolve (show tcpPort)
  -- Dùng bracket để đảm bảo socket chính được đóng khi server tắt
  bracket (open addr) close $ \sock -> do
    putStrLn $ "[TCP Server] Listening on port " ++ show tcpPort
    forever $ do
      (conn, clientAddr) <- accept sock
      putStrLn $ "[TCP Server] Client connected from: " ++ show clientAddr
      _ <- forkIO $ handleClient conn clientAddr serverStateRef -- forkIO cho mỗi client mới
      return ()
  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream, addrFamily = AF_INET, addrProtocol = defaultProtocol }
      head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

handleClient :: Socket -> SockAddr -> MVar ServerState -> IO ()
handleClient sock addr serverStateRef = do
  setSocketOption sock NoDelay 1 -- Tắt Nagle's algorithm để giảm độ trễ
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering    -- Tắt buffering để gửi nhận ngay lập tức
  
  -- Vòng lặp đọc-xử lý, bắt exception như disconnect
  loop h Nothing LBS.empty `catch` \(e :: SomeException) -> do
    putStrLn $ "[TCP] Terminating client loop for " ++ show addr ++ ". Reason: " ++ show e
    handleDisconnect h Nothing serverStateRef -- Đảm bảo ngắt kết nối an toàn nếu vòng lặp bị crash

  where
    -- Vòng lặp chính đọc dữ liệu từ Handle
    loop :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    loop h mPlayerId buffer = do
      strictChunk <- (BS.hGetSome h 8192) `catch` \(e :: SomeException) -> do
        putStrLn $ "[TCP] hGetSome Error (" ++ show addr ++ "): " ++ show e
        pure BS.empty
        
      -- Kiểm tra ngắt kết nối
      if BS.null strictChunk && LBS.null buffer
      then handleDisconnect h mPlayerId serverStateRef
      else do
        let fullBuffer = buffer <> fromStrict strictChunk
        processBuffer h mPlayerId fullBuffer

    -- Xử lý buffer
    processBuffer :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    processBuffer h mPlayerId buffer = do
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ClientTcpPacket)
      -- Phân tích kết quả decode
      case decodeResult of
        -- Không đủ dữ liệu -> Quay lại vòng lặp 'loop' để đọc thêm
        Left (_, _, _) -> do
          loop h mPlayerId buffer
            
        -- Decode thành công -> Xử lý gói tin và buffer còn lại
        Right (remainingBuffer, _, pkt) -> do
          
          -- Nếu client chưa login (mPlayerId == Nothing) thì packet phải là Login hoặc Register
          when (isNothing mPlayerId && not (isLoginOrRegisterPkt pkt)) $
            fail $ "Received packet before login from " ++ show addr ++ ": " ++ show pkt

          (new_mPlayerId, actions) <- modifyMVar serverStateRef $ \sState -> do
            
            putStrLn $ "[TCP] Received from " ++ show (fromMaybe 0 mPlayerId) ++ ": " ++ show pkt
            
            -- Xử lý Packet, trả về State mới và (Maybe PlayerID, [IO Action])
            (sState_final, pid_action) <- processPacket (ssDbConn sState) mPlayerId h pkt sState serverStateRef
            
            -- Trả về state cuối cùng và các hành động
            pure (sState_final, pid_action)
          
          -- Thực thi các hành động IO (bên ngoài MVar)
          sequence_ actions
          
          -- Xử lý phần buffer còn dư
          if LBS.null remainingBuffer
            then loop h new_mPlayerId remainingBuffer -- Hết buffer, quay lại chờ
            else do
              processBuffer h new_mPlayerId remainingBuffer -- Xử lý ngay phần còn lại
    
    isLoginOrRegisterPkt (CTP_Login _ _) = True
    isLoginOrRegisterPkt (CTP_Register _ _) = True
    isLoginOrRegisterPkt _ = False

    -- Xử lý ngắt kết nối
    handleDisconnect :: Handle -> Maybe Int -> MVar ServerState -> IO ()
    handleDisconnect h mPid serverStateRef = do
      putStrLn $ "[TCP] Client disconnected: " ++ show mPid
      hClose h
      -- Cập nhật ServerState để xóa client
      case mPid of
        Nothing -> pure ()
        Just pid -> do
          -- Lấy ra các hành động cần làm khi xóa client (ví dụ như broadcast)
          actionsToRun <- modifyMVar serverStateRef $ \sState -> do
            let newClients = Map.delete pid (ssClients sState)
            let (mRoom, mRoomId) = findRoomByPlayerId pid sState

            -- Nếu client ở trong phòng, xóa khỏi phòng và broadcast cập nhật
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
                  -- Reset rematch nếu có người ngắt kết nối
                  let updatedRoom = room { roomPlayers = newPlayers, roomRematchRequests = Set.empty }
                  let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
                  let newState = sState { ssClients = newClients, ssRooms = newRooms }
                  let roomUpdateActions = broadcastRoomUpdate updatedRoom
                  let rematchClearActions = broadcastToRoom updatedRoom (STP_RematchUpdate [])
                  pure (newState, roomUpdateActions ++ rematchClearActions)
              _ -> pure (sState { ssClients = newClients }, []) -- Không ở trong phòng
          
          -- Chạy các hành động (broadcast) bên ngoài MVar
          sequence_ actionsToRun

-- Hàm thuần túy xử lý gói tin, trả về State mới và [IO Action]
processPacket :: Connection -> Maybe Int -> Handle -> ClientTcpPacket -> ServerState -> MVar ServerState -> IO (ServerState, (Maybe Int, [IO ()]))
processPacket dbConn mPid h pkt sState serverStateRef =
  -- Xử lý dựa trên loại gói tin và trạng thái đăng nhập
  case (pkt, mPid) of
    -- Xử lý đăng ký
    (CTP_Register user pass, Nothing) -> do
      eResult <- try (registerUser dbConn user pass) :: IO (Either SomeException (Either String Int))
      case eResult of
        Left dbErr -> do
          putStrLn $ "[DB Error] Register: " ++ show dbErr
          let action = sendTcpPacket h (STP_LoginResult False 0 "" "Registration failed (DB Error)")
          pure (sState, (Nothing, [action]))
        Right (Left errMsg) -> do
          putStrLn $ "[Auth] Register failed: " ++ errMsg
          let action = sendTcpPacket h (STP_LoginResult False 0 "" errMsg)
          pure (sState, (Nothing, [action]))
        Right (Right newId) -> do
          putStrLn $ "[Auth] Register success for " ++ user ++ " (ID: " ++ show newId ++ ")"
          let action = sendTcpPacket h (STP_LoginResult False 0 "" "Registration successful. Please log in.")
          pure (sState, (Nothing, [action]))

    -- Xử lý đăng nhập
    (CTP_Login user pass, Nothing) -> do
      eResult <- try (authenticateUser dbConn user pass) :: IO (Either SomeException (Maybe (Int, T.Text)))
      case eResult of
        Left dbErr -> do
          putStrLn $ "[DB Error] Login: " ++ show dbErr
          let action = sendTcpPacket h (STP_LoginResult False 0 "" "Login failed (DB Error)")
          pure (sState, (Nothing, [action]))
        Right Nothing -> do
          putStrLn $ "[Auth] Login failed for " ++ user
          let action = sendTcpPacket h (STP_LoginResult False 0 "" "Invalid username or password")
          pure (sState, (Nothing, [action]))
        Right (Just (pid, username)) -> do
          putStrLn $ "[Auth] Login success for " ++ user ++ " (ID: " ++ show pid ++ ")"
          
          -- Tạo PlayerClient và thêm vào ServerState
          let strUsername = T.unpack username 
          let newPlayerInfo = PlayerInfo pid strUsername Nothing False
          let newClient = PlayerClient h newPlayerInfo Nothing
          let newClients = Map.insert pid newClient (ssClients sState)
          -- Cập nhật NextPlayerId để tránh trùng lặp nếu server restart nhưng DB thì không
          let newState = sState { ssClients = newClients, ssNextPlayerId = max (ssNextPlayerId sState) (pid + 1) }
          let action = sendTcpPacket h (STP_LoginResult True pid strUsername "Login successful")
          -- Trả về (Just pid) để cập nhật state của vòng lặp
          pure (newState, (Just pid, [action]))

    -- Xử lý tạo phòng
    (CTP_CreateRoom, Just pid) -> do
      newRoomId <- generateRoomId
      let client = ssClients sState Map.! pid
      let newRoom = Room newRoomId (Map.singleton pid client) Nothing Set.empty
      let newRooms = Map.insert newRoomId newRoom (ssRooms sState)
      let action = sendTcpPacket h (STP_RoomUpdate newRoomId [pcInfo client])
      pure (sState { ssRooms = newRooms }, (Just pid, [action]))

    -- Xử lý tham gia phòng
    (CTP_JoinRoom roomId, Just pid) -> do
      case Map.lookup roomId (ssRooms sState) of
        Nothing -> do
          let action = sendTcpPacket h (STP_Kicked "Room not found")
          pure (sState, (Just pid, [action]))
        
        Just room ->
          -- Dùng if-then-else lồng nhau để tránh việc đọc ssRooms nhiều lần trong MVar
          if Map.size (roomPlayers room) >= 2
            then do
              let action = sendTcpPacket h (STP_Kicked "Room is full")
              pure (sState, (Just pid, [action]))
            else if isJust (roomGame room)
              then do
                let action = sendTcpPacket h (STP_Kicked "Game already in progress")
                pure (sState, (Just pid, [action]))
              else do
                let client = ssClients sState Map.! pid
                let newPlayers = Map.insert pid client (roomPlayers room)
                let updatedRoom = room { roomPlayers = newPlayers }
                let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
                let actions = broadcastRoomUpdate updatedRoom
                pure (sState { ssRooms = newRooms }, (Just pid, actions))
    
    -- Xử lý bắt đầu trận PvE Bot Match
    (CTP_StartPvEBotMatch myTank botTank, Just pid) -> do
      putStrLn $ "[TCP] Client " ++ show pid ++ " starting PvE Bot Match."
      -- Tạo phòng mới
      newRoomId <- generateRoomId
      let client = ssClients sState Map.! pid
      -- Tạo GameState
      let spawns = ssSpawns sState
      let gameMap = ssMap sState
      let GameMap { gmapWidth = gw, gmapHeight = gh } = gameMap
      let mapCenter = Vec2 (fromIntegral gw * serverTileSize / 2.0)
                           (fromIntegral gh * serverTileSize / 2.0)

      let p1_spawn = if not (null spawns) then spawns !! 0 else Vec2 100 100
      let p2_spawn = if length spawns > 1 then spawns !! 1 else Vec2 700 500

      let p1_angle = atan2 (vecX (mapCenter - p1_spawn)) (vecY (mapCenter - p1_spawn))
      let p2_angle = atan2 (vecX (mapCenter - p2_spawn)) (vecY (mapCenter - p2_spawn))

      let p1_state = initialPlayerState p1_spawn pid myTank p1_angle
      let p2_state = initialPlayerState p2_spawn AISystem.botPlayerId botTank p2_angle

      -- Dùng fake address cho state (giống PvP)
      let fakeAddr1 = SockAddrInet 1 (tupleToHostAddress (127,0,0,1))
      let fakeAddr2 = SockAddrInet 2 (tupleToHostAddress (127,0,0,2))

      let playerStates = Map.fromList [(fakeAddr1, p1_state), (fakeAddr2, p2_state)]

      let newRoomGame = initialRoomGameState gameMap spawns PvE
      let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }

      roomGameMVar <- newMVar newRoomGame'

      -- Cập nhật ServerState
      let finalRoom = Room newRoomId (Map.singleton pid client) (Just roomGameMVar) Set.empty
      let finalRooms = Map.insert newRoomId finalRoom (ssRooms sState)
      let udpSock = ssUdpSocket sState

      -- Tạo actions. Hành động forkIO là an toàn, nó không khóa R-lock ngay lập tức
      let gameLoopAction = forkIO $ gameLoop udpSock newRoomId roomGameMVar serverStateRef
      let clientAction = sendTcpPacket h (STP_GameStarting PvE)
      let allActions = [void gameLoopAction, clientAction]

      pure (sState { ssRooms = finalRooms }, (Just pid, allActions))

    -- Xử lý cập nhật trạng thái sẵn sàng và chọn tank trong lobby
    (CTP_UpdateLobbyState mTank isReady, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId) of
        (Just room, Just roomId) -> do
          let client = roomPlayers room Map.! pid
          let newPlayerInfo = (pcInfo client) { piSelectedTank = mTank, piIsReady = isReady }
          let newClient = client { pcInfo = newPlayerInfo }
          let newPlayers = Map.insert pid newClient (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers }
          let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
          
          let (gameCanStart, pInfos_unsorted) = checkGameStart updatedRoom
          -- Sắp xếp pInfos để đảm bảo player 1 luôn là P1
          let pInfos = case pInfos_unsorted of
                         (p:ps) -> p : ps -- Giữ nguyên thứ tự (ví dụ host là P1)
                         [] -> []

          -- Nếu đủ điều kiện bắt đầu game, khởi tạo RoomGameState và forkIO gameLoop
          if gameCanStart && length pInfos == 2
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is starting game!"
              let p1_info = head pInfos
              let p2_info = pInfos !! 1
              
              let spawns = ssSpawns sState

              let gameMap = ssMap sState
              let GameMap { gmapWidth = gw, gmapHeight = gh } = gameMap
              let mapCenter = Vec2 (fromIntegral gw * serverTileSize / 2.0)
                                   (fromIntegral gh * serverTileSize / 2.0)

              let p1_spawn = if not (null spawns) then spawns !! 0 else Vec2 100 100
              let p2_spawn = if length spawns > 1 then spawns !! 1 else Vec2 700 500

              case (piSelectedTank p1_info, piSelectedTank p2_info) of
                (Just tank1, Just tank2) -> do

                  -- TÍNH TOÁN GÓC
                  let p1_angle = atan2 (vecX (mapCenter - p1_spawn)) (vecY (mapCenter - p1_spawn))
                  let p2_angle = atan2 (vecX (mapCenter - p2_spawn)) (vecY (mapCenter - p2_spawn))

                  -- TRUYỀN GÓC VÀO
                  let p1_state = initialPlayerState p1_spawn (piId p1_info) tank1 p1_angle
                  let p2_state = initialPlayerState p2_spawn (piId p2_info) tank2 p2_angle
                  
                  let fakeAddr1 = SockAddrInet 1 (tupleToHostAddress (127,0,0,1))
                  let fakeAddr2 = SockAddrInet 2 (tupleToHostAddress (127,0,0,2))
                  
                  let playerStates = Map.fromList [(fakeAddr1, p1_state), (fakeAddr2, p2_state)]
                  
                  let newRoomGame = initialRoomGameState gameMap spawns PvP
                  let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }
                  
                  roomGameMVar <- newMVar newRoomGame'
                  
                  let finalRoom = updatedRoom { roomGame = Just roomGameMVar }
                  let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
                  
                  let udpSock = ssUdpSocket sState

                  -- Hành động forkIO là an toàn, nó không khóa R-lock ngay lập tức
                  let gameLoopAction = forkIO $ gameLoop udpSock roomId roomGameMVar serverStateRef
                  let broadcastActions = broadcastToRoom finalRoom (STP_GameStarting PvP)
                  let allActions = void gameLoopAction : broadcastActions
                  
                  pure (sState { ssRooms = finalRooms }, (Just pid, allActions))

                _ -> do
                  putStrLn "[TCP] Error: Game start check-pass but tank type is Nothing."
                  let actions = broadcastRoomUpdate updatedRoom
                  pure (sState { ssRooms = newRooms }, (Just pid, actions))
            else
              -- Chỉ broadcast
              let actions = broadcastRoomUpdate updatedRoom
              in pure (sState { ssRooms = newRooms }, (Just pid, actions))
        _ -> pure (sState, (Just pid, [])) -- Không tìm thấy phòng

    -- Xử lý tạm dừng trò chơi
    (CTP_PauseGame isPaused, Just pid) -> do
      -- Chỉ lấy mRoomGame (con trỏ MVar) bên trong S-lock
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      let mGameMVar = mRoom >>= roomGame

      -- Tạo action (chạy bên ngoài S-lock)
      let actions = case (mRoomId, mGameMVar) of
            (Just roomId, Just gameMVar) ->
              let pauseAction = modifyMVar_ gameMVar $ \rgs -> do
                    if (rgsMode rgs == PvE)
                      then do
                        putStrLn $ "[GameLoop " ++ roomId ++ "] Setting Paused: " ++ show isPaused
                        pure $ rgs { rgsIsPaused = isPaused }
                      else 
                        pure rgs -- Không cho phép pause PvP
              in [pauseAction]
            _ -> [] -- Không tìm thấy phòng hoặc game
      
      -- Trả về state và action
      pure (sState, (Just pid, actions))

    -- Xử lý rời phòng
    (CTP_LeaveRoom, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      let actionToClient = sendTcpPacket h STP_ShowMenu -- Gửi client về menu

      let mGameMVar = mRoom >>= roomGame
      
      -- Kiểm tra xem game đã kết thúc chưa bằng cách dùng 'tryReadMVar' để tránh bị block nếu GameLoop đang giữ MVar
      isGameOver <- case mGameMVar of
            Nothing -> pure False            -- Không có game, không thể "game over"
            Just gameMVar -> do
              mRgs <- tryReadMVar gameMVar   -- Đọc không block
              case mRgs of
                Nothing -> pure False        -- GameLoop đang chạy, giả sử game chưa 'over'
                Just rgs -> 
                  case rgsMatchState rgs of
                    GameOver _ -> pure True  -- Game đã kết thúc
                    _          -> pure False -- Game đang chạy hoặc chờ
      
      -- Tạo action unpause (chỉ khi game chưa 'over')
      let unpauseAction = case (mGameMVar, isGameOver) of
            (Just gameMVar, False) ->
              [modifyMVar_ gameMVar (\rgs -> pure rgs { rgsIsPaused = False })]
            _ -> [] -- Không unpause nếu game đã 'over' hoặc không có game

      -- Cập nhật S-lock (state của server)
      case (mRoom, mRoomId) of
        (Just room, Just roomId) -> do
          let newPlayers = Map.delete pid (roomPlayers room)

          -- Nếu game đã kết thúc, reset roomGame về Nothing
          let roomToSave = if isGameOver
                             then room { roomPlayers = newPlayers, roomRematchRequests = Set.empty, roomGame = Nothing }
                             else room { roomPlayers = newPlayers, roomRematchRequests = Set.empty }

          if Map.null newPlayers
            then do
              -- Phòng rỗng, xóa phòng
              putStrLn $ "[TCP] Room " ++ roomId ++ " is empty. Deleting."
              let newState = sState { ssRooms = Map.delete roomId (ssRooms sState) }
              pure (newState, (Just pid, unpauseAction ++ [actionToClient]))
            else do
              -- Phòng còn người, cập nhật phòng
              let newRooms = Map.insert roomId roomToSave (ssRooms sState)
              let newState = sState { ssRooms = newRooms }
              let roomUpdateActions = broadcastRoomUpdate roomToSave
              let rematchClearActions = broadcastToRoom roomToSave (STP_RematchUpdate [])
              let actions = [actionToClient] ++ roomUpdateActions ++ unpauseAction ++ rematchClearActions
              pure (newState, (Just pid, actions))
        _ -> pure (sState, (Just pid, [actionToClient])) -- Không ở trong phòng

    -- Xử lý yêu cầu tái đấu
    (CTP_RequestRematch, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      let mGameMVar = mRoom >>= roomGame

      case (mRoom, mRoomId, mGameMVar) of
        (Just room, Just roomId, Just gameMVar) -> do
          let newRequests = Set.insert pid (roomRematchRequests room)
          let updatedRoom = room { roomRematchRequests = newRequests }
          
          let allPlayers = Map.keysSet (roomPlayers room)
          let allPlayersWantRematch = allPlayers == newRequests
          
          if allPlayersWantRematch
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is starting a rematch!"
              
              -- Lấy GameMode hiện tại từ MVar (PvP hoặc PvE)
              rgs_peek <- readMVar gameMVar
              let currentGameMode = rgsMode rgs_peek

              -- Reset trạng thái trò chơi bên trong MVar
              let resetGameAction = modifyMVar_ gameMVar $ \rgs -> do
                    let spawns = rgsSpawns rgs
                    -- Lấy danh sách (SockAddr, PlayerState)
                    let playerAssocs = Map.assocs (rgsPlayers rgs)
                    
                    -- Hàm reset, nhận (addr, player) và vị trí spawn mới
                    let resetAndAssign (addr, p) spawnPos = 
                          (addr, p { psHealth = 100
                                   , psLives = 3
                                   , psPosition = spawnPos
                                   , psLastFireTime = 0.0 
                                   })
                    
                    -- Ghép người chơi với vị trí spawn (p1 -> spawn1, p2 -> spawn2). Đảm bảo an toàn nếu không có spawn (thêm fallback)
                    let safeSpawns = if (length spawns) < (length playerAssocs)
                                       then cycle (spawns ++ [Vec2 100 100])
                                       else spawns
                                       
                    let newPlayerAssocs = zipWith resetAndAssign playerAssocs safeSpawns
                    
                    -- Tạo lại Map
                    let newPlayersMap = Map.fromList newPlayerAssocs

                    let newGameState = (initialRoomGameState (rgsMap rgs) (rgsSpawns rgs) (rgsMode rgs))
                          { rgsPlayers = newPlayersMap
                          , rgsMatchState = InProgress
                          }
                    pure newGameState

              -- Cập nhật S-lock
              let finalRoom = updatedRoom { roomRematchRequests = Set.empty }
              let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
              
              -- Gửi đúng GameMode đã lấy
              let broadcastActions = broadcastToRoom finalRoom (STP_GameStarting currentGameMode)
              
              pure (sState { ssRooms = finalRooms }, (Just pid, resetGameAction : broadcastActions))
              
            else do
              putStrLn $ "[TCP] Player " ++ show pid ++ " requested rematch. Waiting for others."
              let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
              let broadcastActions = broadcastToRoom updatedRoom (STP_RematchUpdate (Set.toList newRequests))
              pure (sState { ssRooms = newRooms }, (Just pid, broadcastActions))

        _ -> do
          putStrLn $ "[TCP] Invalid Rematch request from " ++ show pid ++ ". No game found."
          pure (sState, (Just pid, [])) -- Bỏ qua


    -- ================================================================
    -- TRẠNG THÁI KHÔNG HỢP LỆ
    -- ================================================================
    -- Đã đăng nhập nhưng gửi gói tin đăng nhập
    (CTP_Login _ _, Just pid) -> do
      putStrLn $ "[TCP] Warning: Client " ++ show pid ++ " sent Login packet while already logged in."
      pure (sState, (Just pid, []))
    -- Đã đăng nhập nhưng gửi gói tin đăng ký
    (CTP_Register _ _, Just pid) -> do
      putStrLn $ "[TCP] Warning: Client " ++ show pid ++ " sent Register packet while already logged in."
      pure (sState, (Just pid, []))


-- HÀM TIỆN ÍCH
-- Tìm phòng của người chơi (PURE)
findRoomByPlayerId :: Int -> ServerState -> (Maybe Room, Maybe String)
findRoomByPlayerId pid sState =
  let found = find (\(_, room) -> Map.member pid (roomPlayers room)) (Map.assocs (ssRooms sState))
  in case found of
    Just (roomId, room) -> (Just room, Just roomId)
    Nothing -> (Nothing, Nothing)

-- Gửi cập nhật phòng đến tất cả người chơi trong phòng (PURE)
broadcastRoomUpdate :: Room -> [IO ()]
broadcastRoomUpdate room =
  let playerInfos = map pcInfo (Map.elems (roomPlayers room))
      packet = STP_RoomUpdate (roomMsgId room) playerInfos
  in broadcastToRoom room packet

-- Gửi gói tin đến tất cả người chơi trong phòng (PURE)
broadcastToRoom :: Room -> ServerTcpPacket -> [IO ()]
broadcastToRoom room packet =
  map (\client -> sendTcpPacket (pcHandle client) packet) (Map.elems (roomPlayers room))

-- Kiểm tra xem game có thể bắt đầu (PURE)
checkGameStart :: Room -> (Bool, [PlayerInfo])
checkGameStart room =
  let infos = map pcInfo (Map.elems (roomPlayers room))
      allReady = all piIsReady infos
      allSelected = all (isJust . piSelectedTank) infos
      playerCount = length infos
  in (playerCount == 2 && allReady && allSelected, infos)