{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use fewer imports" #-}

module Network.TCPServer (startTcpServer) where

import Control.Monad (forever, when, void)
import Control.Concurrent (forkIO, MVar, modifyMVar, readMVar, newMVar, modifyMVar_)
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
import Database.SQLite.Simple (Connection) -- DB
import Data.Queries.PlayerQuery (registerUser, authenticateUser) -- DB
import qualified Data.Text as T 

import Core.Types
import Types.Tank 
import qualified Types.Tank as Tank
import Network.Packet
import GameLoop (gameLoop) 
import Types.MatchState (MatchState(..)) 
import Types.Common (Vec2(..)) 
import Core.Config (AppConfig(..))
import qualified Utils.Random as Rnd (getRandomNumber)
import qualified Data.ByteString as BS 
import qualified Data.ByteString as BS (hPut)
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Types.GameMode (GameMode(..))
import Types.Player (PlayerState(..))

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
  -- Đảm bảo Room ID có 4 chữ số
  pure $ take 4 (show (abs num `mod` 10000 + 10000))


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
      -- forkIO cho mỗi client mới
      _ <- forkIO $ handleClient conn clientAddr serverStateRef
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
  setSocketOption sock NoDelay 1 -- Tắt Nagle's algorithm
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering    -- Rất quan trọng
  
  -- Vòng lặp đọc-xử lý, bắt exception (như disconnect)
  loop h Nothing LBS.empty `catch` \(e :: SomeException) -> do
    putStrLn $ "[TCP] Terminating client loop for " ++ show addr ++ ". Reason: " ++ show e
    -- Đảm bảo ngắt kết nối an toàn nếu vòng lặp bị crash
    handleDisconnect h Nothing serverStateRef

  where
    -- Vòng lặp chính đọc dữ liệu từ Handle
    loop :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    loop h mPlayerId buffer = do
      strictChunk <- (BS.hGetSome h 8192) `catch` \(e :: SomeException) -> do
        putStrLn $ "[TCP] hGetSome Error (" ++ show addr ++ "): " ++ show e
        pure BS.empty -- Coi như client ngắt kết nối

      if BS.null strictChunk && LBS.null buffer
      then handleDisconnect h mPlayerId serverStateRef -- Client đóng kết nối
      else do
        let fullBuffer = buffer <> fromStrict strictChunk
        processBuffer h mPlayerId fullBuffer

    -- Xử lý buffer một cách đệ quy
    processBuffer :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    processBuffer h mPlayerId buffer = do
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ClientTcpPacket)
      
      case decodeResult of
        -- 1. Không đủ dữ liệu -> Quay lại vòng lặp 'loop' để đọc thêm
        Left (_, _, _) -> do
          loop h mPlayerId buffer
            
        -- 2. Decode thành công -> Xử lý gói tin và buffer còn lại
        Right (remainingBuffer, _, pkt) -> do
          
          -- KIỂM TRA: Nếu client chưa login (mPlayerId == Nothing)
          -- thì packet *phải* là Login hoặc Register
          when (isNothing mPlayerId && not (isLoginOrRegisterPkt pkt)) $
            fail $ "Received packet before login from " ++ show addr ++ ": " ++ show pkt

          (new_mPlayerId, actions) <- modifyMVar serverStateRef $ \sState -> do
            
            putStrLn $ "[TCP] Received from " ++ show (fromMaybe 0 mPlayerId) ++ ": " ++ show pkt
            
            -- 2b. Xử lý Packet, trả về State mới và (Maybe PlayerID, [IO Action])
            (sState_final, pid_action) <- processPacket (ssDbConn sState) mPlayerId h pkt sState serverStateRef
            
            -- 2c. Trả về state cuối cùng và các hành động
            pure (sState_final, pid_action)
          
          -- 3. Thực thi các hành động IO (bên ngoài MVar)
          sequence_ actions
          
          -- 4. Đệ quy: Xử lý phần buffer còn dư
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

-- | Hàm PURE xử lý gói tin, trả về State mới và [IO Action]
-- | Hàm này CHẠY BÊN TRONG modifyMVar
-- | Trả về (State Mới, (Maybe PID Mới, [Hành động IO]))
processPacket :: Connection -> Maybe Int -> Handle -> ClientTcpPacket -> ServerState -> MVar ServerState -> IO (ServerState, (Maybe Int, [IO ()]))
processPacket dbConn mPid h pkt sState serverStateRef =
  case (pkt, mPid) of
    
    -- ================================================================
    -- TRẠNG THÁI: CHƯA ĐĂNG NHẬP (mPid is Nothing)
    -- ================================================================

    -- === XỬ LÝ ĐĂNG KÝ ===
    (CTP_Register user pass, Nothing) -> do
      eResult <- try (registerUser dbConn user pass) :: IO (Either SomeException (Either String Int))
      case eResult of
        Left dbErr -> do
          putStrLn $ "[DB Error] Register: " ++ show dbErr
          let action = sendTcpPacket h (STP_LoginResult False 0 "Registration failed (DB Error)")
          pure (sState, (Nothing, [action]))
        Right (Left errMsg) -> do
          putStrLn $ "[Auth] Register failed: " ++ errMsg
          let action = sendTcpPacket h (STP_LoginResult False 0 errMsg)
          pure (sState, (Nothing, [action]))
        Right (Right newId) -> do
          putStrLn $ "[Auth] Register success for " ++ user ++ " (ID: " ++ show newId ++ ")"
          let action = sendTcpPacket h (STP_LoginResult False 0 "Registration successful. Please log in.")
          pure (sState, (Nothing, [action]))

    -- === XỬ LÝ ĐĂNG NHẬP ===
    (CTP_Login user pass, Nothing) -> do
      eResult <- try (authenticateUser dbConn user pass) :: IO (Either SomeException (Maybe (Int, T.Text)))
      case eResult of
        Left dbErr -> do
          putStrLn $ "[DB Error] Login: " ++ show dbErr
          let action = sendTcpPacket h (STP_LoginResult False 0 "Login failed (DB Error)")
          pure (sState, (Nothing, [action]))
        Right Nothing -> do
          putStrLn $ "[Auth] Login failed for " ++ user
          let action = sendTcpPacket h (STP_LoginResult False 0 "Invalid username or password")
          pure (sState, (Nothing, [action]))
        Right (Just (pid, username)) -> do
          putStrLn $ "[Auth] Login success for " ++ user ++ " (ID: " ++ show pid ++ ")"
          
          -- Tạo PlayerClient và thêm vào ServerState
          let newPlayerInfo = PlayerInfo pid (T.unpack username) Nothing False
          let newClient = PlayerClient h newPlayerInfo Nothing
          let newClients = Map.insert pid newClient (ssClients sState)
          -- Cập nhật NextPlayerId để tránh trùng lặp nếu server restart nhưng DB thì không
          let newState = sState { ssClients = newClients, ssNextPlayerId = max (ssNextPlayerId sState) (pid + 1) }
          
          let action = sendTcpPacket h (STP_LoginResult True pid "Login successful")
          -- Trả về (Just pid) để cập nhật state của vòng lặp
          pure (newState, (Just pid, [action]))

    -- ================================================================
    -- TRẠNG THÁI: ĐÃ ĐĂNG NHẬP (mPid is Just pid)
    -- ==================================================================

    (CTP_CreateRoom, Just pid) -> do
      newRoomId <- generateRoomId
      let client = ssClients sState Map.! pid
      let newRoom = Room newRoomId (Map.singleton pid client) Nothing Set.empty
      let newRooms = Map.insert newRoomId newRoom (ssRooms sState)
      let action = sendTcpPacket h (STP_RoomUpdate newRoomId [pcInfo client])
      pure (sState { ssRooms = newRooms }, (Just pid, [action]))

    (CTP_JoinRoom roomId, Just pid) -> do
      case Map.lookup roomId (ssRooms sState) of
        Nothing -> do
          let action = sendTcpPacket h (STP_Kicked "Room not found")
          pure (sState, (Just pid, [action]))
        Just room -> do
          let client = ssClients sState Map.! pid
          let newPlayers = Map.insert pid client (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers }
          let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
          let actions = broadcastRoomUpdate updatedRoom -- Lấy list [IO ()]
          pure (sState { ssRooms = newRooms }, (Just pid, actions))
      
    (CTP_StartDungeon mTank, Just pid) -> do
      putStrLn $ "[TCP] Client " ++ show pid ++ " requested Dungeon start, but PvE is disabled."
      let action = sendTcpPacket h (STP_Kicked "PvE mode is disabled on this server")
      pure (sState, (Just pid, [action]))

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
                         (p:ps) -> p : ps -- Giữ nguyên thứ tự (ví dụ: host là P1)
                         [] -> []

          if gameCanStart && length pInfos == 2
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is starting game!"
              let p1_info = head pInfos
              let p2_info = pInfos !! 1
              
              let spawns = ssSpawns sState
              let p1_spawn = if not (null spawns) then spawns !! 0 else Vec2 100 100
              let p2_spawn = if length spawns > 1 then spawns !! 1 else Vec2 700 500

              case (piSelectedTank p1_info, piSelectedTank p2_info) of
                (Just tank1, Just tank2) -> do
                  let p1_state = initialPlayerState p1_spawn (piId p1_info) tank1
                  let p2_state = initialPlayerState p2_spawn (piId p2_info) tank2
                  
                  let fakeAddr1 = SockAddrInet 1 (tupleToHostAddress (127,0,0,1))
                  let fakeAddr2 = SockAddrInet 2 (tupleToHostAddress (127,0,0,2))
                  
                  let playerStates = Map.fromList [(fakeAddr1, p1_state), (fakeAddr2, p2_state)]
                  
                  let newRoomGame = initialRoomGameState (ssMap sState) (ssSpawns sState) PvP
                  let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }
                  
                  roomGameMVar <- newMVar newRoomGame'
                  
                  let finalRoom = updatedRoom { roomGame = Just roomGameMVar }
                  let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
                  
                  let gameLoopAction = forkIO $ gameLoop serverStateRef roomId roomGameMVar
                  let broadcastActions = broadcastToRoom finalRoom (STP_GameStarting PvP)
                  let allActions = void gameLoopAction : broadcastActions
                  
                  pure (sState { ssRooms = finalRooms }, (Just pid, allActions))

                _ -> do
                  putStrLn "[TCP] Lỗi: Game start check-pass nhưng tank type là Nothing."
                  let actions = broadcastRoomUpdate updatedRoom
                  pure (sState { ssRooms = newRooms }, (Just pid, actions))
            else
              -- Chỉ broadcast
              let actions = broadcastRoomUpdate updatedRoom
              in pure (sState { ssRooms = newRooms }, (Just pid, actions))
        _ -> pure (sState, (Just pid, [])) -- Không tìm thấy phòng

    (CTP_PauseGame isPaused, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId) of
        (Just room, Just roomId) ->
          case roomGame room of
            Just gameMVar -> do
              let action = modifyMVar_ gameMVar $ \rgs -> do
                    if (rgsMode rgs == PvE) -- Vẫn giữ logic chỉ cho PvE (mặc dù PvE đang bị tắt)
                      then do
                        putStrLn $ "[GameLoop " ++ roomId ++ "] Setting Paused: " ++ show isPaused
                        pure $ rgs { rgsIsPaused = isPaused }
                      else 
                        pure rgs -- Không cho phép pause PvP
              pure (sState, (Just pid, [action]))
            _ -> pure (sState, (Just pid, [])) -- Game không chạy
        _ -> pure (sState, (Just pid, [])) -- Không tìm thấy phòng

    (CTP_LeaveRoom, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      let actionToClient = sendTcpPacket h STP_ShowMenu -- Gửi client về menu
      case (mRoom, mRoomId) of
        (Just room, Just roomId) -> do
          -- TỰ ĐỘNG UNPAUSE KHI RỜI PHÒNG
          let unpauseAction = case roomGame room of
                Just gameMVar -> [modifyMVar_ gameMVar (\rgs -> pure rgs { rgsIsPaused = False })]
                Nothing -> []

          let newPlayers = Map.delete pid (roomPlayers room)
          let updatedRoom = room { roomPlayers = newPlayers, roomRematchRequests = Set.empty }

          if Map.null newPlayers
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is empty. Deleting."
              let newState = sState { ssRooms = Map.delete roomId (ssRooms sState) }
              pure (newState, (Just pid, unpauseAction ++ [actionToClient])) -- Chạy unpause
            else do
              let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
              let newState = sState { ssRooms = newRooms }
              let roomUpdateActions = broadcastRoomUpdate updatedRoom
              let rematchClearActions = broadcastToRoom updatedRoom (STP_RematchUpdate [])
              let actions = [actionToClient] ++ roomUpdateActions ++ unpauseAction ++ rematchClearActions
              pure (newState, (Just pid, actions))
        _ -> pure (sState, (Just pid, [actionToClient])) -- Không ở trong phòng

    (CTP_RequestRematch, Just pid) -> do
      let (mRoom, mRoomId) = findRoomByPlayerId pid sState
      case (mRoom, mRoomId, mRoom >>= roomGame) of
        (Just room, Just roomId, Just gameMVar) -> do
          let newRequests = Set.insert pid (roomRematchRequests room)
          let updatedRoom = room { roomRematchRequests = newRequests }
          
          let allPlayers = Map.keysSet (roomPlayers room)
          let allPlayersWantRematch = allPlayers == newRequests
          
          if allPlayersWantRematch
            then do
              putStrLn $ "[TCP] Room " ++ roomId ++ " is starting a rematch!"
              
              let resetGameAction = modifyMVar_ gameMVar $ \rgs -> do
                    let getSpawnPos pId spawns = if null spawns
                                                  then Vec2 100 100
                                                  else spawns !! (pId `mod` length spawns)
                    
                    let resetPlayerState p = p { psHealth = 100, psLives = 3, psPosition = getSpawnPos (psId p) (rgsSpawns rgs), psLastFireTime = 0.0 }
                    let newPlayerStates = Map.map resetPlayerState (rgsPlayers rgs)

                    let newGameState = (initialRoomGameState (rgsMap rgs) (rgsSpawns rgs) (rgsMode rgs))
                          { rgsPlayers = newPlayerStates
                          , rgsMatchState = InProgress
                          }
                    pure newGameState

              let finalRoom = updatedRoom { roomRematchRequests = Set.empty }
              let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
              
              let broadcastActions = broadcastToRoom finalRoom (STP_GameStarting PvP)
              
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
    (CTP_Register _ _, Just pid) -> do
      putStrLn $ "[TCP] Warning: Client " ++ show pid ++ " sent Register packet while already logged in."
      pure (sState, (Just pid, []))

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