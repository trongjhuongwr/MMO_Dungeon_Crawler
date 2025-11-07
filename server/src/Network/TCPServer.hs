{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Network.TCPServer (startTcpServer) where

import Control.Monad (forever, when)
import Control.Concurrent (forkIO, MVar, modifyMVar, readMVar, newMVar, modifyMVar_)
import Network.Socket
import System.IO
import qualified Data.Map as Map
import Data.Binary (encode, decode, decodeOrFail)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int64)
import Data.Maybe (isJust, fromJust)
import Data.List (find)
import Control.Exception (catch, SomeException)

import Core.Types
import Types.Tank (TankType)
import Network.Packet
import GameLoop (gameLoop) 
import Types.MatchState (MatchState(..)) 
import Types.Common (Vec2(..)) 
import qualified Utils.Random as Rnd (getRandomNumber)

-- Hàm tiện ích để gửi gói tin TCP
sendTcpPacket :: Handle -> ServerTcpPacket -> IO ()
sendTcpPacket h pkt = (LBS.hPut h (encode pkt) >> hFlush h) `catch` \(e :: SomeException) ->
  putStrLn $ "[TCP] Failed to send packet: " ++ show e

-- Hàm tiện ích để tạo Room ID
generateRoomId :: IO String
generateRoomId = do
  num <- Rnd.getRandomNumber
  pure $ take 4 (show (abs num) ++ "0000")

tcpPort :: PortNumber
tcpPort = 4000 

startTcpServer :: MVar ServerState -> IO ()
startTcpServer serverStateRef = withSocketsDo $ do
  addr <- resolve (show tcpPort)
  sock <- open addr
  putStrLn $ "[TCP Server] Listening on port " ++ show tcpPort
  forever $ do
    (conn, clientAddr) <- accept sock
    putStrLn $ "[TCP Server] Client connected from: " ++ show clientAddr
    _ <- forkIO $ handleClient conn clientAddr serverStateRef
    return ()
  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

handleClient :: Socket -> SockAddr -> MVar ServerState -> IO ()
handleClient sock addr serverStateRef = do
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering -- Giữ nguyên NoBuffering
  
  -- Bắt đầu vòng lặp với buffer rỗng
  loop h Nothing LBS.empty `catch` \(e :: SomeException) -> do
    putStrLn $ "[TCP] Error in client loop: " ++ show e
    handleDisconnect h Nothing -- Ngắt kết nối an toàn

  where
    -- SỬA: Thêm 'buffer' vào chữ ký hàm
    loop :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    loop h mPlayerId buffer = do
      -- 1. Đọc thêm dữ liệu
      newChunk <- (LBS.hGet h 8192) `catch` \(e :: SomeException) -> do
        -- putStrLn $ "[TCP] hGet Error: " ++ show e
        pure LBS.empty -- Coi như client ngắt kết nối
      
      if LBS.null newChunk && LBS.null buffer
      then handleDisconnect h mPlayerId -- Client đã ngắt kết nối
      else do
        -- 2. Nối buffer cũ với dữ liệu mới
        let fullBuffer = buffer <> newChunk
        -- 3. Gọi hàm xử lý buffer
        processBuffer h mPlayerId fullBuffer

    -- HÀM MỚI: Xử lý buffer một cách đệ quy
    processBuffer :: Handle -> Maybe Int -> LBS.ByteString -> IO ()
    processBuffer h mPlayerId buffer = do
      -- 3a. Thử decode buffer
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ClientTcpPacket)
      
      case decodeResult of
        -- 4a. Không đủ dữ liệu
        Left (_, _, errMsg) -> do
          -- Gói tin chưa hoàn chỉnh, quay lại vòng lặp 'loop' để đọc thêm
          -- và giữ nguyên 'buffer' hiện tại
          -- putStrLn $ "[TCP] Incomplete packet: " ++ errMsg ++ ". Buffering " ++ show (LBS.length buffer) ++ " bytes."
          loop h mPlayerId buffer
            
        -- 4b. Decode thành công
        Right (remaining, _, pkt) -> do
          -- Xử lý gói tin thành công
          (pid, isNew) <- case mPlayerId of
                  Just pid -> pure (pid, False)
                  Nothing -> case pkt of
                    CTP_Login _ _ -> modifyMVar serverStateRef $ \sState -> do
                      let newId = ssNextPlayerId sState
                      let newName = "Player" ++ show newId
                      let newPlayerInfo = PlayerInfo newId newName Nothing False
                      let newClient = PlayerClient h newPlayerInfo Nothing
                      let newClients = Map.insert newId newClient (ssClients sState)
                      let newState = sState { ssClients = newClients, ssNextPlayerId = newId + 1 }
                      putStrLn $ "[TCP] Client assigned ID: " ++ show newId
                      pure (newState, (newId, True))
                    _ -> fail "Received packet before login"
          
          putStrLn $ "[TCP] Received from " ++ show pid ++ ": " ++ show pkt
          
          -- Xử lý logic gói tin
          processPacket_safe pid isNew h pkt -- (Hàm processPacket_safe được tạo bên dưới)
          
          -- 5. ĐỆ QUY: Xử lý phần buffer còn dư
          -- Nếu còn dư, có thể có gói tin thứ 2 nối đuôi
          if LBS.null remaining
            then loop h (Just pid) remaining -- Quay lại chờ dữ liệu mới
            else do
              putStrLn $ "[TCP] Processing " ++ show (LBS.length remaining) ++ " remaining bytes in buffer."
              processBuffer h (Just pid) remaining -- Xử lý ngay phần còn lại

    -- TÁCH LOGIC: Hàm này chỉ xử lý gói tin
    processPacket_safe :: Int -> Bool -> Handle -> ClientTcpPacket -> IO ()
    processPacket_safe pid isNew h pkt = do
      modifyMVar_ serverStateRef $ \sState -> do
        case pkt of
          CTP_Login name _ -> do
            let client = ssClients sState Map.! pid
            let newPlayerInfo = (pcInfo client) { piName = name }
            let newClient = client { pcInfo = newPlayerInfo }
            let newClients = Map.insert pid newClient (ssClients sState)
            
            -- SỬA DÒNG NÀY:
            -- Xóa "when isNew" để server LUÔN gửi phản hồi khi nhận được CTP_Login
            sendTcpPacket h (STP_LoginResult True pid "Login successful")
            
            pure sState { ssClients = newClients }

          CTP_CreateRoom -> do
            newRoomId <- generateRoomId
            let client = ssClients sState Map.! pid
            let newRoom = Room newRoomId (Map.singleton pid client) Nothing
            let newRooms = Map.insert newRoomId newRoom (ssRooms sState)
            sendTcpPacket h (STP_RoomUpdate newRoomId [pcInfo client])
            pure sState { ssRooms = newRooms }

          CTP_JoinRoom roomId -> do
            case Map.lookup roomId (ssRooms sState) of
              Nothing -> do
                sendTcpPacket h (STP_Kicked "Room not found")
                pure sState
              Just room -> do
                let client = ssClients sState Map.! pid
                let newPlayers = Map.insert pid client (roomPlayers room)
                let updatedRoom = room { roomPlayers = newPlayers }
                let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
                broadcastRoomUpdate updatedRoom
                pure sState { ssRooms = newRooms }

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
                
                broadcastRoomUpdate updatedRoom
                
                let (gameCanStart, pInfos) = checkGameStart updatedRoom
                if gameCanStart
                  then do
                    putStrLn $ "[TCP] Room " ++ roomId ++ " is starting game!"
                    let p1_info = head pInfos
                    let p2_info = pInfos !! 1
                    
                    let spawns = ssSpawns sState
                    let p1_spawn = if not (null spawns) then spawns !! 0 else Vec2 0 0
                    let p2_spawn = if length spawns > 1 then spawns !! 1 else Vec2 50 50

                    let p1_state = initialPlayerState p1_spawn (piId p1_info) (fromJust $ piSelectedTank p1_info)
                    let p2_state = initialPlayerState p2_spawn (piId p2_info) (fromJust $ piSelectedTank p2_info)
                    
                    let fakeAddr1 = SockAddrInet 0 (tupleToHostAddress (0,0,0,1))
                    let fakeAddr2 = SockAddrInet 0 (tupleToHostAddress (0,0,0,2))
                    
                    let playerStates = Map.fromList [(fakeAddr1, p1_state), (fakeAddr2, p2_state)]
                    
                    let newRoomGame = initialRoomGameState (ssMap sState) (ssSpawns sState)
                    let newRoomGame' = newRoomGame { rgsPlayers = playerStates, rgsMatchState = InProgress }
                    
                    roomGameMVar <- newMVar newRoomGame'
                    
                    let finalRoom = updatedRoom { roomGame = Just roomGameMVar }
                    let finalRooms = Map.insert roomId finalRoom (ssRooms sState)
                    
                    _ <- forkIO $ gameLoop serverStateRef roomId roomGameMVar
                    
                    broadcastToRoom finalRoom (STP_GameStarting)
                    pure sState { ssRooms = finalRooms }
                  else
                    pure sState { ssRooms = newRooms } 
              _ -> pure sState 
          
          CTP_LeaveRoom -> do
            let (mRoom, mRoomId) = findRoomByPlayerId pid sState
            case (mRoom, mRoomId) of
              (Just room, Just roomId) -> do
                let newPlayers = Map.delete pid (roomPlayers room)
                let updatedRoom = room { roomPlayers = newPlayers }
                broadcastRoomUpdate updatedRoom
                pure sState { ssRooms = Map.insert roomId updatedRoom (ssRooms sState) }
              _ -> pure sState 
          
          CTP_RequestRematch -> do
            pure sState
            
    -- Xóa hàm 'processPacket' cũ bị trùng lặp
    
    handleDisconnect :: Handle -> Maybe Int -> IO ()
    handleDisconnect h mPid = do
      putStrLn $ "[TCP] Client disconnected: " ++ show mPid
      hClose h
      case mPid of
        Nothing -> pure () -- Chưa login, không cần làm gì
        Just pid -> do
          modifyMVar_ serverStateRef $ \sState -> do
            let newClients = Map.delete pid (ssClients sState)
            let (mRoom, mRoomId) = findRoomByPlayerId pid sState
            case (mRoom, mRoomId) of
              (Just room, Just roomId) -> do
                let newPlayers = Map.delete pid (roomPlayers room)
                if Map.null newPlayers
                then do
                  putStrLn $ "[TCP] Room " ++ roomId ++ " is empty. Deleting."
                  pure sState { ssClients = newClients, ssRooms = Map.delete roomId (ssRooms sState) }
                else do
                  let updatedRoom = room { roomPlayers = newPlayers }
                  let newRooms = Map.insert roomId updatedRoom (ssRooms sState)
                  broadcastRoomUpdate updatedRoom
                  pure sState { ssClients = newClients, ssRooms = newRooms }
              _ -> pure sState { ssClients = newClients } 

    -- === HÀM TIỆN ÍCH ===
    findPidByHandle :: Handle -> ServerState -> Maybe Int
    findPidByHandle h sState =
      fmap fst (find (\(_, client) -> pcHandle client == h) (Map.assocs (ssClients sState)))

    findRoomByPlayerId :: Int -> ServerState -> (Maybe Room, Maybe String)
    findRoomByPlayerId pid sState =
      let found = find (\(_, room) -> Map.member pid (roomPlayers room)) (Map.assocs (ssRooms sState))
      in case found of
        Just (roomId, room) -> (Just room, Just roomId)
        Nothing -> (Nothing, Nothing)

    broadcastRoomUpdate :: Room -> IO ()
    broadcastRoomUpdate room = do
      let playerInfos = map pcInfo (Map.elems (roomPlayers room))
      let packet = STP_RoomUpdate (roomMsgId room) playerInfos
      broadcastToRoom room packet

    broadcastToRoom :: Room -> ServerTcpPacket -> IO ()
    broadcastToRoom room packet =
      mapM_ (\client -> sendTcpPacket (pcHandle client) packet) (Map.elems (roomPlayers room))

    checkGameStart :: Room -> (Bool, [PlayerInfo])
    checkGameStart room =
      let infos = map pcInfo (Map.elems (roomPlayers room))
          allReady = all piIsReady infos
          allSelected = all (isJust . piSelectedTank) infos
          playerCount = length infos
      in (playerCount == 2 && allReady && allSelected, infos)

    fromJust :: Maybe a -> a
    fromJust (Just x) = x