{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where

import Network.Socket hiding (recv, SendTo, RecvFrom)
import System.IO
import Control.Exception (bracket, try, SomeException, catch)
import Data.Binary (encode, decode, decodeOrFail)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, modifyMVar_, modifyMVar)
import Control.Monad (forever, when)
import Graphics.Gloss.Interface.IO.Game
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Renderer.Resources as R
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Network.Packet
import Types.Player
import Types.Common
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..))
import Input (KeyMap, calculateMoveVector)
import Core.Renderer (render) -- Render game cũ
import Core.Effect (Effect(..), makeExplosion, updateEffect, isEffectFinished)
import Core.Animation (Animation(..), updateAnimation, startAnimation)
import Systems.MapLoader (loadMapFromFile) 
import Renderer.Resources (Resources(..))
import Types.MatchState (MatchState(..)) 
import UI.Screens
import Types.Tank (TankType(..))
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.List (find)
import Data.Int (Int64)

-- ===================================================================
-- KIỂU DỮ LIỆU STATE MÁY
-- ===================================================================

-- Trạng thái khi đang trong game (ClientState cũ)
data InGameState = InGameState
  { igsKeys              :: KeyMap
  , igsMousePos          :: (Float, Float)
  , igsWorld             :: WorldSnapshot
  , igsGameMap           :: GameMap 
  , igsDidFire           :: Bool
  , igsEffects           :: [Effect]
  , igsNextEffectId      :: Int
  , igsTurretAnimRapid   :: Animation 
  , igsTurretAnimBlast   :: Animation 
  , igsMyId              :: Int
  , igsMatchState        :: MatchState 
  }

-- Dữ liệu cho màn hình Login
data LoginData = LoginData { ldUsername :: String, ldStatus :: String }

-- Dữ liệu cho sảnh chờ
data LobbyData = LobbyData 
  { ldRoomId :: String
  , ldPlayers :: [PlayerInfo] -- Lấy từ server (chứa lựa chọn của đối thủ)
  , ldMyTank :: Maybe TankType -- Lựa chọn của bản thân
  , ldMyReady :: Bool         -- Trạng thái sẵn sàng của bản thân
  }

-- Dữ liệu màn hình kết thúc
data PostGameData = PostGameData { pgStatus :: String }

-- Trạng thái của toàn bộ ứng dụng
data AppState
  = S_Login    LoginData    -- Màn hình đăng nhập
  | S_Menu                  -- Menu chính (Nút Start)
  | S_RoomSelection String  -- (String là RoomID đang nhập)
  | S_Lobby    LobbyData    -- Sảnh chờ (Tạo/Vào phòng)
  | S_InGame   InGameState  -- Trạng thái game (ClientState cũ)
  | S_PostGame PostGameData -- Màn hình kết thúc (Chơi lại/Thoát)

-- TRẠNG THÁI CLIENT TOÀN CỤC
data ClientState = ClientState
  { csTcpHandle  :: Handle     -- Kết nối TCP vĩnh viễn
  , csUdpSocket  :: Socket     -- Socket UDP
  , csServerAddr :: SockAddr   -- Địa chỉ UDP của server
  , csMyId       :: Int        -- ID của mình (lấy sau khi login)
  , csState      :: AppState
  , csResources  :: Resources
  }

-- ===================================================================
-- HÀM MAIN VÀ KHỞI TẠO
-- ===================================================================

main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  
  putStrLn "Starting client..."
  eResources <- R.loadResources --
  
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do 
      putStrLn "Assets loaded."
      
      -- Kết nối TCP và UDP
      eConn <- try $ connectTcp "127.0.0.1" 4000
      
      case eConn of
        Left (e :: SomeException) -> putStrLn $ "Cannot connect to server: " ++ show e
        Right (h, sockUDP, serverAddrUDP) -> do
          putStrLn "Connected to TCP/UDP."
          
          let initialState = ClientState
                { csTcpHandle = h
                , csUdpSocket = sockUDP
                , csServerAddr = serverAddrUDP
                , csMyId = 0 -- Sẽ được cập nhật
                , csState = S_Login (LoginData "Player" "Please login")
                , csResources = assets
                }
          
          clientStateRef <- newMVar initialState
          
          -- Luồng lắng nghe TCP (Quản lý State)
          _ <- forkIO $ tcpListenLoop h clientStateRef
          -- Luồng lắng nghe UDP (In-Game Data)
          _ <- forkIO $ udpListenLoop sockUDP clientStateRef
          
          -- Khởi chạy cửa sổ game
          playIO
            (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
            black 60
            clientStateRef
            renderIO
            handleInputIO
            updateClientIO

connectTcp :: HostName -> PortNumber -> IO (Handle, Socket, SockAddr)
connectTcp host port = do
  -- TCP
  addrTCP <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just host) (Just $ show port)
  sockTCP <- socket (addrFamily addrTCP) (addrSocketType addrTCP) (addrProtocol addrTCP)
  connect sockTCP (addrAddress addrTCP)
  h <- socketToHandle sockTCP ReadWriteMode
  hSetBuffering h NoBuffering -- Giữ nguyên NoBuffering
  
  -- UDP
  sockUDP <- socket AF_INET Datagram defaultProtocol
  bind sockUDP (SockAddrInet 0 0) -- Ràng buộc vào một cổng ngẫu nhiên
  serverAddrUDP <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just host) (Just "8888")
  
  return (h, sockUDP, addrAddress serverAddrUDP)

-- Hàm tiện ích gửi gói TCP
sendTcpPacket :: Handle -> ClientTcpPacket -> IO ()
sendTcpPacket h pkt = LBS.hPut h (encode pkt) >> hFlush h

-- Hàm tiện ích gửi gói UDP
sendUdpPacket :: Socket -> SockAddr -> ClientUdpPacket -> IO ()
sendUdpPacket sock addr pkt = do
  let lazyMsg = encode pkt
  _ <- BS.sendTo sock (toStrict lazyMsg) addr
  return ()

-- ===================================================================
-- VÒNG LẶP NETWORK (LẮNG NGHE)
-- ===================================================================

-- Lắng nghe gói TCP từ Server (Quản lý trạng thái)
tcpListenLoop :: Handle -> MVar ClientState -> IO ()
tcpListenLoop h mvar = loop LBS.empty -- SỬA: Bắt đầu với buffer rỗng
  where
    -- SỬA: Thêm 'buffer' vào chữ ký hàm
    loop :: LBS.ByteString -> IO ()
    loop buffer = do
      -- 1. Đọc thêm dữ liệu
      newChunk <- (LBS.hGet h 8192) `catch` \(e :: SomeException) -> do
        -- putStrLn $ "[TCP] hGet Error: " ++ show e
        pure LBS.empty -- Coi như server ngắt kết nối
      
      if LBS.null newChunk && LBS.null buffer
      then putStrLn "[TCP] Server disconnected." >> fail "Server disconnected"
      else do
        -- 2. Nối buffer cũ với dữ liệu mới
        let fullBuffer = buffer <> newChunk
        -- 3. Gọi hàm xử lý buffer
        processBuffer fullBuffer

    -- HÀM MỚI: Xử lý buffer một cách đệ quy
    processBuffer :: LBS.ByteString -> IO ()
    processBuffer buffer = do
      -- 3a. Thử decode buffer
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ServerTcpPacket)
      
      case decodeResult of
        -- 4a. Không đủ dữ liệu
        Left (_, _, errMsg) -> do
          -- Gói tin chưa hoàn chỉnh, quay lại vòng lặp 'loop' để đọc thêm
          -- và giữ nguyên 'buffer' hiện tại
          -- putStrLn $ "[TCP] Incomplete packet: " ++ errMsg ++ ". Buffering " ++ show (LBS.length buffer) ++ " bytes."
          loop buffer
            
        -- 4b. Decode thành công
        Right (remaining, _, pkt) -> do
          -- Xử lý gói tin thành công
          putStrLn $ "[TCP] Received: " ++ show pkt
          modifyMVar_ mvar $ \cState -> do
            case pkt of
              STP_LoginResult success pid msg ->
                if success
                then pure cState { csMyId = pid, csState = S_Menu }
                else pure cState { csState = S_Login (LoginData "" msg) }
                
              STP_RoomUpdate roomId pInfos ->
                let myInfo = find (\p -> piId p == csMyId cState) pInfos
                    myTank = myInfo >>= piSelectedTank
                    myReady = maybe False piIsReady myInfo
                    lobbyData = LobbyData roomId pInfos myTank myReady
                in pure cState { csState = S_Lobby lobbyData }

              STP_GameStarting -> do
                eMapData <- loadMapFromFile "client/assets/maps/pvp.json"
                case eMapData of
                  Left err -> putStrLn ("Failed to load map: " ++ err) >> pure cState
                  Right (gmap, _) -> do
                    let (S_Lobby lobbyData) = csState cState
                    let myTank = fromJust $ ldMyTank lobbyData
                    
                    let assets = csResources cState
                    let animR = (dummyAnim assets) { animFrames = resTurretFramesRapid assets }
                    let animB = (dummyAnim assets) { animFrames = resTurretFramesBlast assets }
                    
                    let newInGameState = InGameState
                          { igsKeys = Set.empty
                          , igsMousePos = (0, 0)
                          , igsWorld = initialWorldSnapshot
                          , igsGameMap = gmap
                          , igsDidFire = False
                          , igsEffects = []
                          , igsNextEffectId = 0
                          , igsTurretAnimRapid = animR
                          , igsTurretAnimBlast = animB
                          , igsMyId = csMyId cState
                          , igsMatchState = Waiting
                          }
                    
                    sendUdpPacket (csUdpSocket cState) (csServerAddr cState) (CUP_Handshake (csMyId cState))
                    
                    pure cState { csState = S_InGame newInGameState }
                
              STP_Kicked msg ->
                pure cState { csState = S_Login (LoginData "" msg) }
              
              STP_ShowMenu ->
                pure cState { csState = S_Menu }
          
          -- 5. ĐỆ QUY: Xử lý phần buffer còn dư
          if LBS.null remaining
            then loop remaining -- Quay lại chờ dữ liệu mới
            else do
              putStrLn $ "[TCP] Processing " ++ show (LBS.length remaining) ++ " remaining bytes in buffer."
              processBuffer remaining -- Xử lý ngay phần còn lại

-- Lắng nghe gói UDP từ Server (Trong Game)
udpListenLoop :: Socket -> MVar ClientState -> IO ()
udpListenLoop sock mvar = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  
  mState <- readMVar mvar
  
  -- Chỉ xử lý nếu đang trong game
  case (csState mState) of
    S_InGame gdata -> do
      -- Decode gói UDP
      case decodeOrFail (fromStrict strictMsg) of
        Left _ -> pure () -- Lỗi decode, bỏ qua
        Right (_, _, udpPkt) -> do
          -- Cập nhật InGameState
          newGData <- case (udpPkt :: ServerUdpPacket) of
            SUP_MatchStateUpdate newState -> do
              when (newState /= igsMatchState gdata) $ 
                putStrLn $ "[Game] Match status changed to: " ++ show newState
              pure gdata { igsMatchState = newState }
              
            SUP_Snapshot newSnapshot -> 
              pure $ updateSnapshot (csResources mState) gdata newSnapshot
              
          -- Ghi lại vào MVar
          modifyMVar_ mvar (\cs -> pure cs { csState = S_InGame newGData })
    _ -> pure () -- Không trong game, bỏ qua gói UDP

-- ===================================================================
-- HÀM CHÍNH CỦA GLOSS (ROUTERS)
-- ===================================================================

-- RENDER CHÍNH (Router)
renderIO :: MVar ClientState -> IO Picture
renderIO mvar = do
  cState <- readMVar mvar
  case (csState cState) of
    S_Login (LoginData user status) -> pure $ renderLogin user status
    S_Menu -> pure renderMenu
    S_RoomSelection roomId -> pure $ renderRoomSelection roomId
    S_Lobby (LobbyData rId pInfo myTank myReady) -> pure $ renderLobby rId pInfo (csMyId cState) myTank myReady
    S_InGame gdata -> 
      pure $ render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                    (igsEffects gdata) (igsTurretAnimRapid gdata) 
                    (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                    (igsMatchState gdata)
    S_PostGame (PostGameData status) -> pure $ renderPostGame status

-- INPUT CHÍNH (Router)
handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  modifyMVar_ mvar $ \cState -> do
    case (csState cState) of
      S_Login data_ -> handleInputLogin event cState
      S_Menu        -> handleInputMenu event cState
      S_RoomSelection data_ -> handleInputRoomSelection event cState
      S_Lobby data_   -> handleInputLobby event cState
      S_InGame gdata  -> 
        -- Chỉ xử lý input game nếu trận đấu đang diễn ra
        if (igsMatchState gdata == InProgress)
          then pure $ cState { csState = S_InGame (handleInputGame event gdata) }
          else case (igsMatchState gdata) of
                (GameOver _) -> handleInputPostGame event cState
                _ -> pure cState -- Chờ...
      S_PostGame data_ -> handleInputPostGame event cState
  return mvar

-- UPDATE CHÍNH (Router)
updateClientIO :: Float -> MVar ClientState -> IO (MVar ClientState)
updateClientIO dt mvar = do
  modifyMVar mvar $ \cState -> do
    case (csState cState) of
      S_InGame gdata -> 
        -- Chỉ update game logic nếu đang trong game
        let (gdata', mCommand) = updateGame dt gdata
        in do
          -- Gửi PlayerCommand (UDP) nếu có
          case mCommand of
            Just cmd -> sendUdpPacket (csUdpSocket cState) (csServerAddr cState) (CUP_Command cmd)
            Nothing -> pure ()
          
          -- Kiểm tra nếu game over (từ UDP)
          case (igsMatchState gdata') of
            GameOver mWinnerId ->
              let status = case (Just (igsMyId gdata'), mWinnerId) of
                            (Just myId, Just winnerId) | myId == winnerId -> "YOU WIN!"
                            (Just _, Nothing) -> "DRAW!"
                            _ -> "YOU LOSE!"
              in pure (cState { csState = S_PostGame (PostGameData status) }, mvar)
            _ -> 
              pure (cState { csState = S_InGame gdata' }, mvar)
      
      _ -> pure (cState, mvar) -- Không làm gì ở các màn hình khác
  

-- ===================================================================
-- HÀM XỬ LÝ CHO TỪNG STATE
-- ===================================================================

-- === LOGIN ===
handleInputLogin :: Event -> ClientState -> IO ClientState
handleInputLogin event cState@(ClientState { csTcpHandle = h, csState = (S_Login ld) }) =
  case event of
    (EventKey (Char c) Down _ _) -> -- Gõ phím
      pure cState { csState = S_Login ld { ldUsername = ldUsername ld ++ [c] } }
    (EventKey (SpecialKey KeyBackspace) Down _ _) -> -- Xóa phím
      pure cState { csState = S_Login ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) } }
    (EventKey (MouseButton LeftButton) Down _ (x, y)) ->
      -- Tọa độ nút Login (tâm: -20, -150; size: 200x50)
      if (x > -120 && x < 80 && y > -175 && y < -125) -- Bấm nút Login
      then do
        sendTcpPacket h (CTP_Login (ldUsername ld) "")
        pure cState { csState = S_Login ld { ldStatus = "Logging in..." } }
      else pure cState
    _ -> pure cState
handleInputLogin _ cState = pure cState -- Trạng thái không hợp lệ

-- === MENU ===
handleInputMenu :: Event -> ClientState -> IO ClientState
handleInputMenu event cState@(ClientState { csTcpHandle = h }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y)) ->
      if (x > -20 && x < 100 && y > -200 && y < -150) -- Bấm nút Start PvP
      then pure cState { csState = S_RoomSelection "" }
      else pure cState
    _ -> pure cState

-- === ROOM SELECTION ===
handleInputRoomSelection :: Event -> ClientState -> IO ClientState
handleInputRoomSelection event cState@(ClientState { csTcpHandle = h, csState = (S_RoomSelection roomId) }) =
  case event of
    (EventKey (Char c) Down _ _) -> -- Gõ Room ID
      pure cState { csState = S_RoomSelection (roomId ++ [c]) }
    (EventKey (SpecialKey KeyBackspace) Down _ _) -> -- Xóa phím
      pure cState { csState = S_RoomSelection (if null roomId then "" else init roomId) }
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do -- Bấm "Create Room"
          sendTcpPacket h CTP_CreateRoom
          pure cState
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- Bấm "Join Room"
          sendTcpPacket h (CTP_JoinRoom roomId)
          pure cState
    _ -> pure cState
handleInputRoomSelection _ cState = pure cState

-- === LOBBY ===
handleInputLobby :: Event -> ClientState -> IO ClientState
handleInputLobby event cState@(ClientState { csTcpHandle = h, csState = (S_Lobby ld) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -75 && y < -25) -> do -- Bấm "Select Rapid"
          let newTank = Just Rapid
          sendTcpPacket h (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure cState { csState = S_Lobby ld { ldMyTank = newTank } }
      | (x > 0 && x < 200 && y > -75 && y < -25) -> do -- Bấm "Select Blast"
          let newTank = Just Blast
          sendTcpPacket h (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure cState { csState = S_Lobby ld { ldMyTank = newTank } }
      | (x > -100 && x < 100 && y > -225 && y < -175) -> do -- Bấm "Ready"
          let newReady = not (ldMyReady ld)
          sendTcpPacket h (CTP_UpdateLobbyState (ldMyTank ld) newReady)
          pure cState { csState = S_Lobby ld { ldMyReady = newReady } }
    _ -> pure cState
handleInputLobby _ cState = pure cState

-- === POST GAME ===
handleInputPostGame :: Event -> ClientState -> IO ClientState
handleInputPostGame event cState@(ClientState { csTcpHandle = h }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do -- Bấm "Rematch"
          sendTcpPacket h CTP_RequestRematch
          pure cState -- Chờ server phản hồi
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- Bấm "Exit to Menu"
          sendTcpPacket h CTP_LeaveRoom
          pure cState { csState = S_Menu } -- Quay về Menu
    _ -> pure cState

-- ===================================================================
-- HÀM LOGIC GAME (TỪ CODE CŨ)
-- ===================================================================

-- Hàm helper
initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot [] [] []

dummyAnim :: Resources -> Animation
dummyAnim assets = Animation (resTurretFramesRapid assets) 0.05 0 8 False

-- Xử lý snapshot
updateSnapshot :: Resources -> InGameState -> WorldSnapshot -> InGameState
updateSnapshot assets gdata newSnapshot =
  let
    oldWorld = igsWorld gdata
    oldBullets = wsBullets oldWorld
    newBulletIds = Set.fromList (map bsId (wsBullets newSnapshot))
    disappearedBullets = filter (\b -> bsId b `Set.notMember` newBulletIds) oldBullets
    
    (newNextId, newEffects) = foldl makeEffect (igsNextEffectId gdata, []) disappearedBullets
      where
        makeEffect :: (Int, [Effect]) -> BulletState -> (Int, [Effect])
        makeEffect (nextId, effects) bullet =
          let effect = makeExplosion nextId (resExplosionFrames assets) (bsPosition bullet) --
          in (nextId + 1, effect : effects)
        
  in
    gdata { igsWorld = newSnapshot, igsEffects = igsEffects gdata ++ newEffects, igsNextEffectId = newNextId }

-- Input trong game (code cũ)
handleInputGame :: Event -> InGameState -> InGameState
handleInputGame event gdata =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      gdata { igsDidFire = True
            , igsTurretAnimRapid = startAnimation (igsTurretAnimRapid gdata)
            , igsTurretAnimBlast = startAnimation (igsTurretAnimBlast gdata)
            }
    EventKey key Down _ _ ->
      let newKeys = Set.insert key (igsKeys gdata)
      in gdata { igsKeys = newKeys }
    EventKey key Up _ _ ->
      let newKeys = Set.delete key (igsKeys gdata)
      in gdata { igsKeys = newKeys }
    EventMotion pos ->
      gdata { igsMousePos = pos }
    _ -> gdata

-- Update trong game (code cũ)
updateGame :: Float -> InGameState -> (InGameState, Maybe PlayerCommand)
updateGame dt gdata =
  let
    updatedEffects = map (updateEffect dt) (igsEffects gdata)
    activeEffects = filter (not . isEffectFinished) updatedEffects
    newTurretAnimRapid = updateAnimation dt (igsTurretAnimRapid gdata)
    newTurretAnimBlast = updateAnimation dt (igsTurretAnimBlast gdata)
    
    -- Tạo command
    moveVec = calculateMoveVector (igsKeys gdata)
    (mouseX, mouseY) = igsMousePos gdata
    mathAngle = atan2 mouseY mouseX 
    glossAngle = mathAngle - (pi / 2)
    
    command = PlayerCommand
      { pcMoveVec     = moveVec
      , pcTurretAngle = -glossAngle 
      , pcDidFire     = igsDidFire gdata
      }
      
    newState = gdata 
      { igsEffects = activeEffects
      , igsDidFire = False
      , igsTurretAnimRapid = newTurretAnimRapid
      , igsTurretAnimBlast = newTurretAnimBlast
      }
  in
    (newState, Just command)