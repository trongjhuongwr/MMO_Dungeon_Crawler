{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
{-# HLINT ignore "Redundant as" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unless" #-}
module Network.Client
  ( connectTcp
  , sendTcpPacket
  , sendUdpPacket
  , tcpListenLoop
  , udpListenLoop
  ) where

import Network.Socket hiding (recv, SendTo, RecvFrom)
import System.IO
import Control.Exception (try, SomeException, catch)
import Data.Binary (encode, decode, decodeOrFail)
import Control.Concurrent (MVar, newMVar, readMVar, modifyMVar_, modifyMVar, threadDelay)
import Control.Monad (forever, when)
import qualified Data.ByteString.Lazy as LBS
import qualified Network.Socket.ByteString as BS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Network.Packet
import Types.Common
import Types.Player
import Types.MatchState (MatchState(..)) 
import Data.Maybe (Maybe(..), isJust, fromJust)
import Data.List (find, isInfixOf)
import Data.Int (Int64)
import qualified Data.ByteString as BS (hPut) 
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (toStrict, fromStrict)
import qualified Data.Set as Set
import Core.Animation (Animation(..))

import Types
import Game (updateSnapshot, initialWorldSnapshot, dummyAnim)
import Systems.MapLoader (loadMapFromFile) 
import Renderer.Resources (Resources(..))
import qualified Settings as Settings
import Types.GameMode (GameMode(..))
import qualified Data.Set as Set

connectTcp :: HostName -> PortNumber -> String -> IO (Handle, Socket, SockAddr)
connectTcp host tcpPort udpPortString = do
  -- TCP
  addrTCP <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream, addrFamily = AF_INET }) (Just host) (Just $ show tcpPort)
  sockTCP <- socket (addrFamily addrTCP) (addrSocketType addrTCP) (addrProtocol addrTCP)
  
  setSocketOption sockTCP NoDelay 1 
  
  connect sockTCP (addrAddress addrTCP)
  h <- socketToHandle sockTCP ReadWriteMode
  hSetBuffering h NoBuffering
  
  -- UDP
  sockUDP <- socket AF_INET Datagram defaultProtocol 
  bind sockUDP (SockAddrInet 0 0) 
  serverAddrUDP <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram, addrFamily = AF_INET }) (Just host) (Just udpPortString) -- <--- SỬA
  
  return (h, sockUDP, addrAddress serverAddrUDP)

-- Hàm tiện ích gửi gói TCP
sendTcpPacket :: Handle -> ClientTcpPacket -> IO ()
sendTcpPacket h pkt = do
  let lazyMsg = encode pkt
  let strictMsg = toStrict lazyMsg
  BS.hPut h strictMsg
  hFlush h

-- Hàm tiện ích gửi gói UDP
sendUdpPacket :: Socket -> SockAddr -> ClientUdpPacket -> IO ()
sendUdpPacket sock addr pkt = do
  let lazyMsg = encode pkt
  _ <- BS.sendTo sock (toStrict lazyMsg) addr
  return ()

-- Lắng nghe gói TCP từ Server (Quản lý trạng thái)
tcpListenLoop :: Handle -> MVar ClientState -> IO ()
tcpListenLoop h mvar = loop LBS.empty
  where
    -- Vòng lặp 'loop' chỉ ĐỌC từ socket và GỌI 'processBuffer'
    loop :: LBS.ByteString -> IO ()
    loop buffer = do
      strictChunk <- do
        eres <- try (BS.hGetSome h 8192) :: IO (Either SomeException BS.ByteString)
        case eres of
          Left e -> do
            putStrLn $ "[TCP] hGetSome Error: " ++ show e
            pure BS.empty
          Right bs -> pure bs

      if BS.null strictChunk && LBS.null buffer
      then putStrLn "[TCP] Server disconnected." >> fail "Server disconnected"
      else do
        let fullBuffer = buffer <> fromStrict strictChunk
        processBuffer fullBuffer -- Luôn gọi processBuffer với buffer MỚI

    -- 'processBuffer' xử lý buffer (có thể đệ quy)
    processBuffer :: LBS.ByteString -> IO ()
    processBuffer buffer = do
      -- Thử decode
      let decodeResult = decodeOrFail buffer :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ServerTcpPacket)
      
      case decodeResult of
        -- 1. Decode thất bại (chưa đủ dữ liệu)
        Left (_, _, _) -> do
          -- Không đủ data, quay lại 'loop' với buffer hiện tại để chờ đọc thêm
          loop buffer
            
        -- 2. Decode thành công
        Right (remaining, _, pkt) -> do
          putStrLn $ "[TCP] Received: " ++ show pkt
          -- Xử lý packet (logic không đổi)
          modifyMVar_ mvar $ \cState -> do
            case pkt of
              STP_LoginResult success pid msg ->
                if success
                then pure cState { csMyId = pid, csState = S_Menu }
                else pure cState { csState = S_Login (LoginData "" "" msg UserField) }
                
              STP_RoomUpdate roomId pInfos ->
                let myInfo = find (\p -> piId p == csMyId cState) pInfos
                    myTank = myInfo >>= piSelectedTank
                    myReady = maybe False piIsReady myInfo
                    lobbyData = LobbyData roomId pInfos myTank myReady
                in pure cState { csState = S_Lobby lobbyData }

              STP_GameStarting gameMode -> do
                -- SỬ DỤNG CONFIG ASSETS
                let (mapPath, mapName) = case gameMode of
                      PvP -> (Settings.mapPVP, "PvP")
                      PvE -> (Settings.mapPVP, "PvE Bot Match")
                
                eMapData <- loadMapFromFile mapPath
                case eMapData of
                  Left err -> putStrLn ("Failed to load map: " ++ err) >> pure cState
                  Right (gmap, _) -> do
                    myTank <- case (csState cState) of
                      -- Trạng thái trận đầu (PvP)
                      (S_Lobby ld) -> 
                        pure $ fromJust $ ldMyTank ld
                      
                      -- Trạng thái trận đầu (PvE)
                      (S_PvEBotLobby (PvEBotLobbyData (Just tank) _)) ->
                        pure tank
                        
                      -- Trạng thái Đấu Lại (Cả PvP và PvE)
                      (S_PostGame pgd) ->
                        pure $ pgMyLastTank pgd
                        
                      -- Các trạng thái không hợp lệ khác
                      _ -> 
                        fail "Lỗi Client: Nhận STP_GameStarting khi đang ở trạng thái không hợp lệ (ví dụ: không phải S_Lobby hoặc S_PostGame)"
                    
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
                          , igsMatchState = InProgress
                          , igsMode = gameMode
                          }
                    
                    sendUdpPacket (csUdpSocket cState) (csServerAddr cState) (CUP_Handshake (csMyId cState))
                    
                    pure cState { csState = S_InGame newInGameState }
                
              STP_Kicked msg ->
                case (csState cState) of
                  -- Nếu bị "kick" khi đang ở màn hình chọn phòng, có nghĩa là Join thất bại. Hiển thị lỗi.
                  S_RoomSelection rsd -> 
                    pure cState { csState = S_RoomSelection (rsd { rsdError = msg, rsdRoomId = "" }) }

                  _ -> 
                    pure cState { csState = S_Menu }
              
              STP_ShowMenu ->
                pure cState { csState = S_Menu }

              STP_RematchUpdate requesterIds ->
                case (csState cState) of
                  S_PostGame pgData ->
                    let newSet = Set.fromList requesterIds
                    in pure cState { csState = S_PostGame (pgData { pgRematchRequesters = newSet }) }
                  _ -> pure cState -- Bỏ qua nếu không ở màn hình PostGame
          
          -- Xử lý phần buffer còn lại
          if LBS.null remaining
            then loop remaining -- Hết buffer, quay lại chờ đọc
            else do
              putStrLn $ "[TCP] Processing " ++ show (LBS.length remaining) ++ " remaining bytes in buffer."
              processBuffer remaining -- Đệ quy 'processBuffer'

-- Lắng nghe gói UDP từ Server (Trong Game)
udpListenLoop :: Socket -> MVar ClientState -> IO ()
udpListenLoop sock mvar = forever $ do
  eres <- try (BS.recvFrom sock 8192) :: IO (Either SomeException (BS.ByteString, SockAddr))
  case eres of
    Left e -> do
      let serr = show e
      -- Suppress noisy WSA errors that commonly occur when a UDP packet
      -- hits an unreachable port on Windows (ICMP port unreachable).
      if "WSAECONNRESET" `isInfixOf` serr || "Connection reset by peer" `isInfixOf` serr
        then pure ()
        else putStrLn $ "[UDP] recvFrom Error: " ++ show e
      -- backoff a little to avoid tight logging loops
      threadDelay 100000

    Right (strictMsg, _) ->
      -- Chỉ xử lý nếu có dữ liệu
      when (not (BS.null strictMsg)) $ do
        -- 2. Bắt đầu khối modifyMVar_ ĐỂ ĐẢM BẢO TÍNH NGUYÊN TỬ
        modifyMVar_ mvar $ \cState -> do
          -- 3. Kiểm tra trạng thái HIỆN TẠI (lấy từ cState)
          case (csState cState) of
            S_InGame gdata -> do
              -- 4. Decode packet
              case decodeOrFail (fromStrict strictMsg) of
                Left _ -> do
                  -- Lỗi decode, bỏ qua, trả về trạng thái cũ
                  pure cState 

                Right (_, _, (udpPkt :: ServerUdpPacket)) -> do
                  -- 5. Tính toán state mới DỰA TRÊN gdata HIỆN TẠI
                  newGData <- case udpPkt of
                    SUP_MatchStateUpdate newState -> do
                      when (newState /= igsMatchState gdata) $
                        putStrLn $ "[Game] Match status changed to: " ++ show newState
                      pure gdata { igsMatchState = newState }

                    SUP_Snapshot newSnapshot -> 
                      -- updateSnapshot giờ sẽ nhận được gdata "sạch"
                      -- (đã được updateGame lọc)
                      pure $ updateSnapshot (csResources cState) gdata newSnapshot

                  -- 6. Trả về ClientState đã cập nhật
                  pure cState { csState = S_InGame newGData }

            -- 7. Nếu không phải S_InGame, bỏ qua packet, trả về trạng thái cũ
            _ -> pure cState