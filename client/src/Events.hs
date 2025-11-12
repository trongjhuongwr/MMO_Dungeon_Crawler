{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Events (handleInputIO) where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (MVar, modifyMVar_, modifyMVar, forkIO)
import Control.Exception (try, SomeException, catch, SomeException(..))
import Control.Monad (void, when)
import Types
import Network.Client (sendTcpPacket, connectTcp, tcpListenLoop, udpListenLoop)
import Types.Tank (TankType(..))
import Network.Packet (ClientTcpPacket(..))
import Core.Animation (startAnimation)
import Types.GameMode (GameMode(..))
import Config (loadConfig, ClientConfig(..))
import System.IO (hClose) 
import Data.Maybe (isJust, fromJust)
import qualified Data.ByteString as BS (null)
import qualified Data.ByteString.Lazy as LBS (null)
import qualified Data.Set as Set

-- INPUT CHÍNH (Router)
handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  -- 1. modifyMVar chỉ nên chạy các hàm PURE
  actions <- modifyMVar mvar $ \cState ->
    
    case event of
      (EventResize (w, h)) ->
        let newState = cState { csWindowSize = (fromIntegral w, fromIntegral h) }
        in pure (newState, []) 
      
      _ -> 
        -- 2. Gọi hàm PURE (trả về (State, [IO ()]), không có IO wrapper)
        let (newState, ioActions) = handleStateEvent event cState mvar
        in pure (newState, ioActions)
      
  -- 3. Chạy IO BÊN NGOÀI MVar
  sequence_ actions
  return mvar

-- BỘ ĐỊNH TUYẾN TRẠNG THÁI (PURE)
handleStateEvent :: Event -> ClientState -> MVar ClientState -> (ClientState, [IO ()])
handleStateEvent event cState mvar = case (csState cState) of
  S_Login data_   -> handleInputLogin event cState data_ mvar
  S_Menu          -> handleInputMenu event cState mvar
  S_RoomSelection data_ -> handleInputRoomSelection event cState
  S_Lobby data_   -> handleInputLobby event cState
  S_PvEBotLobby data_ -> handleInputPvEBotLobby event cState
  S_InGame gdata  -> handleInputInGame event cState gdata
  S_PostGame data_ -> handleInputPostGame event cState
  S_Paused gdata isConfirming -> handleInputPaused event cState

-- === LOGIN (PURE, TÁCH IO) ===
handleInputLogin :: Event -> ClientState -> LoginData -> MVar ClientState -> (ClientState, [IO ()])
handleInputLogin event cState ld mvar =
  case event of
    -- Xử lý gõ phím
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = ldUsername ld ++ [c] }
                        PassField -> ld { ldPassword = ldPassword ld ++ [c] }
      in (cState { csState = S_Login updateField }, [])

    -- Xử lý Backspace
    (EventKey (Char '\b') Down _ _) ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
                        PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }
      in (cState { csState = S_Login updateField }, [])
    
    (EventKey (SpecialKey KeyBackspace) Down _ _) ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
                        PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }
      in (cState { csState = S_Login updateField }, [])

    -- Xử lý phím Tab
    (EventKey (SpecialKey KeyTab) Down _ _) ->
      let toggleField = case ldActiveField ld of
                        UserField -> ld { ldActiveField = PassField }
                        PassField -> ld { ldActiveField = UserField }
      in (cState { csState = S_Login toggleField }, [])

    -- Xử lý Click chuột
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Click vào ô Username
      | (x > -20 && x < 180 && y > 25 && y < 75) ->
          (cState { csState = S_Login (ld { ldActiveField = UserField }) }, [])
      -- Click vào ô Password
      | (x > -20 && x < 180 && y > -55 && y < -5) ->
          (cState { csState = S_Login (ld { ldActiveField = PassField }) }, [])

      -- Click nút Login (x = -100)
      | (x > -200 && x < 0 && y > -175 && y < -125) ->
          -- 1. Nhóm 2 'let' lại
          let 
            loginAction = do -- Hành động IO
                putStrLn "[Action] Login action executing..."
                config <- loadConfig "client/config/client.yaml"
                eConn <- try $ connectTcp 
                  (server_host config) 
                  (fromIntegral $ server_tcp_port config) 
                  (server_udp_port config)
                  
                case eConn of
                  Left (e :: SomeException) -> do
                    putStrLn $ "Connection failed: " ++ show e
                    modifyMVar_ mvar $ \s -> pure $ s { csState = S_Login (ld { ldStatus = "Connection failed" }) }
                    
                  Right (h, sockUDP, serverAddrUDP) -> do
                    putStrLn "Connection successful. Forking listeners and sending login."
                    sendTcpPacket (Just h) (CTP_Login (ldUsername ld) (ldPassword ld))
                    void $ forkIO $ tcpListenLoop h mvar
                    void $ forkIO $ udpListenLoop sockUDP mvar
                    modifyMVar_ mvar $ \s -> pure $ s 
                          { csTcpHandle = Just h
                          , csUdpSocket = Just sockUDP
                          , csServerAddr = Just serverAddrUDP
                          }
            
            -- Trạng thái mới (cùng trong 'let' block)
            newState = cState { csState = S_Login (ld { ldStatus = "Logging in..." }) }
          
          -- 2. Thêm 'in'
          in (newState, [loginAction])

      -- Click nút Register (x = 100)
      | (x > 0 && x < 200 && y > -175 && y < -125) ->
          -- 1. Nhóm 2 'let' lại
          let
            registerAction = do
                putStrLn "[Action] Register action executing..."
                config <- loadConfig "client/config/client.yaml"
                eConn <- try $ connectTcp 
                  (server_host config) 
                  (fromIntegral $ server_tcp_port config) 
                  (server_udp_port config)

                case eConn of
                  Left (e :: SomeException) ->
                    modifyMVar_ mvar $ \s -> pure $ s { csState = S_Login (ld { ldStatus = "Connection failed" }) }
                  Right (h, sockUDP, serverAddrUDP) -> do
                    sendTcpPacket (Just h) (CTP_Register (ldUsername ld) (ldPassword ld))
                    void $ forkIO $ tcpListenLoop h mvar
                    void $ forkIO $ udpListenLoop sockUDP mvar
                    modifyMVar_ mvar $ \s -> pure $ s 
                          { csTcpHandle = Just h
                          , csUdpSocket = Just sockUDP
                          , csServerAddr = Just serverAddrUDP
                          }
            
            -- Trạng thái mới (cùng trong 'let' block)
            newState = cState { csState = S_Login (ld { ldStatus = "Registering..." }) }
          
          -- 2. Thêm 'in'
          in (newState, [registerAction])

      | otherwise -> (cState, [])
    _ -> (cState, [])

-- === MAIN MENU (PURE, TÁCH IO) ===
handleInputMenu :: Event -> ClientState -> MVar ClientState -> (ClientState, [IO ()])
handleInputMenu event cState@(ClientState { csTcpHandle = h_maybe }) mvar =
  case event of
    EventKey (MouseButton LeftButton) Down _ (x, y)
      | x > -100 && x < 100 && y > -25 && y < 25 -> -- PvP
          (cState { csState = S_RoomSelection (RoomSelectionData "" "") }, [])
          
      | x > -100 && x < 100 && y > -85 && y < -35 -> -- PvE
          (cState { csState = S_PvEBotLobby (PvEBotLobbyData Nothing Nothing) }, [])
          
      | x > -100 && x < 100 && y > -145 && y < -95 -> -- 2PvE
          (cState, [])

      | x > -100 && x < 100 && y > -205 && y < -155 -> -- "Logout"
          -- Biểu thức duy nhất cho nhánh này là 'case h_maybe of ...'
          case h_maybe of
            Just h  ->
              -- 1. Trạng thái MỚI (PURE)
              let newState = cState 
                    { csTcpHandle = Nothing
                    , csUdpSocket = Nothing
                    , csServerAddr = Nothing
                    , csState = S_Login (LoginData "" "" "Logged out" UserField)
                    }
                  -- 2. Hành động IO (được trả về)
                  action = do
                    putStrLn "[Action] Forking handle closer..."
                    -- *** ĐÂY LÀ THAY ĐỔI QUAN TRỌNG ***
                    void $ forkIO $ do
                      putStrLn "[T_Closer] Closing handle..."
                      void (try (hClose h) :: IO (Either SomeException ()))
                      putStrLn "[T_Closer] Handle closed."
                      
              -- 3. 'in' cho 'let newState/action'
              in (newState, [action])
              
            Nothing -> 
              -- Đã bị disconnect, chỉ cần đảm bảo về S_Login
              (cState { csState = S_Login (LoginData "" "" "Disconnected" UserField) }, [])

      | otherwise -> (cState, [])
      
    _ -> (cState, [])

-- === ROOM SELECTION (PURE) ===
handleInputRoomSelection :: Event -> ClientState -> (ClientState, [IO ()])
handleInputRoomSelection event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_RoomSelection rsd) }) =
  let roomId = rsdRoomId rsd
  in case event of
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      (cState { csState = S_RoomSelection (rsd { rsdRoomId = roomId ++ [c] }) }, [])
    (EventKey (Char '\b') Down _ _) -> 
      (cState { csState = S_RoomSelection (rsd { rsdRoomId = if null roomId then "" else init roomId }) }, [])
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> -- Create
          let action = sendTcpPacket h_maybe CTP_CreateRoom
          in (cState { csState = S_RoomSelection (rsd { rsdError = "" }) }, [action])
      | (x > -100 && x < 100 && y > -85 && y < -35) -> -- Join
          let action = sendTcpPacket h_maybe (CTP_JoinRoom roomId)
          in (cState { csState = S_RoomSelection (rsd { rsdError = "" }) }, [action])
      | (x > -100 && x < 100 && y > -235 && y < -185) -> -- Back
          (cState { csState = S_Menu }, [])
    _ -> (cState, [])

-- === LOBBY (PURE) ===
handleInputLobby :: Event -> ClientState -> (ClientState, [IO ()])
handleInputLobby event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_Lobby ld) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -75 && y < -25) -> -- "Select Rapid"
          let newTank = Just Rapid
              action = sendTcpPacket h_maybe (CTP_UpdateLobbyState newTank (ldMyReady ld))
          in (cState { csState = S_Lobby ld { ldMyTank = newTank } }, [action])
      | (x > 0 && x < 200 && y > -75 && y < -25) -> -- "Select Blast"
          let newTank = Just Blast
              action = sendTcpPacket h_maybe (CTP_UpdateLobbyState newTank (ldMyReady ld))
          in (cState { csState = S_Lobby ld { ldMyTank = newTank } }, [action])
      | (x > -100 && x < 100 && y > -225 && y < -175) -> -- "Ready"
          let newReady = not (ldMyReady ld)
              action = sendTcpPacket h_maybe (CTP_UpdateLobbyState (ldMyTank ld) newReady)
          in (cState { csState = S_Lobby ld { ldMyReady = newReady } }, [action])
      | (x > -100 && x < 100 && y > -285 && y < -235) -> -- "Back"
          let action = sendTcpPacket h_maybe CTP_LeaveRoom
          in (cState, [action])
    _ -> (cState, [])

-- === PVE BOT LOBBY (PURE) ===
handleInputPvEBotLobby :: Event -> ClientState -> (ClientState, [IO ()])
handleInputPvEBotLobby event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_PvEBotLobby ld@(PvEBotLobbyData myTank botTank)) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -300 && x < -100 && y > 125 && y < 175) -> -- My Rapid
          (cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Rapid }) }, [])
      | (x > -300 && x < -100 && y > 65 && y < 115) -> -- My Blast
          (cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Blast }) }, [])
      | (x > 100 && x < 300 && y > 125 && y < 175) -> -- Bot Rapid
          (cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Rapid }) }, [])
      | (x > 100 && x < 300 && y > 65 && y < 115) -> -- Bot Blast
          (cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Blast }) }, [])
      | (x > -100 && x < 100 && y > -175 && y < -125) -> -- Start
          case (myTank, botTank) of
            (Just myT, Just botT) ->
              let action = sendTcpPacket h_maybe (CTP_StartPvEBotMatch myT botT)
              in (cState, [action])
            _ -> (cState, [])
      | (x > -100 && x < 100 && y > -235 && y < -185) -> -- Back
          (cState { csState = S_Menu }, [])
      | otherwise -> (cState, [])
    _ -> (cState, [])

-- === IN GAME (ROUTER) (PURE) ===
handleInputInGame :: Event -> ClientState -> InGameState -> (ClientState, [IO ()])
handleInputInGame event cState gdata = 
  case event of
    (EventKey (SpecialKey KeyEsc) Down _ _) ->
      if (igsMode gdata == PvE)
      then 
        let action = sendTcpPacket (csTcpHandle cState) (CTP_PauseGame True)
        in (cState { csState = S_Paused gdata False }, [action])
      else 
        (cState, []) -- Không pause PvP
    
    _ -> if (igsMatchState gdata == InProgress)
           then (cState { csState = S_InGame (handleInputGame event gdata) }, [])
           else case (igsMatchState gdata) of
                  (GameOver _) -> handleInputPostGame event cState
                  _ -> (cState, [])

-- === POST GAME (PURE) ===
handleInputPostGame :: Event -> ClientState -> (ClientState, [IO ()])
handleInputPostGame event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_PostGame pgData) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> -- "Rematch"
          if Set.notMember (csMyId cState) (pgRematchRequesters pgData)
            then
              let action = sendTcpPacket h_maybe CTP_RequestRematch
                  newSet = Set.insert (csMyId cState) (pgRematchRequesters pgData)
              in (cState { csState = S_PostGame (pgData { pgRematchRequesters = newSet }) }, [action])
            else 
              (cState, [])
      | (x > -100 && x < 100 && y > -85 && y < -35) -> -- "Exit"
          let action = sendTcpPacket h_maybe CTP_LeaveRoom
          in (cState { csState = S_Menu }, [action]) 
    _ -> (cState, [])

-- === IN GAME (LOGIC THUẦN) (PURE) ===
handleInputGame :: Event -> InGameState -> InGameState
handleInputGame event gdata =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      gdata { igsDidFire = True}
    EventKey key Down _ _ ->
      let newKeys = Set.insert key (igsKeys gdata)
      in gdata { igsKeys = newKeys }
    EventKey key Up _ _ ->
      let newKeys = Set.delete key (igsKeys gdata)
      in gdata { igsKeys = newKeys }
    EventMotion pos ->
      gdata { igsMousePos = pos }
    _ -> gdata

-- === PAUSE MENU (PURE) ===
handleInputPaused :: Event -> ClientState -> (ClientState, [IO ()])
handleInputPaused event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_Paused gdata isConfirming) }) =
  case (isConfirming, event) of
    (True, EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -125 && y < -75) -> -- "Yes, Exit"
          let action1 = sendTcpPacket h_maybe (CTP_PauseGame False)
              action2 = sendTcpPacket h_maybe CTP_LeaveRoom
          in (cState { csState = S_Menu }, [action1, action2])
      | (x > 0 && x < 200 && y > -125 && y < -75) -> -- "No, Cancel"
          (cState { csState = S_Paused gdata False }, [])
      | otherwise -> (cState, [])

    (False, EventKey (SpecialKey KeyEsc) Down _ _) -> -- Continue (Esc)
      let action = sendTcpPacket h_maybe (CTP_PauseGame False)
      in (cState { csState = S_InGame gdata }, [action])

    (False, EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > 75 && y < 125) -> -- "Continue"
          let action = sendTcpPacket h_maybe (CTP_PauseGame False)
          in (cState { csState = S_InGame gdata }, [action])
      | (x > -100 && x < 100 && y > 15 && y < 65) -> -- "Settings"
          (cState, [])
      | (x > -100 && x < 100 && y > -45 && y < 5) -> -- "Exit to Menu"
          (cState { csState = S_Paused gdata True }, [])
      | otherwise -> (cState, [])
    _ -> (cState, [])