{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Events (handleInputIO) where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (MVar, modifyMVar_, modifyMVar, forkIO)
import Control.Exception (try, SomeException)
import Control.Monad (void)
import qualified Data.Set as Set

import Types
import Network.Client (sendTcpPacket, connectTcp, tcpListenLoop, udpListenLoop)
import Types.Tank (TankType(..))
import Network.Packet (ClientTcpPacket(..))
import Core.Animation (startAnimation)
import Types.GameMode (GameMode(..))
import Config (loadConfig, ClientConfig(..))
import System.IO (hClose) 
import Data.Maybe (isJust)

-- INPUT CHÍNH (Router)
handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  -- Chuyển sang modifyMVar để trả về [IO ()]
  actions <- modifyMVar mvar $ \cState ->
    
    -- Xử lý Resize toàn cục
    case event of
      (EventResize (w, h)) ->
        let newState = cState { csWindowSize = (fromIntegral w, fromIntegral h) }
        in pure (newState, []) -- Không có action
      
      -- Xử lý các event khác theo state
      _ -> handleStateEvent event cState mvar
      
  -- Chạy các IO Action (như forkIO, hClose) bên ngoài MVar
  sequence_ actions
  
  return mvar

-- BỘ ĐỊNH TUYẾN TRẠNG THÁI (CHẠY BÊN TRONG MVAR)
handleStateEvent :: Event -> ClientState -> MVar ClientState -> IO (ClientState, [IO ()])
handleStateEvent event cState mvar = case (csState cState) of
  S_Login data_   -> handleInputLogin event cState data_ mvar
  S_Menu          -> handleInputMenu event cState
  S_RoomSelection data_ -> handleInputRoomSelection event cState
  S_Lobby data_   -> handleInputLobby event cState
  S_PvEBotLobby data_ -> handleInputPvEBotLobby event cState
  S_InGame gdata  -> handleInputInGame event cState gdata
  S_PostGame data_ -> handleInputPostGame event cState
  S_Paused gdata isConfirming -> handleInputPaused event cState

-- === LOGIN ===
handleInputLogin :: Event -> ClientState -> LoginData -> MVar ClientState -> IO (ClientState, [IO ()])
handleInputLogin event cState ld mvar =
  case event of
    -- Xử lý gõ phím
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = ldUsername ld ++ [c] }
                        PassField -> ld { ldPassword = ldPassword ld ++ [c] }
      in pure (cState { csState = S_Login updateField }, [])

    -- Xử lý Backspace
    (EventKey (Char '\b') Down _ _) ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
                        PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }
      in pure (cState { csState = S_Login updateField }, [])
    
    (EventKey (SpecialKey KeyBackspace) Down _ _) ->
      let updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
                        PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }
      in pure (cState { csState = S_Login updateField }, [])

    -- Xử lý phím Tab
    (EventKey (SpecialKey KeyTab) Down _ _) ->
      let toggleField = case ldActiveField ld of
                        UserField -> ld { ldActiveField = PassField }
                        PassField -> ld { ldActiveField = UserField }
      in pure (cState { csState = S_Login toggleField }, [])

    -- Xử lý Click chuột
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Click vào ô Username
      | (x > -20 && x < 180 && y > 25 && y < 75) ->
          pure (cState { csState = S_Login (ld { ldActiveField = UserField }) }, [])
      -- Click vào ô Password
      | (x > -20 && x < 180 && y > -55 && y < -5) ->
          pure (cState { csState = S_Login (ld { ldActiveField = PassField }) }, [])

      -- Click nút Login (x = -100)
      | (x > -200 && x < 0 && y > -175 && y < -125) -> do
          -- 1. Load Config
          config <- loadConfig "client/config/client.yaml"
          
          -- 2. Thử kết nối
          eConn <- try $ connectTcp 
            (server_host config) 
            (fromIntegral $ server_tcp_port config) 
            (server_udp_port config)
            
          case eConn of
            Left (e :: SomeException) -> do
              putStrLn $ "Connection failed: " ++ show e
              let newState = cState { csState = S_Login (ld { ldStatus = "Connection failed" }) }
              pure (newState, [])
              
            Right (h, sockUDP, serverAddrUDP) -> do
              putStrLn "Connection successful. Forking listeners and sending login."
              
              -- 3. Tạo các Action IO
              let sendLoginAction = sendTcpPacket (Just h) (CTP_Login (ldUsername ld) (ldPassword ld))
              let forkTcp = forkIO $ tcpListenLoop h mvar
              let forkUdp = forkIO $ udpListenLoop sockUDP mvar
              
              -- 4. Cập nhật State
              let newState = cState 
                    { csTcpHandle = Just h
                    , csUdpSocket = Just sockUDP
                    , csServerAddr = Just serverAddrUDP
                    , csState = S_Login (ld { ldStatus = "Logging in..." })
                    }
                    
              pure (newState, [sendLoginAction, void forkTcp, void forkUdp])

      -- Click nút Register (x = 100)
      | (x > 0 && x < 200 && y > -175 && y < -125) -> do
          config <- loadConfig "client/config/client.yaml"
          
          -- <<< ĐÂY LÀ PHẦN ĐÃ SỬA LẠI TỪ BÀI TRƯỚC >>>
          eConn <- try $ connectTcp 
            (server_host config) 
            (fromIntegral $ server_tcp_port config) 
            (server_udp_port config)

          case eConn of
            Left (e :: SomeException) -> do
              putStrLn $ "Connection failed: " ++ show e
              pure (cState { csState = S_Login (ld { ldStatus = "Connection failed" }) }, [])
            Right (h, sockUDP, serverAddrUDP) -> do
              let sendRegisterAction = sendTcpPacket (Just h) (CTP_Register (ldUsername ld) (ldPassword ld))
              let forkTcp = forkIO $ tcpListenLoop h mvar
              let forkUdp = forkIO $ udpListenLoop sockUDP mvar
              
              let newState = cState 
                    { csTcpHandle = Just h
                    , csUdpSocket = Just sockUDP
                    , csServerAddr = Just serverAddrUDP
                    , csState = S_Login (ld { ldStatus = "Registering..." })
                    }
              pure (newState, [sendRegisterAction, void forkTcp, void forkUdp])

      | otherwise -> pure (cState, [])
    _ -> pure (cState, [])

-- === MAIN MENU ===
handleInputMenu :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputMenu event cState@(ClientState { csTcpHandle = h_maybe }) =
  case event of
    EventKey (MouseButton LeftButton) Down _ (x, y)
      | x > -100 && x < 100 && y > -25 && y < 25 -> -- PvP
          pure (cState { csState = S_RoomSelection (RoomSelectionData "" "") }, [])
      | x > -100 && x < 100 && y > -85 && y < -35 -> -- PvE
          pure (cState { csState = S_PvEBotLobby (PvEBotLobbyData Nothing Nothing) }, [])
      | x > -100 && x < 100 && y > -145 && y < -95 -> -- 2PvE
          pure (cState, [])
      | x > -100 && x < 100 && y > -205 && y < -155 -> do -- "Logout"
          putStrLn "[Input] Logging out..."
          -- Tạo action đóng handle
          let closeAction = case h_maybe of
                              Just h -> void (try (hClose h) :: IO (Either SomeException ()))
                              Nothing -> pure ()
          
          -- Khi handle bị đóng, thread tcpListenLoop sẽ bị crash
          -- và block `catch` của nó sẽ tự động reset state về S_Login.
          pure (cState, [closeAction])
      | otherwise -> pure (cState, [])
    _ -> pure (cState, [])

-- === ROOM SELECTION ===
handleInputRoomSelection :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputRoomSelection event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_RoomSelection rsd) }) =
  let roomId = rsdRoomId rsd
  in case event of
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      pure (cState { csState = S_RoomSelection (rsd { rsdRoomId = roomId ++ [c] }) }, [])
    (EventKey (Char '\b') Down _ _) -> 
      pure (cState { csState = S_RoomSelection (rsd { rsdRoomId = if null roomId then "" else init roomId }) }, [])
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do -- Create
          let action = sendTcpPacket h_maybe CTP_CreateRoom
          pure (cState { csState = S_RoomSelection (rsd { rsdError = "" }) }, [action])
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- Join
          let action = sendTcpPacket h_maybe (CTP_JoinRoom roomId)
          pure (cState { csState = S_RoomSelection (rsd { rsdError = "" }) }, [action])
      | (x > -100 && x < 100 && y > -235 && y < -185) -> -- Back
          pure (cState { csState = S_Menu }, [])
    _ -> pure (cState, [])

-- === LOBBY ===
handleInputLobby :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputLobby event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_Lobby ld) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -75 && y < -25) -> do -- "Select Rapid"
          let newTank = Just Rapid
          let action = sendTcpPacket h_maybe (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure (cState { csState = S_Lobby ld { ldMyTank = newTank } }, [action])
      | (x > 0 && x < 200 && y > -75 && y < -25) -> do -- "Select Blast"
          let newTank = Just Blast
          let action = sendTcpPacket h_maybe (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure (cState { csState = S_Lobby ld { ldMyTank = newTank } }, [action])
      | (x > -100 && x < 100 && y > -225 && y < -175) -> do -- "Ready"
          let newReady = not (ldMyReady ld)
          let action = sendTcpPacket h_maybe (CTP_UpdateLobbyState (ldMyTank ld) newReady)
          pure (cState { csState = S_Lobby ld { ldMyReady = newReady } }, [action])
      | (x > -100 && x < 100 && y > -285 && y < -235) -> do -- "Back"
          let action = sendTcpPacket h_maybe CTP_LeaveRoom
          pure (cState, [action])
    _ -> pure (cState, [])

-- === PVE BOT LOBBY ===
handleInputPvEBotLobby :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputPvEBotLobby event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_PvEBotLobby ld@(PvEBotLobbyData myTank botTank)) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -300 && x < -100 && y > 125 && y < 175) -> -- My Rapid
          pure (cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Rapid }) }, [])
      | (x > -300 && x < -100 && y > 65 && y < 115) -> -- My Blast
          pure (cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Blast }) }, [])
      | (x > 100 && x < 300 && y > 125 && y < 175) -> -- Bot Rapid
          pure (cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Rapid }) }, [])
      | (x > 100 && x < 300 && y > 65 && y < 115) -> -- Bot Blast
          pure (cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Blast }) }, [])
      | (x > -100 && x < 100 && y > -175 && y < -125) -> do -- Start
          case (myTank, botTank) of
            (Just myT, Just botT) -> do
              let action = sendTcpPacket h_maybe (CTP_StartPvEBotMatch myT botT)
              pure (cState, [action])
            _ -> pure (cState, [])
      | (x > -100 && x < 100 && y > -235 && y < -185) -> -- Back
          pure (cState { csState = S_Menu }, [])
      | otherwise -> pure (cState, [])
    _ -> pure (cState, [])

-- === IN GAME (ROUTER) ===
handleInputInGame :: Event -> ClientState -> InGameState -> IO (ClientState, [IO ()])
handleInputInGame event cState gdata = 
  case event of
    (EventKey (SpecialKey KeyEsc) Down _ _) ->
      if (igsMode gdata == PvE)
      then do
        let action = sendTcpPacket (csTcpHandle cState) (CTP_PauseGame True)
        pure (cState { csState = S_Paused gdata False }, [action])
      else 
        pure (cState, []) -- Không pause PvP
    
    _ -> if (igsMatchState gdata == InProgress)
           then pure (cState { csState = S_InGame (handleInputGame event gdata) }, [])
           else case (igsMatchState gdata) of
                  (GameOver _) -> handleInputPostGame event cState
                  _ -> pure (cState, [])

-- === POST GAME ===
handleInputPostGame :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputPostGame event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_PostGame pgData) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do -- "Rematch"
          if Set.notMember (csMyId cState) (pgRematchRequesters pgData)
            then do
              let action = sendTcpPacket h_maybe CTP_RequestRematch
              let newSet = Set.insert (csMyId cState) (pgRematchRequesters pgData)
              pure (cState { csState = S_PostGame (pgData { pgRematchRequesters = newSet }) }, [action])
            else 
              pure (cState, [])
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- "Exit"
          let action = sendTcpPacket h_maybe CTP_LeaveRoom
          pure (cState { csState = S_Menu }, [action]) 
    _ -> pure (cState, [])

-- === IN GAME (LOGIC THUẦN) ===
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

-- === PAUSE MENU ===
handleInputPaused :: Event -> ClientState -> IO (ClientState, [IO ()])
handleInputPaused event cState@(ClientState { csTcpHandle = h_maybe, csState = (S_Paused gdata isConfirming) }) =
  case (isConfirming, event) of
    (True, EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -125 && y < -75) -> do -- "Yes, Exit"
          let action1 = sendTcpPacket h_maybe (CTP_PauseGame False)
          let action2 = sendTcpPacket h_maybe CTP_LeaveRoom
          pure (cState { csState = S_Menu }, [action1, action2])
      | (x > 0 && x < 200 && y > -125 && y < -75) -> -- "No, Cancel"
          pure (cState { csState = S_Paused gdata False }, [])
      | otherwise -> pure (cState, [])

    (False, EventKey (SpecialKey KeyEsc) Down _ _) -> do -- Continue (Esc)
      let action = sendTcpPacket h_maybe (CTP_PauseGame False)
      pure (cState { csState = S_InGame gdata }, [action])

    (False, EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > 75 && y < 125) -> do -- "Continue"
          let action = sendTcpPacket h_maybe (CTP_PauseGame False)
          pure (cState { csState = S_InGame gdata }, [action])
      | (x > -100 && x < 100 && y > 15 && y < 65) -> -- "Settings"
          pure (cState, [])
      | (x > -100 && x < 100 && y > -45 && y < 5) -> -- "Exit to Menu"
          pure (cState { csState = S_Paused gdata True }, [])
      | otherwise -> pure (cState, [])
    _ -> pure (cState, [])