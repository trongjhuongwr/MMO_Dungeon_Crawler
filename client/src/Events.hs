{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Events (handleInputIO) where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (MVar, modifyMVar_)
import qualified Data.Set as Set

import Types
import Network.Client (sendTcpPacket)
import Types.Tank (TankType(..))
import Network.Packet (ClientTcpPacket(..))
import Core.Animation (startAnimation)
import Types.GameMode (GameMode(..))

-- INPUT CHÍNH (Router)
handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  modifyMVar_ mvar $ \cState ->
    
    -- Bước 1: Xử lý các sự kiện toàn cục (như Resize) trước
    case event of
      (EventResize (w, h)) -> do
        putStrLn $ "[Input] Window Resized: " ++ show (w, h)
        pure $ cState { csWindowSize = (fromIntegral w, fromIntegral h) }
      
      -- Bước 2: Chuyển sang các xử lý theo state cho các event khác
      _ -> handleStateSpecificInput event cState
      
  return mvar

handleStateSpecificInput :: Event -> ClientState -> IO ClientState
handleStateSpecificInput event cState =
  case (csState cState) of
    S_Login data_ -> handleInputLogin event cState
    S_Menu        -> handleInputMenu event cState
    S_RoomSelection data_ -> handleInputRoomSelection event cState
    S_Lobby data_   -> handleInputLobby event cState
    S_PvEBotLobby data_ -> handleInputPvEBotLobby event cState
    S_InGame gdata  -> 
      case event of
        -- <--- LOGIC BẮT PHÍM ESC Ở ĐÂY
        (EventKey (SpecialKey KeyEsc) Down _ _) ->
          if (igsMode gdata == PvE) -- Chỉ cho phép pause PvE
          then do
            putStrLn "[Input] Pausing PvE game"
            sendTcpPacket (csTcpHandle cState) (CTP_PauseGame True)
            pure cState { csState = S_Paused gdata False } -- Chuyển sang state Paused
          else 
            pure cState -- Không làm gì ở PvP
        
        -- Xử lý input game bình thường
        _ -> if (igsMatchState gdata == InProgress)
               then pure $ cState { csState = S_InGame (handleInputGame event gdata) }
               else case (igsMatchState gdata) of
                      (GameOver _) -> handleInputPostGame event cState
                      _ -> pure cState 
    
    S_PostGame data_ -> handleInputPostGame event cState
    S_Paused gdata isConfirming -> handleInputPaused event cState

-- === LOGIN ===
handleInputLogin :: Event -> ClientState -> IO ClientState
handleInputLogin event cState@(ClientState { csTcpHandle = h, csState = (S_Login ld) }) =
  case event of
    -- Xử lý gõ phím
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      pure $ cState { csState = S_Login updateField }
      where 
        updateField = case ldActiveField ld of
                        UserField -> ld { ldUsername = ldUsername ld ++ [c] }
                        PassField -> ld { ldPassword = ldPassword ld ++ [c] }

    -- Xử lý Backspace
    (EventKey (Char '\b') Down _ _) ->
      pure $ cState { csState = S_Login (updateField) }
      where 
        updateField = case ldActiveField ld of
          UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
          PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }
    
    (EventKey (SpecialKey KeyBackspace) Down _ _) ->
      pure $ cState { csState = S_Login (updateField) }
      where 
        updateField = case ldActiveField ld of
          UserField -> ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) }
          PassField -> ld { ldPassword = if null (ldPassword ld) then "" else init (ldPassword ld) }

    -- Xử lý phím Tab
    (EventKey (SpecialKey KeyTab) Down _ _) ->
      pure $ cState { csState = S_Login (toggleField) }
      where
        toggleField = case ldActiveField ld of
                        UserField -> ld { ldActiveField = PassField }
                        PassField -> ld { ldActiveField = UserField }

    -- Xử lý Click chuột
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Click vào ô Username
      | (x > -20 && x < 180 && y > 25 && y < 75) ->
          pure cState { csState = S_Login (ld { ldActiveField = UserField }) }
      -- Click vào ô Password
      | (x > -20 && x < 180 && y > -55 && y < -5) ->
          pure cState { csState = S_Login (ld { ldActiveField = PassField }) }

      -- Click nút Login (x = -100)
      | (x > -200 && x < 0 && y > -175 && y < -125) -> do
          sendTcpPacket h (CTP_Login (ldUsername ld) (ldPassword ld))
          pure cState { csState = S_Login ld { ldStatus = "Logging in..." } }

      -- Click nút Register (x = 100)
      | (x > 0 && x < 200 && y > -175 && y < -125) -> do
          sendTcpPacket h (CTP_Register (ldUsername ld) (ldPassword ld))
          pure cState { csState = S_Login ld { ldStatus = "Registering..." } }

      | otherwise -> pure cState
    _ -> pure cState
handleInputLogin _ cState = pure cState

-- === MAIN MENU ===
handleInputMenu :: Event -> ClientState -> IO ClientState
handleInputMenu event cState@(ClientState { csTcpHandle = h }) =
  case event of
    EventKey (MouseButton LeftButton) Down _ (x, y)
      | x > -100 && x < 100 && y > -25 && y < 25 -> do
          putStrLn "[Input] Clicked Start PvP"
          pure cState { csState = S_RoomSelection (RoomSelectionData "" "") }
      | x > -100 && x < 100 && y > -85 && y < -35 -> do
          putStrLn "[Input] Clicked Start PvE (disabled)"
          pure cState { csState = S_PvEBotLobby (PvEBotLobbyData Nothing Nothing) }
      | x > -100 && x < 100 && y > -145 && y < -95 -> do
          putStrLn "[Input] Clicked Start 2PvE (Disabled)"
          pure cState
      | otherwise -> pure cState
    _ -> pure cState

handleInputPvEBotLobby :: Event -> ClientState -> IO ClientState
handleInputPvEBotLobby event cState@(ClientState { csTcpHandle = h, csState = (S_PvEBotLobby ld@(PvEBotLobbyData myTank botTank)) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Cột 1: My Tank
      | (x > -300 && x < -100 && y > 125 && y < 175) -> -- My Rapid
          pure cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Rapid }) }
      | (x > -300 && x < -100 && y > 65 && y < 115) -> -- My Blast
          pure cState { csState = S_PvEBotLobby (ld { pveMyTank = Just Blast }) }
      
      -- Cột 2: Bot Tank
      | (x > 100 && x < 300 && y > 125 && y < 175) -> -- Bot Rapid
          pure cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Rapid }) }
      | (x > 100 && x < 300 && y > 65 && y < 115) -> -- Bot Blast
          pure cState { csState = S_PvEBotLobby (ld { pveBotTank = Just Blast }) }

      -- Nút Start
      | (x > -100 && x < 100 && y > -175 && y < -125) -> do
          case (myTank, botTank) of
            (Just myT, Just botT) -> do
              putStrLn "[Input] Starting PvE Bot Match..."
              -- Gửi gói tin mới
              sendTcpPacket h (CTP_StartPvEBotMatch myT botT)
              pure cState
            _ -> pure cState -- Nút bị vô hiệu hóa, không làm gì

      -- Nút Back
      | (x > -100 && x < 100 && y > -235 && y < -185) -> do
          putStrLn "[Input] Back to Menu"
          pure cState { csState = S_Menu }
      | otherwise -> pure cState
    _ -> pure cState
handleInputPvEBotLobby _ cState = pure cState

-- === ROOM SELECTION ===
handleInputRoomSelection :: Event -> ClientState -> IO ClientState
handleInputRoomSelection event cState@(ClientState { csTcpHandle = h, csState = (S_RoomSelection rsd) }) =
  let roomId = rsdRoomId rsd -- Lấy room ID cũ
  in case event of
    (EventKey (Char c) Down _ _) | c >= ' ' ->
      pure cState { csState = S_RoomSelection (rsd { rsdRoomId = roomId ++ [c] }) }

    (EventKey (Char '\b') Down _ _) -> 
      pure cState { csState = S_RoomSelection (rsd { rsdRoomId = if null roomId then "" else init roomId }) }

    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Create Room (xóa lỗi cũ nếu có)
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do 
          sendTcpPacket h CTP_CreateRoom
          pure cState { csState = S_RoomSelection (rsd { rsdError = "" }) }

      -- Join Room (xóa lỗi cũ khi thử join)
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do 
          sendTcpPacket h (CTP_JoinRoom roomId)
          pure cState { csState = S_RoomSelection (rsd { rsdError = "" }) }

      -- Back to Menu (xóa lỗi cũ)
      | (x > -100 && x < 100 && y > -235 && y < -185) -> do
          putStrLn "[Input] Back to Menu"
          pure cState { csState = S_Menu }
    _ -> pure cState
handleInputRoomSelection _ cState = pure cState -- Fallback

-- === LOBBY ===
handleInputLobby :: Event -> ClientState -> IO ClientState
handleInputLobby event cState@(ClientState { csTcpHandle = h, csState = (S_Lobby ld) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -200 && x < 0 && y > -75 && y < -25) -> do -- "Select Rapid"
          let newTank = Just Rapid
          sendTcpPacket h (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure cState { csState = S_Lobby ld { ldMyTank = newTank } }
      | (x > 0 && x < 200 && y > -75 && y < -25) -> do -- "Select Blast"
          let newTank = Just Blast
          sendTcpPacket h (CTP_UpdateLobbyState newTank (ldMyReady ld))
          pure cState { csState = S_Lobby ld { ldMyTank = newTank } }
      | (x > -100 && x < 100 && y > -225 && y < -175) -> do -- "Ready"
          let newReady = not (ldMyReady ld)
          sendTcpPacket h (CTP_UpdateLobbyState (ldMyTank ld) newReady)
          pure cState { csState = S_Lobby ld { ldMyReady = newReady } }
      | (x > -100 && x < 100 && y > -285 && y < -235) -> do
          putStrLn "[Input] Back (Leaving Room)..."
          -- Gửi yêu cầu rời phòng. Server sẽ phản hồi bằng STP_ShowMenu
          sendTcpPacket h CTP_LeaveRoom
          -- Client không tự ý đổi state, mà chờ phản hồi từ server
          pure cState
    _ -> pure cState
handleInputLobby _ cState = pure cState

-- === POST GAME ===
handleInputPostGame :: Event -> ClientState -> IO ClientState
handleInputPostGame event cState@(ClientState { csTcpHandle = h, csState = (S_PostGame pgData) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Nút "Rematch" (y = 0)
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do 
          -- Chỉ gửi nếu chúng ta chưa yêu cầu
          if Set.notMember (csMyId cState) (pgRematchRequesters pgData)
            then do
              putStrLn "[Input] Requesting Rematch..."
              sendTcpPacket h CTP_RequestRematch
              -- Cập nhật UI ngay lập tức (tạm thời)
              let newSet = Set.insert (csMyId cState) (pgRematchRequesters pgData)
              pure cState { csState = S_PostGame (pgData { pgRematchRequesters = newSet }) }
            else 
              pure cState -- Không làm gì nếu đã click
              
      -- Nút "Exit to Menu" (y = -60)
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- "Exit to Menu"
          putStrLn "[Input] Exiting to Menu."
          sendTcpPacket h CTP_LeaveRoom
          pure cState { csState = S_Menu } 
    _ -> pure cState
handleInputPostGame _ cState = pure cState -- Fallback

-- === IN GAME ===
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
handleInputPaused :: Event -> ClientState -> IO ClientState
handleInputPaused event cState@(ClientState { csTcpHandle = h, csState = (S_Paused gdata isConfirming) }) =
  case (isConfirming, event) of
    
    -- --- Đang ở màn hình xác nhận ---
    (True, EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Nút "Yes, Exit" (x = -100)
      | (x > -200 && x < 0 && y > -125 && y < -75) -> do
          putStrLn "[Input] Confirmed Exit to Menu."
          sendTcpPacket h (CTP_PauseGame False) -- Gửi unpause (dù server sẽ tự xử lý khi LeaveRoom)
          sendTcpPacket h CTP_LeaveRoom
          pure cState { csState = S_Menu }
      -- Nút "No, Cancel" (x = 100)
      | (x > 0 && x < 200 && y > -125 && y < -75) -> do
          putStrLn "[Input] Cancelled Exit."
          pure cState { csState = S_Paused gdata False } -- Quay lại menu pause
      | otherwise -> pure cState

    -- --- Đang ở menu pause chính ---
    (False, EventKey (SpecialKey KeyEsc) Down _ _) -> do -- Nhấn Esc để Continue
      putStrLn "[Input] Resuming game (Esc)"
      sendTcpPacket h (CTP_PauseGame False)
      pure cState { csState = S_InGame gdata }

    (False, EventKey (MouseButton LeftButton) Down _ (x, y))
      -- Nút "Continue" (y = 100)
      | (x > -100 && x < 100 && y > 75 && y < 125) -> do
          putStrLn "[Input] Resuming game (Button)"
          sendTcpPacket h (CTP_PauseGame False)
          pure cState { csState = S_InGame gdata }
      -- Nút "Settings" (y = 40)
      | (x > -100 && x < 100 && y > 15 && y < 65) -> do
          putStrLn "[Input] Settings (Disabled)"
          pure cState
      -- Nút "Exit to Menu" (y = -20)
      | (x > -100 && x < 100 && y > -45 && y < 5) -> do
          putStrLn "[Input] Requesting Exit to Menu..."
          pure cState { csState = S_Paused gdata True } -- Chuyển sang màn hình xác nhận
      | otherwise -> pure cState

    -- Bất kỳ input nào khác thì bỏ qua
    _ -> pure cState
handleInputPaused _ cState = pure cState -- Fallback