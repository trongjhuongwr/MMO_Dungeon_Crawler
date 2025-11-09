{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Events (handleInputIO) where

import Graphics.Gloss.Interface.IO.Game
import Control.Concurrent (MVar, modifyMVar_)
import qualified Data.Set as Set

import Types -- Module Types.hs mới
import Network.Client (sendTcpPacket) -- Module Network/Client.hs mới
import Types.Tank (TankType(..))
import Network.Packet (ClientTcpPacket(..))
import Core.Animation (startAnimation)

-- INPUT CHÍNH (Router)
handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  modifyMVar_ mvar $ \cState -> do
    case (csState cState) of
      S_Login data_ -> handleInputLogin event cState
      S_Menu        -> handleInputMenu event cState
      S_RoomSelection data_ -> handleInputRoomSelection event cState
      S_Lobby data_   -> handleInputLobby event cState
      S_DungeonLobby _ -> handleInputDungeonLobby event cState
      S_InGame gdata  -> 
        if (igsMatchState gdata == InProgress)
          then pure $ cState { csState = S_InGame (handleInputGame event gdata) }
          else case (igsMatchState gdata) of
                (GameOver _) -> handleInputPostGame event cState
                _ -> pure cState 
      S_PostGame data_ -> handleInputPostGame event cState
  return mvar

-- === LOGIN ===
handleInputLogin :: Event -> ClientState -> IO ClientState
handleInputLogin event cState@(ClientState { csTcpHandle = h, csState = (S_Login ld) }) =
  case event of
    (EventKey (Char c) Down _ _) -> 
      pure cState { csState = S_Login ld { ldUsername = ldUsername ld ++ [c] } }
    (EventKey (SpecialKey KeyBackspace) Down _ _) -> 
      pure cState { csState = S_Login ld { ldUsername = if null (ldUsername ld) then "" else init (ldUsername ld) } }
    (EventKey (MouseButton LeftButton) Down _ (x, y)) ->
      if (x > -120 && x < 80 && y > -175 && y < -125) 
      then do
        sendTcpPacket h (CTP_Login (ldUsername ld) "")
        pure cState { csState = S_Login ld { ldStatus = "Logging in..." } }
      else pure cState
    _ -> pure cState
handleInputLogin _ cState = pure cState 

-- === MAIN MENU ===
handleInputMenu :: Event -> ClientState -> IO ClientState
handleInputMenu event cState@(ClientState { csTcpHandle = h }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do
          putStrLn "[Input] Clicked Start PvP"
          pure cState { csState = S_RoomSelection "" }
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do
          putStrLn "[Input] Clicked Start PvE"
          pure cState { csState = S_DungeonLobby Nothing } 
      | (x > -100 && x < 100 && y > -145 && y < -95) -> do
          putStrLn "[Input] Clicked Start 2PvE (Disabled)"
          pure cState 
      | otherwise -> pure cState
    _ -> pure cState

handleInputDungeonLobby :: Event -> ClientState -> IO ClientState
handleInputDungeonLobby event cState@(ClientState { csTcpHandle = h, csState = (S_DungeonLobby mTank) }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      -- "Select Rapid"
      | (x > -200 && x < 0 && y > -25 && y < 25) -> 
          pure cState { csState = S_DungeonLobby (Just Rapid) }
      -- "Select Blast"
      | (x > 0 && x < 200 && y > -25 && y < 25) -> 
          pure cState { csState = S_DungeonLobby (Just Blast) }
      -- "Start Dungeon"
      | (x > -100 && x < 100 && y > -225 && y < -175) -> do 
          case mTank of
            Just _ -> do
              putStrLn $ "[Input] Starting PvE with tank: " ++ show mTank
              sendTcpPacket h (CTP_StartDungeon mTank)
              pure cState -- Server sẽ chuyển state sang InGame
            Nothing -> do
              putStrLn "[Input] Must select a tank first!"
              pure cState -- Không làm gì nếu chưa chọn tank
      | otherwise -> pure cState
    _ -> pure cState
handleInputDungeonLobby _ cState = pure cState

-- === ROOM SELECTION ===
handleInputRoomSelection :: Event -> ClientState -> IO ClientState
handleInputRoomSelection event cState@(ClientState { csTcpHandle = h, csState = (S_RoomSelection roomId) }) =
  case event of
    (EventKey (Char c) Down _ _) -> 
      pure cState { csState = S_RoomSelection (roomId ++ [c]) }
    (EventKey (SpecialKey KeyBackspace) Down _ _) -> 
      pure cState { csState = S_RoomSelection (if null roomId then "" else init roomId) }
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do 
          sendTcpPacket h CTP_CreateRoom
          pure cState
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do 
          sendTcpPacket h (CTP_JoinRoom roomId)
          pure cState
    _ -> pure cState
handleInputRoomSelection _ cState = pure cState

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
    _ -> pure cState
handleInputLobby _ cState = pure cState

-- === POST GAME ===
handleInputPostGame :: Event -> ClientState -> IO ClientState
handleInputPostGame event cState@(ClientState { csTcpHandle = h }) =
  case event of
    (EventKey (MouseButton LeftButton) Down _ (x, y))
      | (x > -100 && x < 100 && y > -25 && y < 25) -> do -- "Rematch"
          sendTcpPacket h CTP_RequestRematch
          pure cState 
      | (x > -100 && x < 100 && y > -85 && y < -35) -> do -- "Exit to Menu"
          sendTcpPacket h CTP_LeaveRoom
          pure cState { csState = S_Menu } 
    _ -> pure cState

-- === IN GAME ===
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