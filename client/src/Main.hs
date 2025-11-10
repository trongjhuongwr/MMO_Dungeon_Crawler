{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Main where

import Network.Socket hiding (recv, SendTo, RecvFrom)
import System.IO
import Control.Exception (try, SomeException)
import Control.Concurrent (forkIO, MVar, newMVar, readMVar, modifyMVar)
import Graphics.Gloss.Interface.IO.Game
import qualified Renderer.Resources as R
import Core.Renderer (render) -- Render game cũ
import UI.Screens
import Types.MatchState (MatchState(..)) 
import Data.Maybe (Maybe(..)) 
import Config (ClientConfig(..), loadConfig)

-- Imports các module đã tách
import Types
import Game
import Network.Client
import Events
import Network.Packet (ClientUdpPacket(..))
import qualified Data.Set as Set

-- ===================================================================
-- HÀM MAIN VÀ KHỞI TẠO
-- ===================================================================

main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  
  putStrLn "Starting client..."

  config <- loadConfig "client/config/client.yaml"
  putStrLn $ "Config loaded: " ++ show config

  eResources <- R.loadResources 
  
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do 
      putStrLn "Assets loaded."
      
      eConn <- try $ connectTcp 
        (server_host config) 
        (fromIntegral $ server_tcp_port config) 
        (server_udp_port config)
      
      case eConn of
        Left (e :: SomeException) -> putStrLn $ "Cannot connect to server: " ++ show e
        Right (h, sockUDP, serverAddrUDP) -> do
          putStrLn "Connected to TCP/UDP."
          
          let initialState = ClientState
                { csTcpHandle = h
                , csUdpSocket = sockUDP
                , csServerAddr = serverAddrUDP
                , csMyId = 0 
                , csState = S_Login (LoginData "" "" "Please login" UserField)
                , csResources = assets
                }
          
          clientStateRef <- newMVar initialState
          
          _ <- forkIO $ tcpListenLoop h clientStateRef
          _ <- forkIO $ udpListenLoop sockUDP clientStateRef
          
          playIO
            (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
            black 60
            clientStateRef
            renderIO
            handleInputIO
            updateClientIO

-- ===================================================================
-- VÒNG LẶP GLOSS (ROUTERS)
-- ===================================================================

-- RENDER CHÍNH (Router)
renderIO :: MVar ClientState -> IO Picture
renderIO mvar = do
  cState <- readMVar mvar
  case (csState cState) of
    S_Login loginData -> pure $ renderLogin loginData
    S_Menu -> pure renderMenu
    S_RoomSelection roomId -> pure $ renderRoomSelection roomId
    S_Lobby (LobbyData rId pInfo myTank myReady) -> pure $ renderLobby rId pInfo (csMyId cState) myTank myReady
    S_DungeonLobby mTank -> pure $ renderDungeonLobby mTank
    S_InGame gdata -> 
      pure $ render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                    (igsEffects gdata) (igsTurretAnimRapid gdata) 
                    (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                    (igsMatchState gdata)
    S_PostGame pgData -> pure $ renderPostGame pgData (csMyId cState)

    S_Paused gdata isConfirming -> do
      -- 1. Vẽ lại game state y như cũ
      let gamePic = render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                           (igsEffects gdata) (igsTurretAnimRapid gdata) 
                           (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                           (igsMatchState gdata)
      -- 2. Vẽ lớp phủ làm mờ
      let dimOverlay = Color (makeColor 0 0 0 0.5) $ rectangleSolid 800 600
      -- 3. Vẽ menu
      let menuPic = renderPauseMenu isConfirming
      
      pure $ Pictures [gamePic, dimOverlay, menuPic]

-- UPDATE CHÍNH (Router)
updateClientIO :: Float -> MVar ClientState -> IO (MVar ClientState)
updateClientIO dt mvar = do

  -- === BƯỚC 1: Tách State Update và IO ===
  -- Lấy ra mCommand (nếu có) và cập nhật state bên trong MVar
  mCommand <- modifyMVar mvar $ \cState -> do
    case (csState cState) of
      S_InGame gdata -> 
        let (gdata', mCmd) = updateGame dt gdata -- <-- Từ Game.hs
        in case (igsMatchState gdata') of
            GameOver mWinnerId ->
              let status = case (Just (igsMyId gdata'), mWinnerId) of
                            (Just myId, Just winnerId) | myId == winnerId -> "YOU WIN!"
                            (Just _, Nothing) -> "DRAW!"
                            _ -> "YOU LOSE!"
              -- Chuyển sang PostGame VÀ trả ra command (cho frame cuối cùng)
              in pure (cState { csState = S_PostGame (PostGameData status Set.empty) }, mCmd)
            _ -> 
              -- Cập nhật InGame VÀ trả ra command
              pure (cState { csState = S_InGame gdata' }, mCmd)
      
      -- Khi Pause, không update game, không gửi packet UDP
      S_Paused _ _ -> pure (cState, Nothing)
      
      _ -> pure (cState, Nothing)

  -- === BƯỚC 2: Thực hiện IO (gửi packet) BÊN NGOÀI MVar ===
  -- Đọc lại state (chỉ đọc, không khóa) để lấy thông tin socket
  cState <- readMVar mvar
  case mCommand of
    Just cmd -> 
      sendUdpPacket (csUdpSocket cState) (csServerAddr cState) (CUP_Command cmd)
    Nothing -> 
      pure ()
  
  -- Trả về MVar
  return mvar