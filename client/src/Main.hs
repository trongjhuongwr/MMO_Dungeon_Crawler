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
import Data.List (find)

-- Imports các module đã tách
import Types
import Game
import Network.Client
import Events
import Network.Packet (ClientUdpPacket(..), WorldSnapshot(..))
import qualified Data.Set as Set
import Types.Player (PlayerState(..))
import qualified Types.Tank as Tank

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
      
      let initialWindowSizeInt = (800, 600)
      let initialWindowSizeFloat = (fromIntegral $ fst initialWindowSizeInt, fromIntegral $ snd initialWindowSizeInt)

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
                , csUsername = ""
                , csState = S_Login (LoginData "" "" "Please login" UserField)
                , csResources = assets
                , csWindowSize = initialWindowSizeFloat
                }
          
          clientStateRef <- newMVar initialState
          
          _ <- forkIO $ tcpListenLoop h clientStateRef
          _ <- forkIO $ udpListenLoop sockUDP clientStateRef
          
          playIO
            (InWindow "MMO Dungeon Crawler" initialWindowSizeInt (10, 10))
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
  let windowSize = csWindowSize cState -- <<< LẤY KÍCH THƯỚC TỪ STATE
  
  case (csState cState) of
    S_Login loginData -> pure $ renderLogin loginData windowSize -- <<< TRUYỀN VÀO
    S_Menu -> pure $ renderMenu (csUsername cState) windowSize -- <<< TRUYỀN VÀO
    S_RoomSelection rsd -> pure $ renderRoomSelection rsd windowSize -- <<< TRUYỀN VÀO
    S_Lobby (LobbyData rId pInfo myTank myReady) -> pure $ renderLobby rId pInfo (csMyId cState) myTank myReady windowSize -- <<< TRUYỀN VÀO
    S_PvEBotLobby data_ -> pure $ renderPvEBotLobby data_ windowSize -- <<< TRUYỀN VÀO
    
    S_InGame gdata -> 
      pure $ render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                    (igsEffects gdata) (igsTurretAnimRapid gdata) 
                    (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                    (igsMatchState gdata)
                    windowSize -- <<< TRUYỀN VÀO
                    
    S_PostGame pgData -> pure $ renderPostGame pgData (csMyId cState) windowSize -- <<< TRUYỀN VÀO

    S_Paused gdata isConfirming -> do
      -- 1. Vẽ lại game state y như cũ
      let gamePic = render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                           (igsEffects gdata) (igsTurretAnimRapid gdata) 
                           (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                           (igsMatchState gdata)
                           windowSize -- <<< TRUYỀN VÀO
      
      -- 2. Vẽ lớp phủ làm mờ (dựa trên kích thước)
      let (w, h) = windowSize
      let dimOverlay = Color (makeColor 0 0 0 0.5) $ rectangleSolid w h -- <<< SỬ DỤNG
      
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
              let 
                -- Ràng buộc 1: Xác định status
                status = case (Just (igsMyId gdata'), mWinnerId) of
                            (Just myId, Just winnerId) | myId == winnerId -> "YOU WIN!"
                            (Just _, Nothing) -> "DRAW!"
                            _ -> "YOU LOSE!"
                
                -- Ràng buộc 2: Tìm state của người chơi (Sửa lỗi từ lượt trước)
                myPlayerState = find (\p -> psId p == igsMyId gdata') (wsPlayers $ igsWorld gdata')
                
                -- Ràng buộc 3: Lấy tank (Sửa lỗi từ lượt trước)
                myLastTank = maybe Tank.Rapid psTankType myPlayerState

              -- 'in' chỉ xuất hiện một lần ở cuối
              in pure (cState { csState = S_PostGame (PostGameData status Set.empty myLastTank) }, mCmd)
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