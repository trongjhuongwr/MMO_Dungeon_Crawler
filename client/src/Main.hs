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

-- Imports các module đã tách
import Types
import Game
import Network.Client
import Events
import Network.Packet (ClientUdpPacket(..))

-- ===================================================================
-- HÀM MAIN VÀ KHỞI TẠO
-- ===================================================================

main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetBuffering stdout LineBuffering
  
  putStrLn "Starting client..."
  eResources <- R.loadResources 
  
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do 
      putStrLn "Assets loaded."
      
      eConn <- try $ connectTcp "127.0.0.1" 4000
      
      case eConn of
        Left (e :: SomeException) -> putStrLn $ "Cannot connect to server: " ++ show e
        Right (h, sockUDP, serverAddrUDP) -> do
          putStrLn "Connected to TCP/UDP."
          
          let initialState = ClientState
                { csTcpHandle = h
                , csUdpSocket = sockUDP
                , csServerAddr = serverAddrUDP
                , csMyId = 0 
                , csState = S_Login (LoginData "Player" "Please login")
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
            handleInputIO -- <-- Từ Events.hs
            updateClientIO

-- ===================================================================
-- VÒNG LẶP GLOSS (ROUTERS)
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
    S_DungeonLobby mTank -> pure $ renderDungeonLobby mTank
    S_InGame gdata -> 
      pure $ render (csResources cState) (igsGameMap gdata) (igsWorld gdata) 
                    (igsEffects gdata) (igsTurretAnimRapid gdata) 
                    (igsTurretAnimBlast gdata) (Just $ igsMyId gdata) 
                    (igsMatchState gdata)
    S_PostGame (PostGameData status) -> pure $ renderPostGame status

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
  modifyMVar mvar $ \cState -> do
    case (csState cState) of
      S_InGame gdata -> 
        let (gdata', mCommand) = updateGame dt gdata -- <-- Từ Game.hs
        in do
          case mCommand of
            Just cmd -> sendUdpPacket (csUdpSocket cState) (csServerAddr cState) (CUP_Command cmd)
            Nothing -> pure ()
          
          case (igsMatchState gdata') of
            GameOver mWinnerId ->
              let status = case (Just (igsMyId gdata'), mWinnerId) of
                            (Just myId, Just winnerId) | myId == winnerId -> "YOU WIN!"
                            (Just _, Nothing) -> "DRAW!"
                            _ -> "YOU LOSE!"
              in pure (cState { csState = S_PostGame (PostGameData status) }, mvar)
            _ -> 
              pure (cState { csState = S_InGame gdata' }, mvar)
      
      -- Khi Pause, không update game, không gửi packet UDP
      S_Paused _ _ -> pure (cState, mvar)
      
      _ -> pure (cState, mvar)