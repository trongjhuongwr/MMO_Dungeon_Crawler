{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDPServer (udpListenLoop) where

import Control.Concurrent.MVar (MVar, modifyMVar) -- <-- SỬA: Dùng 'modifyMVar'
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.Binary (decode, decodeOrFail, encode)
import Network.Socket

import qualified Network.Socket.ByteString as BS (recvFrom, sendTo)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)

import Core.Types (Command(..), GameState(..), initialPlayerState)
import Types.Common (Vec2(..))
import Types.Player (PlayerCommand(..), PlayerState(..))
import Types.Tank (TankType(..)) 
import qualified Types.Tank as Tank 
import Network.Packet (ServerPacket(..)) 
import qualified Data.Map as Map

-- Vòng lặp chính để lắng nghe các gói tin UDP
udpListenLoop :: Socket -> MVar GameState -> IO ()
udpListenLoop sock gameStateRef = forever $ do
  (strictMsg, addr) <- BS.recvFrom sock 8192 `catch` \(e :: SomeException) -> do
    putStrLn $ "Error in recvFrom: " ++ show e
    pure (mempty, SockAddrInet 0 0)

  if not (LBS.null (fromStrict strictMsg))
    then do
      let lazyMsg = fromStrict strictMsg
      case (decode lazyMsg :: Either String PlayerCommand) of
        Left _ -> pure ()
        Right command -> do
          
          -- SỬA LỖI LOGIC: Tách IO (sendTo) ra khỏi MVar
          -- 'modifyMVar' sẽ trả về 'Maybe Int' (ID của player mới, nếu có)
          mNewPlayerId <- modifyMVar gameStateRef $ \gs -> do
            let (newPlayers, newCommand, newNextId, mIdToSend) =
                  if Map.member addr (gsPlayers gs)
                    then (gsPlayers gs, Command addr command, gsNextId gs, Nothing) -- Player cũ
                    else 
                      let
                        playerCount = Map.size (gsPlayers gs)
                        spawnPoints = gsSpawns gs
                        spawnPos = if null spawnPoints
                                     then Vec2 0 0 
                                     else spawnPoints !! (playerCount `mod` length spawnPoints)
                        newTankType = if playerCount == 0 then Tank.Rapid else Tank.Blast
                        newPlayerId = gsNextId gs
                        newPlayer = initialPlayerState spawnPos newPlayerId newTankType
                      in
                        (Map.insert addr newPlayer (gsPlayers gs), Command addr command, newPlayerId + 1, Just newPlayerId) -- Player mới
            
            let newCommands = newCommand : gsCommands gs
            let newGameState = gs { gsCommands = newCommands, gsPlayers = newPlayers, gsNextId = newNextId }
            
            -- modifyMVar yêu cầu (a, b), chúng ta trả về (GameState, Maybe Int)
            pure (newGameState, mIdToSend) 
          
          -- HÀNH ĐỘNG IO ĐƯỢC THỰC THI BÊN NGOÀI MVar
          case mNewPlayerId of
            Nothing -> pure () -- Không làm gì
            Just newPlayerId -> do
              -- Player mới, GỬI GÓI WELCOME
              let welcomePkt = SPWelcome newPlayerId
              _ <- BS.sendTo sock (toStrict $ encode welcomePkt) addr
              putStrLn $ "[Server] Sent Welcome ID " ++ show newPlayerId ++ " to " ++ show addr

    else pure ()