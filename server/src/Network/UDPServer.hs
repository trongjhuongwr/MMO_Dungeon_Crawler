{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use unless" #-}

module Network.UDPServer (udpListenLoop) where

import Control.Concurrent.MVar (MVar, modifyMVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, when)
import Data.Binary (decodeOrFail, encode)
import Network.Socket
import Data.Int (Int64)

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

  when (not (LBS.null (fromStrict strictMsg))) $ do
    let lazyMsg = fromStrict strictMsg
    
    case (decodeOrFail lazyMsg :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, PlayerCommand)) of
      Left (_, _, errMsg) -> do
        putStrLn $ "[Server] Failed to decode PlayerCommand from " ++ show addr ++ ": " ++ errMsg
        pure ()
      Right (_, _, command) -> do
        
        -- Logic xử lý MVar
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
          
          pure (newGameState, mIdToSend) 
        
        -- Hành động IO bên ngoài MVar
        case mNewPlayerId of
          Nothing -> pure ()
          Just newPlayerId -> do
            -- Player mới, GỬI GÓI WELCOME
            let welcomePkt = SPWelcome newPlayerId
            _ <- BS.sendTo sock (toStrict $ encode welcomePkt) addr
            putStrLn $ "[Server] Sent Welcome ID " ++ show newPlayerId ++ " to " ++ show addr