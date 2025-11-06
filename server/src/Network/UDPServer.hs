{-# LANGUAGE ScopedTypeVariables #-}

module Network.UDPServer (udpListenLoop) where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import Data.Binary (decodeOrFail)
import Network.Socket

import qualified Network.Socket.ByteString as BS (recvFrom)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict)

import Core.Types (Command(..), GameState(..), initialPlayerState)
import Types.Common (Vec2(..))
import Types.Player (PlayerCommand(..), PlayerState(..)) -- <-- Import PlayerState(..)
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
      case decodeOrFail lazyMsg of
        Left _ -> pure ()
        Right (_, _, command) -> do
          -- ... (log) ...
          modifyMVar_ gameStateRef $ \gs -> do
            -- SỬA ĐỔI: Logic gán spawn point VÀ ID
            let (newPlayers, newCommand, newNextId) = -- <-- Thêm newNextId
                  if Map.member addr (gsPlayers gs)
                    then (gsPlayers gs, Command addr (command :: PlayerCommand), gsNextId gs) -- <-- ID giữ nguyên
                    else -- Nếu chưa, tạo player mới VÀ GÁN VỊ TRÍ SPAWN
                      let
                        playerCount = Map.size (gsPlayers gs)
                        spawnPoints = gsSpawns gs
                        
                        -- Chọn điểm spawn (quay vòng nếu hết)
                        spawnPos = if null spawnPoints
                                     then Vec2 0 0 -- Fallback
                                     else spawnPoints !! (playerCount `mod` length spawnPoints)
                        
                        -- Lấy ID mới
                        newPlayerId = gsNextId gs
                        newPlayer = initialPlayerState spawnPos newPlayerId -- Gán spawnPos và ID

                      in
                        (Map.insert addr newPlayer (gsPlayers gs), Command addr command, newPlayerId + 1) -- <-- Tăng NextId
            
            let newCommands = newCommand : gsCommands gs
            pure gs { gsCommands = newCommands, gsPlayers = newPlayers, gsNextId = newNextId } -- <-- Cập nhật NextId
    else pure ()