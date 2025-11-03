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
import Types.Player (PlayerCommand(..))
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
          modifyMVar_ gameStateRef $ \gs -> do
            -- Kiểm tra xem player đã tồn tại chưa
            let (newPlayers, newCommand) =
                  if Map.member addr (gsPlayers gs)
                    then (gsPlayers gs, Command addr (command :: PlayerCommand))
                    else -- Nếu chưa, tạo player mới
                      let newPlayer = initialPlayerState
                      in (Map.insert addr newPlayer (gsPlayers gs), Command addr command)
            
            let newCommands = newCommand : gsCommands gs
            pure gs { gsCommands = newCommands, gsPlayers = newPlayers }
    else pure ()