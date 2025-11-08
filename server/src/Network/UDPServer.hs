{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use unless" #-}

module Network.UDPServer (udpListenLoop) where

import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
import Control.Monad (forever, when)
import Data.Binary (decodeOrFail, encode)
import Network.Socket
import qualified Network.Socket.ByteString as BS (recvFrom)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict)
import Core.Types
import Types.Player (PlayerCommand(..), PlayerState(..))
import Network.Packet
import qualified Data.Map as Map
import Data.List (find)
import Data.Int (Int64)

-- Vòng lặp chính để lắng nghe các gói tin UDP
udpListenLoop :: Socket -> MVar ServerState -> IO ()
udpListenLoop sock serverStateRef = forever $ do
  (strictMsg, addr) <- BS.recvFrom sock 8192 `catch` \(e :: SomeException) -> do
    putStrLn $ "[UDP] Error in recvFrom: " ++ show e
    pure (mempty, SockAddrInet 0 0)

  when (not (LBS.null (fromStrict strictMsg))) $ do
    let lazyMsg = fromStrict strictMsg
    
    -- Decode gói tin UDP mới
    case (decodeOrFail lazyMsg :: Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, ClientUdpPacket)) of
      Left (_, _, errMsg) -> do
        putStrLn $ "[UDP] Failed to decode ClientUdpPacket from " ++ show addr ++ ": " ++ errMsg
        pure ()
      Right (_, _, clientPkt) -> do
        
        -- Xử lý gói tin
        modifyMVar_ serverStateRef $ \sState -> do
          case clientPkt of
            
            -- GÓI HANDSHAKE: Đăng ký địa chỉ UDP với PlayerID
            CUP_Handshake pid -> do
              putStrLn $ "[UDP] Handshake from " ++ show pid ++ " at " ++ show addr
              let (mRoom, mRoomId) = findRoomByPlayerId pid sState
              case (mRoom, mRoomId) of
                (Just room, Just roomId) -> do
                  let mClient = Map.lookup pid (roomPlayers room)
                  case mClient of
                    Nothing -> pure sState -- Lỗi: PlayerID không có trong phòng
                    Just client -> do
                      -- Cập nhật địa chỉ UDP cho client
                      let updatedClient = client { pcUdpAddr = Just addr }
                      let updatedPlayers = Map.insert pid updatedClient (roomPlayers room)
                      let updatedRoom = room { roomPlayers = updatedPlayers }
                      
                      -- Cập nhật RoomGameState
                      case roomGame room of
                        Nothing -> pure sState { ssRooms = Map.insert roomId updatedRoom (ssRooms sState) } -- Game chưa bắt đầu
                        Just gameMVar -> do
                          _ <- modifyMVar gameMVar $ \rgs -> do
                            -- Thay thế "Fake Addr" bằng "Real Addr"
                            let (mFakeAddr, mPlayerState) = findFakeAddrByPlayerId pid rgs
                            case (mFakeAddr, mPlayerState) of
                              (Just fakeAddr, Just pState) -> do
                                let newPlayers = Map.insert addr pState (Map.delete fakeAddr (rgsPlayers rgs))
                                putStrLn $ "[UDP] Registered " ++ show pid ++ " to " ++ show addr
                                pure (rgs { rgsPlayers = newPlayers }, ())
                              _ -> pure (rgs, ()) -- Lỗi: không tìm thấy state
                          pure sState { ssRooms = Map.insert roomId updatedRoom (ssRooms sState) }
                _ -> pure sState -- Lỗi: PlayerID không thuộc phòng nào

            -- GÓI COMMAND: Gửi lệnh vào game
            CUP_Command pCmd -> do
              let (mRoom, _) = findRoomByUdpAddr addr sState
              case mRoom of
                Nothing -> pure sState -- Gói tin rác
                Just room -> do
                  case roomGame room of
                    Nothing -> pure sState -- Phòng chờ
                    Just gameMVar -> do
                      -- Thêm command vào RoomGameState
                      _ <- modifyMVar gameMVar $ \rgs -> do
                        let newCommands = (Command addr pCmd) : rgsCommands rgs
                        pure (rgs { rgsCommands = newCommands }, ())
                      pure sState

-- === HÀM TIỆN ÍCH ===
findRoomByPlayerId :: Int -> ServerState -> (Maybe Room, Maybe String)
findRoomByPlayerId pid sState =
  let found = find (\(_, room) -> Map.member pid (roomPlayers room)) (Map.assocs (ssRooms sState))
  in case found of
    Just (roomId, room) -> (Just room, Just roomId)
    Nothing -> (Nothing, Nothing)

findRoomByUdpAddr :: SockAddr -> ServerState -> (Maybe Room, Maybe String)
findRoomByUdpAddr addr sState =
  let found = find (\(_, room) -> any (\c -> pcUdpAddr c == Just addr) (Map.elems (roomPlayers room))) (Map.assocs (ssRooms sState))
  in case found of
    Just (roomId, room) -> (Just room, Just roomId)
    Nothing -> (Nothing, Nothing)

findFakeAddrByPlayerId :: Int -> RoomGameState -> (Maybe SockAddr, Maybe PlayerState)
findFakeAddrByPlayerId pid rgs =
  let found = find (\(_, pState) -> psId pState == pid) (Map.assocs (rgsPlayers rgs))
  in case found of
    Just (fakeAddr, pState) -> (Just fakeAddr, Just pState)
    Nothing -> (Nothing, Nothing)