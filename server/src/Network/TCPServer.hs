module Network.TCPServer (startTcpServer) where

import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Network.Socket
import System.IO (hSetEncoding, stdout, utf8)
import qualified Network.Socket.ByteString as BS (sendAll) -- <--- THÊM IMPORT
import qualified Data.ByteString.Char8 as C8 (pack)      -- <--- THÊM IMPORT

-- | Cổng TCP cho Lobby/Chat (dựa trên file config/server.yaml)
tcpPort :: PortNumber
tcpPort = 4000

-- | Khởi động server TCP và lắng nghe kết nối
startTcpServer :: IO ()
startTcpServer = withSocketsDo $ do
  hSetEncoding stdout utf8
  
  addr <- resolve (show tcpPort)
  sock <- open addr
  
  putStrLn $ "[TCP Server] Listening on the port " ++ show tcpPort
  forever $ do
    (conn, clientAddr) <- accept sock
    putStrLn $ "[TCP Server] Client has connected from: " ++ show clientAddr
    _ <- forkIO $ handleClient conn clientAddr
    return ()

  where
    resolve port = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) Nothing (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock 10
      return sock

-- | Xử lý một kết nối client TCP
handleClient :: Socket -> SockAddr -> IO ()
handleClient sock addr = do
  putStrLn $ "[TCP Server] Send a welcome message to " ++ show addr
  
  -- Gửi một tin nhắn chào mừng lobby (S2C = Server-to-Client)
  -- Chúng ta thêm "\n" để client biết khi nào tin nhắn kết thúc
  BS.sendAll sock (C8.pack "S2C_WELCOME_LOBBY\n")
  
  -- Giai đoạn sau: Chúng ta sẽ chờ tin nhắn từ client (ví dụ: "CREATE_ROOM")
  -- Hiện tại, chúng ta đóng kết nối ngay sau khi gửi.
  
  close sock
  putStrLn $ "[TCP Server] Connection closed with " ++ show addr