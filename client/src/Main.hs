-- client/src/Main.hs
module Main where

import Network.Socket
import Data.Binary (encode, decodeOrFail)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Graphics.Gloss.Interface.IO.Game hiding (Picture)
import Graphics.Gloss.Juicy (fromDynamicImage, loadJuicyPNG)
import qualified Data.Set as Set

import qualified Network.Socket.ByteString as BS (recvFrom, sendTo)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)

import Types.Player
import Types.Common
import Network.Packet
import Input (KeyMap, calculateMoveVector) -- Giả sử bạn có file này
import Core.Renderer (render)

import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)
-- Thêm import để sử dụng Picture từ Gloss
import Graphics.Gloss (Picture)

-- | Trạng thái của client
data ClientState = ClientState
  { csKeys        :: KeyMap
  , csMousePos    :: (Float, Float)
  , csWorld       :: WorldSnapshot
  }

initialClientState :: ClientState
initialClientState = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = WorldSnapshot { wsPlayers = [] }
  }

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting client..."
  -- Load assets trước
  eResources <- loadResources
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right (tankBody, tankTurret) -> do
      -- Nếu load thành công, thiết lập network và chạy game
      putStrLn "Assets loaded successfully. Starting game..."
      sock <- socket AF_INET Datagram defaultProtocol
      addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
      runGame (addrAddress addr) sock (tankBody, tankTurret)

-- | Hàm helper để tải một sprite cụ thể từ sprite sheet
loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      -- Convert to RGBA8 then crop the pixels region and wrap back as DynamicImage
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

-- | Tải các resources (assets) của game
loadResources :: IO (Either String (Picture, Picture))
loadResources = do
  mTankBody <- loadSprite "client/assets/textures/tanks/rapid_tank/body.png" (0, 0) (128, 128)   -- tọa độ và kích thước
  case mTankBody of
    Nothing -> return $ Left "Failed to load tank body"
    Just tankBody -> do
      mTankTurret <- loadSprite "client/assets/textures/tanks/rapid_tank/turret.png" (0, 0) (128, 128)       -- tọa độ và kích thước
      case mTankTurret of
        Nothing -> return $ Left "Failed to load tank turret"
        Just tankTurret -> return $ Right (tankBody, tankTurret)

-- | Hàm chạy game chính sau khi đã có đủ mọi thứ
runGame :: SockAddr -> Socket -> (Picture, Picture) -> IO ()
runGame serverAddr sock (tankBody, tankTurret) = do
  clientStateRef <- newMVar initialClientState
  
  -- Gửi gói tin "hello" để server nhận biết và tạo state ban đầu
  putStrLn "[DEBUG] Sending initial handshake packet..."
  sendCmdToServer serverAddr sock Set.empty (0, 0)

  _ <- forkIO $ networkListenLoop sock clientStateRef
  
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    white
    60
    initialClientState
    (\_ -> render (tankBody, tankTurret) . csWorld <$> readMVar clientStateRef)
    handleInput
    (updateClient serverAddr sock)

-- | Vòng lặp nhận dữ liệu từ server
networkListenLoop :: Socket -> MVar ClientState -> IO ()
networkListenLoop sock stateRef = do
  (strictMsg, _) <- BS.recvFrom sock 8192
  let lazyMsg = fromStrict strictMsg
  case decodeOrFail lazyMsg of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG] Failed to decode snapshot: " ++ err
      networkListenLoop sock stateRef
    Right (_, _, newSnapshot) -> do
      putStrLn $ "[DEBUG] Received snapshot: " ++ show newSnapshot
      modifyMVar_ stateRef (\cs -> pure cs { csWorld = newSnapshot })
      networkListenLoop sock stateRef

-- | Xử lý input từ Gloss
handleInput :: Event -> ClientState -> IO ClientState
handleInput (EventKey key Down _ _) cs = do
  let newKeys = Set.insert key (csKeys cs)
  return cs { csKeys = newKeys }
handleInput (EventKey key Up _ _) cs = do
  let newKeys = Set.delete key (csKeys cs)
  return cs { csKeys = newKeys }
handleInput (EventMotion pos) cs = do
  return cs { csMousePos = pos }
handleInput _ cs = return cs

-- | Gửi lệnh tới server
sendCmdToServer :: SockAddr -> Socket -> KeyMap -> (Float, Float) -> IO ()
sendCmdToServer serverAddr sock keys (mouseX, mouseY) = do
  let moveVec     = calculateMoveVector keys
  let turretAngle = atan2 mouseY mouseX - (pi / 2) -- Trừ đi 90 độ (pi/2) để bù lại hướng của asset
  let command     = MoveAndAim moveVec (-turretAngle) -- Vẫn giữ đảo ngược góc quay
  putStrLn $ "[DEBUG] Sending command: " ++ show command
  let lazyMsg = encode command
  let strictMsg = toStrict lazyMsg
  _ <- BS.sendTo sock strictMsg serverAddr
  return ()

-- | Update state của client mỗi frame
updateClient :: SockAddr -> Socket -> Float -> ClientState -> IO ClientState
updateClient serverAddr sock _dt cs = do
  sendCmdToServer serverAddr sock (csKeys cs) (csMousePos cs)
  return cs