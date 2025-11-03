module Main where

import Network.Socket
import Data.Binary (encode, decodeOrFail)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (fromDynamicImage, loadJuicyPNG)
import qualified Data.Set as Set

import qualified Network.Socket.ByteString as BS (recvFrom, sendTo)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)

import Types.Player
import Types.Common
import Types.Bullet (BulletState(..)) -- Import
import Types.Enemy (EnemyState(..))   -- Import
import Network.Packet
import Input (KeyMap, calculateMoveVector)
import Core.Renderer (render, GameAssets(..)) -- Import GameAssets

import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)

-- | Trạng thái của client
data ClientState = ClientState
  { csKeys        :: KeyMap
  , csMousePos    :: (Float, Float)
  , csWorld       :: WorldSnapshot
  , csDidFire     :: Bool -- Thêm trạng thái "vừa bắn"
  }

initialClientState :: ClientState
initialClientState = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = WorldSnapshot { wsPlayers = [], wsEnemies = [], wsBullets = [] } -- Khởi tạo
  , csDidFire = False
  }

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting client..."
  eResources <- loadResources -- Tải assets
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do -- assets giờ là GameAssets
      putStrLn "Assets loaded successfully. Starting game..."
      sock <- socket AF_INET Datagram defaultProtocol
      addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
      runGame (addrAddress addr) sock assets -- Truyền assets vào game

-- Tải một sprite (không đổi)
loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

-- | Tải các resources (assets) của game
loadResources :: IO (Either String GameAssets)
loadResources = do
  mTankBody <- loadSprite "client/assets/textures/tanks/rapid_tank/body.png" (0, 0) (128, 128)
  mTankTurret <- loadSprite "client/assets/textures/tanks/rapid_tank/turret.png" (0, 0) (128, 128)
  mBullet <- loadJuicyPNG "client/assets/textures/projectiles/bullet_normal.png" -- Load asset đạn

  case (mTankBody, mTankTurret, mBullet) of
    (Just body, Just turret, Just bullet) ->
      return $ Right $ GameAssets
        { gaTankBody = body
        , gaTankTurret = turret
        , gaBullet = bullet
        }
    _ -> return $ Left "Failed to load one or more assets"

-- | Hàm chạy game chính
runGame :: SockAddr -> Socket -> GameAssets -> IO ()
runGame serverAddr sock assets = do
  clientStateRef <- newMVar initialClientState
  
  putStrLn "[DEBUG] Sending initial handshake packet..."
  sendPlayerCommand serverAddr sock initialClientState -- Gửi 1 gói tin rỗng để đăng ký

  _ <- forkIO $ networkListenLoop sock clientStateRef
  
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    white
    60
    initialClientState
    (\_ -> render assets . csWorld <$> readMVar clientStateRef) -- Dùng assets
    (handleInputIO clientStateRef) -- Thay đổi handleInput
    (updateClientIO serverAddr sock) -- Thay đổi updateClient

-- | Vòng lặp nhận dữ liệu từ server
networkListenLoop :: Socket -> MVar ClientState -> IO ()
networkListenLoop sock stateRef = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  let lazyMsg = fromStrict strictMsg
  case decodeOrFail lazyMsg of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG] Failed to decode snapshot: " ++ err
      -- Không dừng vòng lặp, chỉ bỏ qua gói tin lỗi
    Right (_, _, newSnapshot) -> do
      -- putStrLn $ "[DEBUG] Received snapshot: " ++ show newSnapshot
      modifyMVar_ stateRef (\cs -> pure cs { csWorld = newSnapshot })

-- | Xử lý input từ Gloss (cần IO vì modifyMVar)
handleInputIO :: MVar ClientState -> Event -> ClientState -> IO ClientState
handleInputIO stateRef event cs =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      -- Ghi nhận sự kiện bắn, sẽ được gửi đi ở frame tiếp theo
      return cs { csDidFire = True }
      
    EventKey key Down _ _ ->
      let newKeys = Set.insert key (csKeys cs)
      in return cs { csKeys = newKeys }
      
    EventKey key Up _ _ ->
      let newKeys = Set.delete key (csKeys cs)
      in return cs { csKeys = newKeys }
      
    EventMotion pos ->
      return cs { csMousePos = pos }
      
    _ -> return cs

-- | Gửi lệnh tới server
sendPlayerCommand :: SockAddr -> Socket -> ClientState -> IO ()
sendPlayerCommand serverAddr sock cs = do
  let moveVec     = calculateMoveVector (csKeys cs)
  let (mouseX, mouseY) = csMousePos cs
  let turretAngle = atan2 mouseY mouseX - (pi / 2)
  
  -- Xây dựng gói tin PlayerCommand mới
  let command = PlayerCommand
        { pcMoveVec     = moveVec
        , pcTurretAngle = -turretAngle -- Vẫn giữ đảo ngược góc quay
        , pcDidFire     = csDidFire cs -- Gửi trạng thái bắn
        }
        
  -- putStrLn $ "[DEBUG] Sending command: " ++ show command
  let lazyMsg = encode command
  let strictMsg = toStrict lazyMsg
  _ <- BS.sendTo sock strictMsg serverAddr
  return ()

-- | Update state của client mỗi frame
updateClientIO :: SockAddr -> Socket -> Float -> ClientState -> IO ClientState
updateClientIO serverAddr sock _dt cs = do
  sendPlayerCommand serverAddr sock cs -- Gửi trạng thái input hiện tại
  -- Reset trạng thái "bắn" về False
  -- Điều này đảm bảo lệnh bắn chỉ được gửi 1 lần cho mỗi cú click
  return cs { csDidFire = False }