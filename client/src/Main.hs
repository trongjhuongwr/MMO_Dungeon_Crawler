module Main where

import Network.Socket hiding (recv) -- <--- SỬA IMPORT
import System.IO (hSetEncoding, stdout, stderr, utf8, hSetBuffering, BufferMode(..), hGetLine, IOMode(ReadMode)) -- <--- THÊM IMPORT
import Control.Exception (bracket, finally, try, SomeException) -- <--- THÊM IMPORT
import Data.Binary (encode, decodeOrFail)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, when)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (fromDynamicImage, loadJuicyPNG)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Array as Array
import qualified Renderer.Resources as R
import Data.List (foldl')

import qualified Network.Socket.ByteString as BS (recvFrom, sendTo)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Network.Packet

import Types.Player
import Types.Common
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..))
import Input (KeyMap, calculateMoveVector)
import Core.Renderer (render)
import Core.Effect (Effect(..), makeExplosion, updateEffect, isEffectFinished)
import Core.Animation (Animation(..), updateAnimation, startAnimation)
import Systems.MapLoader (loadMapFromFile) 
import Renderer.Resources (Resources(..))
import Types.MatchState (MatchState(..)) 

-- (Phần ClientState, initial... giữ nguyên)
data ClientState = ClientState
  { csKeys              :: KeyMap
  , csMousePos          :: (Float, Float)
  , csWorld             :: WorldSnapshot
  , csGameMap           :: GameMap 
  , csDidFire           :: Bool
  , csEffects           :: [Effect]
  , csNextEffectId      :: Int
  , csTurretAnimRapid   :: Animation 
  , csTurretAnimBlast   :: Animation 
  , csResources         :: Resources
  , csMyId              :: Maybe Int
  , csMatchState        :: MatchState 
  }

initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot
  { wsPlayers = []
  , wsEnemies = []
  , wsBullets = []
  }

dummyAnim :: Animation
dummyAnim = Animation [] 0 0 0 False

initialClientState :: GameMap -> Resources -> ClientState
initialClientState gmap assets = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = initialWorldSnapshot
  , csGameMap = gmap 
  , csDidFire = False
  , csEffects = []
  , csNextEffectId = 0
  , csTurretAnimRapid = dummyAnim
  , csTurretAnimBlast = dummyAnim
  , csResources = assets
  , csMyId = Nothing
  , csMatchState = Waiting 
  }

-- *** HÀM MAIN ĐÃ ĐƯỢC THAY ĐỔI ***
main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hSetBuffering stdout LineBuffering -- Đảm bảo in ra ngay lập tức
  
  putStrLn "Starting client..."
  
  eResources <- R.loadResources 
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do 
      putStrLn "Assets loaded successfully."
      
      let mapToLoad = "client/assets/maps/pvp.json" 
      putStrLn $ "[Client] Loading map: " ++ mapToLoad
      eMapData <- loadMapFromFile mapToLoad
      
      case eMapData of
        Left err -> putStrLn $ "CLIENT FATAL: Can't load map: " ++ err
        Right (clientMap, _spawnPoints) -> do
          putStrLn "[Client] Map loaded."
          -- Gọi hàm lobby TCP MỚI, thay vì chạy game ngay
          connectToLobby assets clientMap

-- *** HÀM MỚI: Kết nối đến Lobby TCP trước ***
connectToLobby :: Resources -> GameMap -> IO ()
connectToLobby assets clientMap = do
  putStrLn "[Lobby] Connecting to TCP Server (Lobby) at 127.0.0.1:4000..."
  
  -- Thử kết nối TCP
  eResult <- try $ bracket open close run
  case eResult of
    Left e -> do
      let ex = e :: SomeException
      putStrLn $ "[Lobby] ERROR: Cannot connect to TCP server: " ++ show ex
      putStrLn "Make sure the server is running."
    Right (Just True) -> do
      putStrLn "[Lobby] Connection successful! Starting the game..."
      -- Chỉ khi kết nối TCP thành công, chúng ta mới chạy game UDP/Gloss
      runGame assets clientMap
    _ -> do
      putStrLn "[Lobby] ERROR: The server did not respond correctly to the message."
  
  where
    -- Mở kết nối TCP
    open = do
      addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Stream }) (Just "127.0.0.1") (Just "4000")
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock
      
    -- Chạy logic (chờ tin nhắn chào mừng)
    run sock = do
      h <- socketToHandle sock ReadMode
      hSetBuffering h LineBuffering -- Đọc theo dòng
      
      putStrLn "[Lobby] Connected. Waiting for a welcome message from the server..."
      msg <- hGetLine h -- Đọc một dòng (chờ "\n")
      
      if msg == "S2C_WELCOME_LOBBY"
        then do
          putStrLn "[Lobby] Received the welcome message. Authentication successful."
          return (Just True)
        else do
          putStrLn $ "[Lobby] Strange message from the server: " ++ msg
          return (Just False)

-- *** HÀM NÀY GIỮ NGUYÊN (CHỈ ĐỔI TÊN TỪ main/runGame cũ) ***
-- Bắt đầu vòng lặp game UDP và Gloss
runGame :: Resources -> GameMap -> IO ()
runGame assets clientMap = do
  let turretAnimRapid = Animation
        { animFrames = resTurretFramesRapid assets
        , animFrameTime = 0.05
        , animTimer = 0
        , animCurrentFrame = length (resTurretFramesRapid assets)
        , animLoops = False
        }
  let turretAnimBlast = Animation
        { animFrames = resTurretFramesBlast assets
        , animFrameTime = 0.05
        , animTimer = 0
        , animCurrentFrame = length (resTurretFramesBlast assets)
        , animLoops = False
        }

  let initialState = (initialClientState clientMap assets) 
        { csTurretAnimRapid = turretAnimRapid
        , csTurretAnimBlast = turretAnimBlast
        }
  
  clientStateRef <- newMVar initialState
  
  -- Thiết lập kết nối UDP
  sockUDP <- socket AF_INET Datagram defaultProtocol
  bind sockUDP (SockAddrInet 0 0) -- Ràng buộc vào một cổng ngẫu nhiên
  serverAddrUDP <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
  
  _ <- forkIO $ networkListenLoop sockUDP clientStateRef
  
  putStrLn "[Game] Send UDP Handshake packet..."
  let initialCmd = encode (PlayerCommand (Vec2 0 0) 0.0 False)
  _ <- BS.sendTo sockUDP (toStrict initialCmd) (addrAddress serverAddrUDP)
  
  -- Khởi chạy cửa sổ game
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    black 60
    clientStateRef
    renderIO
    handleInputIO
    (updateClientIO (addrAddress serverAddrUDP) sockUDP)


renderIO :: MVar ClientState -> IO Picture
renderIO mvar = do
  cs <- readMVar mvar
  let gameMap = csGameMap cs
  let snapshot = csWorld cs
  let assets = csResources cs
  return $ render assets gameMap snapshot (csEffects cs) (csTurretAnimRapid cs) (csTurretAnimBlast cs) (csMyId cs) (csMatchState cs)

networkListenLoop :: Socket -> MVar ClientState -> IO ()
networkListenLoop sock stateRef = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  
  case decodeOrFail (fromStrict strictMsg) of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG Network] Failed to decode ServerPacket: " ++ err
    Right (_, _, serverPkt) -> do
      modifyMVar_ stateRef (\cs ->
        
        case serverPkt of
          SPWelcome myId -> do
            putStrLn $ "[Game] SERVER: You are Player " ++ show myId
            pure cs { csMyId = Just myId }
          
          SPMatchStateUpdate newState -> do
            when (newState /= csMatchState cs) $ 
              putStrLn $ "[Game] Match status changed to: " ++ show newState
            pure cs { csMatchState = newState }

          SPSnapshot newSnapshot -> 
            let
              assets = csResources cs
              oldWorld = csWorld cs
              oldBullets = wsBullets oldWorld
              newBulletIds = Set.fromList (map bsId (wsBullets newSnapshot))
              disappearedBullets = filter (\b -> bsId b `Set.notMember` newBulletIds) oldBullets
              
              (newNextId, newEffects) = foldl' makeEffect (csNextEffectId cs, []) disappearedBullets
                where
                  makeEffect :: (Int, [Effect]) -> BulletState -> (Int, [Effect])
                  makeEffect (nextId, effects) bullet =
                    let effect = makeExplosion nextId (resExplosionFrames assets) (bsPosition bullet)
                    in (nextId + 1, effect : effects)
              
            in
              pure cs { csWorld = newSnapshot, csEffects = csEffects cs ++ newEffects, csNextEffectId = newNextId }
        )

handleInput :: Event -> ClientState -> ClientState
handleInput event cs =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      cs { csDidFire = True
         , csTurretAnimRapid = startAnimation (csTurretAnimRapid cs)
         , csTurretAnimBlast = startAnimation (csTurretAnimBlast cs)
         }
    EventKey key Down _ _ ->
      let newKeys = Set.insert key (csKeys cs)
      in cs { csKeys = newKeys }
    EventKey key Up _ _ ->
      let newKeys = Set.delete key (csKeys cs)
      in cs { csKeys = newKeys }
    EventMotion pos ->
      cs { csMousePos = pos }
    _ -> cs

handleInputIO :: Event -> MVar ClientState -> IO (MVar ClientState)
handleInputIO event mvar = do
  modifyMVar_ mvar $ \cs ->
    if csMatchState cs == InProgress
      then pure (handleInput event cs) 
      else pure cs 
  return mvar

sendPlayerCommand :: SockAddr -> Socket -> ClientState -> IO ()
sendPlayerCommand serverAddr sock cs = do
  let moveVec     = calculateMoveVector (csKeys cs)
  let (mouseX, mouseY) = csMousePos cs
  let mathAngle = atan2 mouseY mouseX 
  let glossAngle = mathAngle - (pi / 2)
  let command = PlayerCommand
        { pcMoveVec     = moveVec
        , pcTurretAngle = -glossAngle 
        , pcDidFire     = csDidFire cs
        }
  let lazyMsg = encode command
  let strictMsg = toStrict lazyMsg
  _ <- BS.sendTo sock strictMsg serverAddr
  return ()

updateClient :: Float -> ClientState -> ClientState
updateClient dt cs =
  let
    updatedEffects = map (updateEffect dt) (csEffects cs)
    activeEffects = filter (not . isEffectFinished) updatedEffects
    newTurretAnimRapid = updateAnimation dt (csTurretAnimRapid cs)
    newTurretAnimBlast = updateAnimation dt (csTurretAnimBlast cs)
  in
    cs { csEffects = activeEffects
       , csDidFire = False
       , csTurretAnimRapid = newTurretAnimRapid
       , csTurretAnimBlast = newTurretAnimBlast
       }

updateClientIO :: SockAddr -> Socket -> Float -> MVar ClientState -> IO (MVar ClientState)
updateClientIO serverAddr sock dt mvar = do
  _ <- modifyMVar mvar $ \cs -> do
    when (csMatchState cs == InProgress) $
      sendPlayerCommand serverAddr sock cs
      
    let cs' = updateClient dt cs
    return (cs', ())
  return mvar