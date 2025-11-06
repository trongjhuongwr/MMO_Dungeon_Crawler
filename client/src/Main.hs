module Main where

import Network.Socket
import System.IO (hSetEncoding, stdout, stderr, utf8)
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


-- SỬA ĐỔI: Thêm 2 animation
data ClientState = ClientState
  { csKeys              :: KeyMap
  , csMousePos          :: (Float, Float)
  , csWorld             :: WorldSnapshot
  , csGameMap           :: GameMap 
  , csDidFire           :: Bool
  , csEffects           :: [Effect]
  , csNextEffectId      :: Int
  , csTurretAnimRapid   :: Animation -- <-- SỬA
  , csTurretAnimBlast   :: Animation -- <-- THÊM
  , csResources         :: Resources
  }

initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot
  { wsPlayers = []
  , wsEnemies = []
  , wsBullets = []
  }

dummyAnim :: Animation
dummyAnim = Animation [] 0 0 0 False

-- SỬA ĐỔI: Khởi tạo 2 animation
initialClientState :: GameMap -> Resources -> ClientState
initialClientState gmap assets = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = initialWorldSnapshot
  , csGameMap = gmap 
  , csDidFire = False
  , csEffects = []
  , csNextEffectId = 0
  , csTurretAnimRapid = dummyAnim -- <-- SỬA
  , csTurretAnimBlast = dummyAnim -- <-- THÊM
  , csResources = assets
  }

main :: IO ()
main = withSocketsDo $ do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  
  putStrLn "Starting client..."
  
  eResources <- R.loadResources 
  
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do 
      putStrLn "Assets loaded successfully. Starting game..."
      
      let mapToLoad = "client/assets/maps/pvp.json" 
      putStrLn $ "[Client] Loading map: " ++ mapToLoad
      eMapData <- loadMapFromFile mapToLoad
      
      case eMapData of
        Left err -> putStrLn $ "CLIENT FATAL: Không thể tải map: " ++ err
        Right (clientMap, _spawnPoints) -> do
          putStrLn "[Client] Map loaded."

          sock <- socket AF_INET Datagram defaultProtocol
          bind sock (SockAddrInet 0 0)
          addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
          
          runGame (addrAddress addr) sock assets clientMap
  

runGame :: SockAddr -> Socket -> Resources -> GameMap -> IO ()
runGame serverAddr sock assets clientMap = do
  
  -- SỬA ĐỔI: Tạo cả 2 animation
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

  -- SỬA ĐỔI: Gán 2 animation
  let initialState = (initialClientState clientMap assets) 
        { csTurretAnimRapid = turretAnimRapid
        , csTurretAnimBlast = turretAnimBlast
        }
  
  clientStateRef <- newMVar initialState
  
  putStrLn "[DEBUG] Sending initial handshake packet..."
  let initialCmd = encode (PlayerCommand (Vec2 0 0) 0.0 False)
  _ <- BS.sendTo sock (toStrict initialCmd) serverAddr
  _ <- forkIO $ networkListenLoop sock clientStateRef
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    black 60
    clientStateRef
    renderIO
    handleInputIO
    (updateClientIO serverAddr sock)

renderIO :: MVar ClientState -> IO Picture
renderIO mvar = do
  cs <- readMVar mvar
  
  let gameMap = csGameMap cs
  let snapshot = csWorld cs
  let assets = csResources cs
  
  -- Tính toán góc nòng súng của local
  let (mouseX, mouseY) = csMousePos cs
  let mathAngle = atan2 mouseY mouseX 
  let glossAngle = mathAngle - (pi / 2)
  let localTurretAngle = -glossAngle 
  
  -- SỬA ĐỔI: Truyền cả 2 animation vào render
  return $ render assets gameMap snapshot (csEffects cs) (csTurretAnimRapid cs) (csTurretAnimBlast cs) localTurretAngle

networkListenLoop :: Socket -> MVar ClientState -> IO ()
networkListenLoop sock stateRef = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  case decodeOrFail (fromStrict strictMsg) of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG Network] Failed to decode: " ++ err
    Right (_, _, newSnapshot) -> do
      modifyMVar_ stateRef (\cs ->
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

-- SỬA ĐỔI: Bắn -> kích hoạt CẢ HAI anim
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
  modifyMVar_ mvar (pure . handleInput event)
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

-- SỬA ĐỔI: Cập nhật CẢ HAI anim
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
  cs <- readMVar mvar
  sendPlayerCommand serverAddr sock cs
  let cs' = updateClient dt cs
  _ <- swapMVar mvar cs' 
  return mvar