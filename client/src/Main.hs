module Main where

import Network.Socket
import System.IO (hSetEncoding, stdout, stderr, utf8)
import Data.Binary (encode, decodeOrFail)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever, when) -- <-- THÊM 'when'
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (fromDynamicImage, loadJuicyPNG)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (foldl')

import qualified Network.Socket.ByteString as BS (recvFrom, sendTo)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)

import Types.Player
import Types.Common
import Types.Bullet (BulletState(..))
import Types.Enemy (EnemyState(..))
import Types.Map (GameMap(..), TileType(..)) -- <-- THÊM
import qualified Data.Array as Array
import Network.Packet
import Input (KeyMap, calculateMoveVector)
import Core.Renderer (render, GameAssets(..), loadSpriteSheet)
import qualified Renderer.Resources as R
import Core.Effect (Effect(..), makeExplosion, updateEffect, isEffectFinished)
import Core.Animation (Animation(..), updateAnimation, startAnimation)

import qualified Data.Map as Map
import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)

import Systems.MapLoader (loadMapFromFile) 

data ClientState = ClientState
  { csKeys         :: KeyMap
  , csMousePos     :: (Float, Float)
  , csWorld        :: WorldSnapshot
  , csGameMap      :: GameMap -- <-- THÊM: Client tự giữ map
  , csDidFire      :: Bool
  , csEffects      :: [Effect]
  , csNextEffectId :: Int
  , csTurretAnim   :: Animation
  }

initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot
  { wsPlayers = []
  , wsEnemies = []
  , wsBullets = []
  }

dummyAnim :: Animation
dummyAnim = Animation [] 0 0 0 False

initialClientState :: GameMap -> ClientState
initialClientState gmap = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = initialWorldSnapshot
  , csGameMap = gmap 
  , csDidFire = False
  , csEffects = []
  , csNextEffectId = 0
  , csTurretAnim = dummyAnim
  }

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting client..."
  eResources <- loadResources
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do
      putStrLn "Assets loaded successfully. Starting game..."
      
      let mapToLoad = "client/assets/maps/pvp.json" 
      putStrLn $ "[Client] Loading map: " ++ mapToLoad
      eMapData <- loadMapFromFile mapToLoad
      
      case eMapData of
        Left err -> putStrLn $ "CLIENT FATAL: Không thể tải map: " ++ err
        Right (clientMap, _spawnPoints) -> do -- Client không cần spawn points
          putStrLn "[Client] Map loaded."

          sock <- socket AF_INET Datagram defaultProtocol
          bind sock (SockAddrInet 0 0)
          addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
          
          runGame (addrAddress addr) sock assets clientMap

loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

loadResources :: IO (Either String GameAssets)
loadResources = do
  tileRes <- R.loadResources
  mTankBody <- loadSprite "client/assets/textures/tanks/rapid_tank/body.png" (0, 0) (128, 128)
  eTurretImg <- readImage "client/assets/textures/tanks/rapid_tank/turret.png" 
  mBullet <- loadJuicyPNG "client/assets/textures/projectiles/bullet_normal.png"
  eExplosionImg <- readImage "client/assets/textures/projectiles/explosion_spritesheet_blast.png"
  
  case (mTankBody, eTurretImg, mBullet, eExplosionImg) of
    (Just body, Right dynTurretImg, Just bullet, Right dynExplosionImg) ->
      let
        turretFrames = loadSpriteSheet dynTurretImg 128 128 8 
        explosionFrames = loadSpriteSheet dynExplosionImg 256 256 8 
      in
        return $ Right $ GameAssets
          { gaTankBody = body
          , gaTurretFrames = turretFrames
          , gaBullet = bullet
          , gaExplosionFrames = explosionFrames
          , gaTiles = R.resTiles tileRes
          }
    _ -> return $ Left "Failed to load one or more assets"

runGame :: SockAddr -> Socket -> GameAssets -> GameMap -> IO ()
runGame serverAddr sock assets clientMap = do
  let turretAnim = Animation
        { animFrames = gaTurretFrames assets
        , animFrameTime = 0.05
        , animTimer = 0
        , animCurrentFrame = length (gaTurretFrames assets)
        , animLoops = False
        }

  let initialState = (initialClientState clientMap) { csTurretAnim = turretAnim }
  
  clientStateRef <- newMVar initialState
  
  putStrLn "[DEBUG] Sending initial handshake packet..."
  let initialCmd = encode (PlayerCommand (Vec2 0 0) 0.0 False)
  _ <- BS.sendTo sock (toStrict initialCmd) serverAddr
  _ <- forkIO $ networkListenLoop sock assets clientStateRef
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    black 60
    clientStateRef
    (renderIO assets)
    handleInputIO
    (updateClientIO serverAddr sock)

-- SỬA ĐỔI: renderIO
renderIO :: GameAssets -> MVar ClientState -> IO Picture
renderIO assets mvar = do
  cs <- readMVar mvar
  
  -- Lấy map từ ClientState, KHÔNG phải từ WorldSnapshot
  let gameMap = csGameMap cs
  let snapshot = csWorld cs
  
  -- Truyền cả hai vào render
  return $ render assets gameMap snapshot (csEffects cs) (csTurretAnim cs)

-- SỬA ĐỔI: networkListenLoop
networkListenLoop :: Socket -> GameAssets -> MVar ClientState -> IO ()
networkListenLoop sock assets stateRef = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  let lazyMsg = fromStrict strictMsg
  case decodeOrFail lazyMsg of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG Network] Failed to decode: " ++ err
    Right (_, _, newSnapshot) -> do
      -- (newSnapshot không còn map)
      -- ... (log) ...
      modifyMVar_ stateRef (\cs ->
        let
          oldWorld = csWorld cs
          oldBullets = wsBullets oldWorld
          newBulletIds = Set.fromList (map bsId (wsBullets newSnapshot))
          disappearedBullets = filter (\b -> bsId b `Set.notMember` newBulletIds) oldBullets
          
          (newNextId, newEffects) = foldl' makeEffect (csNextEffectId cs, []) disappearedBullets
            where
              makeEffect :: (Int, [Effect]) -> BulletState -> (Int, [Effect])
              makeEffect (nextId, effects) bullet =
                let effect = makeExplosion nextId (gaExplosionFrames assets) (bsPosition bullet)
                in (nextId + 1, effect : effects)
          
        in
          -- SỬA ĐỔI: Chỉ cập nhật csWorld, KHÔNG chạm vào csGameMap
          pure cs { csWorld = newSnapshot, csEffects = csEffects cs ++ newEffects, csNextEffectId = newNextId }
        )

handleInput :: Event -> ClientState -> ClientState
handleInput event cs =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      cs { csDidFire = True, csTurretAnim = startAnimation (csTurretAnim cs) }
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

updateClient :: Float -> ClientState -> ClientState
updateClient dt cs =
  let
    updatedEffects = map (updateEffect dt) (csEffects cs)
    activeEffects = filter (not . isEffectFinished) updatedEffects
    newTurretAnim = updateAnimation dt (csTurretAnim cs)
  in
    cs { csEffects = activeEffects, csDidFire = False, csTurretAnim = newTurretAnim }

updateClientIO :: SockAddr -> Socket -> Float -> MVar ClientState -> IO (MVar ClientState)
updateClientIO serverAddr sock dt mvar = do
  cs <- readMVar mvar
  sendPlayerCommand serverAddr sock cs
  let cs' = updateClient dt cs
  _ <- swapMVar mvar cs' 
  return mvar