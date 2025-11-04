module Main where

import Network.Socket
import Data.Binary (encode, decodeOrFail)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forever) -- <<< Gỡ bỏ 'when'
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
import Network.Packet
import Input (KeyMap, calculateMoveVector)
import Core.Renderer (render, GameAssets(..), loadSpriteSheet)
import Core.Effect
import Core.Animation

import Codec.Picture (readImage, DynamicImage(..), convertRGBA8, pixelAt, generateImage)

-- (ClientState và initial... không đổi)
data ClientState = ClientState
  { csKeys         :: KeyMap
  , csMousePos     :: (Float, Float)
  , csWorld        :: WorldSnapshot
  , csDidFire      :: Bool
  , csEffects      :: [Effect]
  , csNextEffectId :: Int
  }

initialWorldSnapshot :: WorldSnapshot
initialWorldSnapshot = WorldSnapshot { wsPlayers = [], wsEnemies = [], wsBullets = [] }

initialClientState :: ClientState
initialClientState = ClientState
  { csKeys = Set.empty
  , csMousePos = (0, 0)
  , csWorld = initialWorldSnapshot
  , csDidFire = False
  , csEffects = []
  , csNextEffectId = 0
  }

main :: IO ()
main = withSocketsDo $ do
  putStrLn "Starting client..."
  eResources <- loadResources
  case eResources of
    Left err -> putStrLn $ "Failed to load resources: " ++ err
    Right assets -> do
      putStrLn "Assets loaded successfully. Starting game..."
      sock <- socket AF_INET Datagram defaultProtocol
      addr <- head <$> getAddrInfo (Just defaultHints { addrSocketType = Datagram }) (Just "127.0.0.1") (Just "8888")
      runGame (addrAddress addr) sock assets

-- (loadSprite không đổi)
loadSprite :: FilePath -> (Int, Int) -> (Int, Int) -> IO (Maybe Picture)
loadSprite path (x, y) (w, h) = do
  eImg <- readImage path
  case eImg of
    Left _ -> return Nothing
    Right dynImg ->
      let rgba = convertRGBA8 dynImg
          cropped = generateImage (\i j -> pixelAt rgba (x + i) (y + j)) w h
      in return $ fromDynamicImage (ImageRGBA8 cropped)

-- | SỬA KÍCH THƯỚC FRAME
loadResources :: IO (Either String GameAssets)
loadResources = do
  mTankBody <- loadSprite "client/assets/textures/tanks/rapid_tank/body.png" (0, 0) (128, 128)
  mTankTurret <- loadSprite "client/assets/textures/tanks/rapid_tank/turret.png" (0, 0) (128, 128)
  mBullet <- loadJuicyPNG "client/assets/textures/projectiles/bullet_normal.png"
  eExplosionImg <- readImage "client/assets/textures/projectiles/explosion_spritesheet_blast.png"

  case (mTankBody, mTankTurret, mBullet, eExplosionImg) of
    (Just body, Just turret, Just bullet, Right dynExplosionImg) ->
      let
        explosionFrames = loadSpriteSheet dynExplosionImg 256 256 8 
      in
        return $ Right $ GameAssets
          { gaTankBody = body
          , gaTankTurret = turret
          , gaBullet = bullet
          , gaExplosionFrames = explosionFrames
          }
    _ -> return $ Left "Failed to load one or more assets"

-- | SỬA: Đổi nền thành 'black'
runGame :: SockAddr -> Socket -> GameAssets -> IO ()
runGame serverAddr sock assets = do
  clientStateRef <- newMVar initialClientState
  putStrLn "[DEBUG] Sending initial handshake packet..."
  let initialCmd = encode (PlayerCommand (Vec2 0 0) 0.0 False)
  _ <- BS.sendTo sock (toStrict initialCmd) serverAddr
  _ <- forkIO $ networkListenLoop sock assets clientStateRef
  playIO
    (InWindow "MMO Dungeon Crawler" (800, 600) (10, 10))
    black
    60
    clientStateRef
    (renderIO assets)
    handleInputIO
    (updateClientIO serverAddr sock)

renderIO :: GameAssets -> MVar ClientState -> IO Picture
renderIO assets mvar = do
  cs <- readMVar mvar
  return $ render assets (csWorld cs) (csEffects cs)

networkListenLoop :: Socket -> GameAssets -> MVar ClientState -> IO ()
networkListenLoop sock assets stateRef = forever $ do
  (strictMsg, _) <- BS.recvFrom sock 8192
  let lazyMsg = fromStrict strictMsg
  case decodeOrFail lazyMsg of
    Left (_, _, err) -> do
      putStrLn $ "[DEBUG Network] Failed to decode: " ++ err
    Right (_, _, newSnapshot) -> do
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
          pure cs { csWorld = newSnapshot, csEffects = csEffects cs ++ newEffects, csNextEffectId = newNextId }
        )

handleInput :: Event -> ClientState -> ClientState
handleInput event cs =
  case event of
    EventKey (MouseButton LeftButton) Down _ _ ->
      cs { csDidFire = True }
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
  in
    cs { csEffects = activeEffects, csDidFire = False }

updateClientIO :: SockAddr -> Socket -> Float -> MVar ClientState -> IO (MVar ClientState)
updateClientIO serverAddr sock dt mvar = do
  cs <- readMVar mvar
  sendPlayerCommand serverAddr sock cs
  let cs' = updateClient dt cs
  
  _ <- swapMVar mvar cs' 
  
  return mvar