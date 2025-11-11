{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module UI.Screens
  ( renderLogin
  , renderMenu
  , renderRoomSelection
  , renderLobby
  , renderPostGame
  , renderPvEBotLobby
  , renderPauseMenu
  ) where

import Graphics.Gloss
import Network.Packet (PlayerInfo(..))
import Types.Tank (TankType(..))
import Data.Maybe (isJust, fromJust)
import Data.List (find)
import qualified Data.Set as Set
import Types (PostGameData(..), LoginData(..), ActiveField(..), RoomSelectionData(..), PvEBotLobbyData(..))
-- === HÀM TIỆN ÍCH VẼ ===

-- Vẽ một nút bấm
drawButton :: (Float, Float) -> String -> Picture
drawButton (x, y) text = Pictures
  [ Translate x y $ Color (greyN 0.2) $ rectangleSolid 200 50
  , Translate (x - 80) (y - 10) $ Scale 0.2 0.2 $ Color white $ Text text
  , Translate x y $ Color white $ rectangleWire 200 50
  ]

-- Vẽ text
drawText :: (Float, Float) -> Float -> String -> Picture
drawText (x, y) size text = Translate x y $ Scale size size $ Color white $ Text text

-- === CÁC MÀN HÌNH ===

renderLogin :: LoginData -> Picture
renderLogin (LoginData username password status activeField) = Pictures
  [ Color black $ rectangleSolid 800 600   -- Background
  , drawText (-80, 150) 0.3 "LOGIN"

  -- Username
  , drawText (-200, 40) 0.2 "Username:"
  , drawButton (80, 50) username
  , if activeField == UserField -- Hiển thị viền active
      then Translate 80 50 $ Color yellow $ rectangleWire 200 50
      else Blank

  -- Password
  , drawText (-200, -40) 0.2 "Password:"
  , drawButton (80, -30) (map (const '*') password) -- Hiển thị '*'
  , if activeField == PassField -- Hiển thị viền active
      then Translate 80 (-30) $ Color yellow $ rectangleWire 200 50
      else Blank

  -- Nút Login (trước là "Submit")
  , drawButton (-100, -150) "   Login"

  -- Nút Register MỚI
  , drawButton (100, -150) " Register"

  , drawText (-80, -200) 0.1 status
  ]

renderMenu :: Picture
renderMenu = Pictures
  [ Color black $ rectangleSolid 800 600   -- Background
  , drawText (-100, 100) 0.4 "MAIN MENU"
  , drawButton (0, 0) "  PvP"
  , Color (greyN 0.5) $ drawButton (0, -60) "PvE (Disabled)"
  , Color (greyN 0.5) $ drawButton (0, -120) "2PvE (Disabled)"
  ]

renderRoomSelection :: RoomSelectionData -> Picture
renderRoomSelection rsd = Pictures
  [ Color black $ rectangleSolid 800 600
  , drawText (-150, 100) 0.3 "PVP LOBBY"
  , drawButton (0, 0) "Create Room"
  , drawButton (0, -60) "Join Room"
  , drawText (-150, -120) 0.2 "Room ID:"
  , drawButton (0, -150) (rsdRoomId rsd) 
  , drawButton (0, -210) "Back"
  , Translate (-200) (-250) $ Scale 0.15 0.15 $ Color red $ Text (rsdError rsd)
  ]

renderLobby :: String -> [PlayerInfo] -> Int -> Maybe TankType -> Bool -> Picture
renderLobby roomId players myId myTank myReady = Pictures
  [ Color black $ rectangleSolid 800 600   -- Background
  , drawText (-350, 250) 0.2 ("Room ID: " ++ roomId)
  
  -- Thông tin người chơi
  , drawPlayerInfo 1 (getPlayer 1 players) myId
  , drawPlayerInfo 2 (getPlayer 2 players) myId
  
  -- Nút chọn Tank
  , let (c1, t1) = if myTank == Just Rapid then (cyan, "Selected") else (white, "Select RAPID")
    in Color c1 $ drawButton (-100, -50) t1
  , let (c2, t2) = if myTank == Just Blast then (orange, "Selected") else (white, "Select BLAST")
    in Color c2 $ drawButton (100, -50) t2
    
  -- Mô tả Tank
  , drawText (-100, -100) 0.1 "Rapid: Speed = 100, Damage = 4, Cooldown = 0.2s"
  , drawText (-100, -120) 0.1 "Blast: Speed = 70, Damage = 25, Cooldown = 1s"
    
  -- Nút Sẵn sàng
  , let (c3, t3) = if myReady then (green, "NOT READY") else (red, "READY")
    in Color c3 $ drawButton (0, -200) t3
  , drawButton (0, -260) "BACK"
  ]
  where
    getPlayer :: Int -> [PlayerInfo] -> Maybe PlayerInfo
    getPlayer 1 ps = if not (null ps) then Just (head ps) else Nothing
    getPlayer 2 ps = if length ps > 1 then Just (ps !! 1) else Nothing
    
    drawPlayerInfo :: Int -> Maybe PlayerInfo -> Int -> Picture
    drawPlayerInfo slot mInfo selfId =
      let y = 150
          x = if slot == 1 then -200 else 200
          name = maybe "(Waiting...)" piName mInfo
          
          tank = case (mInfo >>= piSelectedTank) of
                  Just Rapid -> "Rapid"
                  Just Blast -> "Blast"
                  _ -> "None"
                  
          ready = maybe False piIsReady mInfo
          isSelf = maybe False (\i -> piId i == selfId) mInfo
          selfColor = if isSelf then yellow else white
      in Pictures
        [ Color (greyN 0.1) $ Translate x y $ rectangleSolid 180 100
        , Translate (x-80) (y+30) $ Scale 0.2 0.2 $ Color selfColor $ Text name
        , Translate (x-80) (y) $ Scale 0.15 0.15 $ Color white $ Text ("Tank: " ++ tank)
        , Translate (x-80) (y-30) $ Scale 0.15 0.15 $ Color (if ready then green else red) $ Text (if ready then "Ready" else "Not Ready")
        ]

renderPostGame :: PostGameData -> Int -> Picture
renderPostGame (PostGameData status requesters _myLastTank) myId = Pictures
  [ Color black $ rectangleSolid 800 600 -- Background
  , drawText (-100, 100) 0.5 status
  
  -- Logic cho nút Rematch
  , let
      (buttonPic, helpText) = if Set.member myId requesters
        then (Color (greyN 0.5) (drawButton (0, 0) "Waiting..."), "Waiting for opponent...")
        else (drawButton (0, 0) "Rematch", "")
    in Pictures 
      [ buttonPic
      , drawText (-150, -25) 0.1 helpText
      ]
      
  , drawButton (0, -60) "Exit to Menu"
  ]

renderPvEBotLobby :: PvEBotLobbyData -> Picture
renderPvEBotLobby (PvEBotLobbyData myTank botTank) = Pictures
  [ Color black $ rectangleSolid 800 600 -- Background
  , drawText (-200, 250) 0.3 "PVE BOT MATCH"

  -- Cột 1: Chọn Tank Của Bạn
  , drawText (-200, 200) 0.2 "My Tank"
  , let (c1, t1) = if myTank == Just Rapid then (cyan, "Selected") else (white, "Select RAPID")
    in Color c1 $ drawButton (-200, 150) t1
  , let (c2, t2) = if myTank == Just Blast then (orange, "Selected") else (white, "Select BLAST")
    in Color c2 $ drawButton (-200, 90) t2

  -- Cột 2: Chọn Tank Của Bot
  , drawText (200, 200) 0.2 "Bot Tank"
  , let (c3, t3) = if botTank == Just Rapid then (cyan, "Selected") else (white, "Select RAPID")
    in Color c3 $ drawButton (200, 150) t3
  , let (c4, t4) = if botTank == Just Blast then (orange, "Selected") else (white, "Select BLAST")
    in Color c4 $ drawButton (200, 90) t4

  -- Mô tả Tank
  , drawText (-150, -20) 0.1 "Rapid: Speed = 100, Damage = 4, Cooldown = 0.2s"
  , drawText (-150, -40) 0.1 "Blast: Speed = 70, Damage = 25, Cooldown = 1s"

  -- Nút Bắt đầu (chỉ bật khi cả 2 đã chọn)
  , case (myTank, botTank) of
      (Just _, Just _) -> drawButton (0, -150) "Start Match"
      _ -> Color (greyN 0.5) $ drawButton (0, -150) "Start Match"
  , drawButton (0, -210) "Back"
  ]

renderPauseMenu :: Bool -> Picture
renderPauseMenu isConfirmingExit =
  if isConfirmingExit
    then Pictures
      [ drawText (-250, 50) 0.3 "Exit to Menu?"
      , drawText (-300, 0) 0.2 "All progress in this run will be lost."
      , drawButton (-100, -100) "Yes, Exit"
      , drawButton (100, -100) "No, Cancel"
      ]
    else Pictures
      [ drawText (-100, 200) 0.4 "PAUSED"
      , drawButton (0, 100) "Continue"
      , Color (greyN 0.5) $ drawButton (0, 40) "Settings (Disabled)"
      , drawButton (0, -20) "Exit to Menu"
      ]