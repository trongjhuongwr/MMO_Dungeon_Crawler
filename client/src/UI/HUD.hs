{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module UI.HUD (renderHUD, renderRadar) where

import Graphics.Gloss
import Types.Player (PlayerState(..))
import Renderer.Resources (Resources(..))
import Types.Common (Vec2(..))

-- Tọa độ góc trên trái của màn hình
screenWidth, screenHeight :: Float
screenWidth = 800.0
screenHeight = 600.0

topLeftX, topLeftY :: Float
topLeftX = -(screenWidth / 2)
topLeftY = screenHeight / 2

-- === THÊM KHỐI NÀY ===
bottomRightX, bottomRightY :: Float
bottomRightX = screenWidth / 2
bottomRightY = -(screenHeight / 2)

-- Cấu hình Radar
radarMaxRange, radarDisplayRadius :: Float
radarMaxRange = 500.0 -- Phạm vi 500 đơn vị thế giới
radarDisplayRadius = 70.0 -- Vẽ trong bán kính 70 pixel (ảnh radar là 150x150)
-- === KẾT THÚC THÊM MỚI ===

maxHealth :: Int
maxHealth = 100

-- Thay đổi chữ ký hàm
-- Hàm render chính cho HUD
renderHUD :: Resources -> PlayerState -> Picture
renderHUD assets player =
  let
    healthBar = drawHealthBar (psHealth player)
    -- Gọi hàm vẽ mạng
    livesPic  = drawLives (resLifeIcons assets) (psLives player)
  in
    Pictures
      [ -- Dịch chuyển thanh máu lên góc trên trái
        Translate (topLeftX + 40) (topLeftY - 50) healthBar
      , -- Dịch chuyển icon mạng ngay bên dưới thanh máu
        Translate (topLeftX + 80) (topLeftY - 100) livesPic
      ]

-- Hàm vẽ thanh máu
drawHealthBar :: Int -> Picture
drawHealthBar currentHP =
  let
    barWidth = 200.0
    barHeight = 20.0

    healthRatio = (fromIntegral currentHP) / (fromIntegral maxHealth)
    currentWidth = barWidth * (max 0 (min 1 healthRatio))

    healthPic = Color red $ rectangleSolid currentWidth barHeight
    backgroundPic = Color (greyN 0.3) $ rectangleSolid barWidth barHeight
    borderPic = Color white $ rectangleWire (barWidth + 2) (barHeight + 2)

  in
    Pictures
      [ Translate (barWidth / 2) 0 backgroundPic
      , Translate (currentWidth / 2) 0 healthPic
      , Translate (barWidth / 2) 0 borderPic
      ]

-- HÀM MỚI: Vẽ số mạng
drawLives :: [Picture] -> Int -> Picture
drawLives lifeFrames lives =
  let
    -- psLives đi từ 3 -> 0.
    -- Mảng lifeFrames có 4 phần tử [3-mạng, 2-mạng, 1-mạng, 0-mạng]
    -- index 0 = 3 mạng
    -- index 1 = 2 mạng
    -- index 2 = 1 mạng
    -- index 3 = 0 mạng

    -- Công thức: index = 3 - lives
    -- Chúng ta kẹp (clamp) giá trị để đảm bảo an toàn
    frameIdx = max 0 (min 3 (3 - lives))
  in
    -- Áp dụng scale 2x giống như map tiles
    Scale 1 1 (lifeFrames !! frameIdx)

-- HÀM MỚI: Vẽ Radar
renderRadar :: Resources -> PlayerState -> [PlayerState] -> Picture
renderRadar assets player opponents =
  let
    -- 1. Định vị Radar ở góc dưới-phải
    --    (Ảnh 150x150, tâm 75x75. Đặt cách lề 80px)
    radarX = bottomRightX - 80.0
    radarY = bottomRightY + 80.0

    -- 2. Lấy ảnh nền radar
    radarBG = resRadar assets

    -- 3. Chấm xanh của người chơi (luôn ở tâm)
    playerDot = Color blue $ circleSolid 3.0

    -- 4. Tính toán và vẽ các chấm đỏ của đối thủ
    opponentDots = map (drawOpponentDot player) opponents

  in
    -- Gộp mọi thứ lại, dịch chuyển về góc
    Translate radarX radarY $ Pictures (radarBG : playerDot : opponentDots)


-- HÀM HELPER: Tính toán và vẽ một chấm đỏ
drawOpponentDot :: PlayerState -> PlayerState -> Picture
drawOpponentDot player opp =
  let
    -- 1. Tính vector vị trí tương đối (Opponent - Player)
    deltaVec = psPosition opp - psPosition player -- Phép trừ Vec2
    (dx, dy) = (vecX deltaVec, vecY deltaVec)

    -- 2. Xoay vector tương đối
    --    Chúng ta muốn hướng "tiến" (psBodyAngle) của người chơi
    --    luôn trỏ lên "phía trên" (trục Y+) của radar.
    --    Góc xoay = (Góc "phía trên" - Góc "tiến" của player)
    angle = (pi/2) - (psBodyAngle player) 

    -- Áp dụng công thức xoay vector
    rx = dx * (cos angle) - dy * (sin angle)
    ry = dx * (sin angle) + dy * (cos angle)

    -- 3. Co dãn (Scale) từ World-space về Radar-space
    scaleFactor = radarDisplayRadius / radarMaxRange
    scaledX = rx * scaleFactor
    scaledY = ry * scaleFactor

    -- 4. Kẹp (Clamp) vị trí vào mép radar nếu ở quá xa
    dist = sqrt (scaledX*scaledX + scaledY*scaledY)
    (finalX, finalY) = if dist > radarDisplayRadius && dist > 0
                       then (scaledX * radarDisplayRadius / dist, scaledY * radarDisplayRadius / dist)
                       else (scaledX, scaledY)
  in
    -- Dịch chuyển chấm đỏ đến vị trí đã tính
    Translate finalX finalY (Color red (circleSolid 3.0))