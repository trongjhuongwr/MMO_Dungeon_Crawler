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
radarScale :: Float
radarScale = 2.0 -- <-- TỶ LỆ PHÓNG TO (Dùng 2.0 như bạn thử nghiệm)

radarDisplayRadius :: Float
radarDisplayRadius = 40-- <-- BÁN KÍNH GỐC CỦA ASSET (KHÔNG NHÂN SCALE)

radarMaxRange :: Float
radarMaxRange = 200 -- Tầm nhìn (World units)

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
    scaledRadius = radarDisplayRadius * radarScale
    radarPadding = scaledRadius + 5.0
    radarX = bottomRightX - radarPadding
    radarY = bottomRightY + radarPadding

    -- scale riêng cho background thôi
    radarBG = Scale radarScale radarScale (resRadar assets)
    playerDot = Color blue $ circleSolid 3.0
    opponentDots = map (drawOpponentDot player) opponents
  in
    Translate radarX radarY $
    Pictures (radarBG : playerDot : opponentDots)


-- HÀM HELPER: Tính toán và vẽ một chấm đỏ
drawOpponentDot :: PlayerState -> PlayerState -> Picture
drawOpponentDot player opp =
  let
    -- 1. Tính vector vị trí tương đối (Opponent - Player)
    deltaVec = psPosition opp - psPosition player -- Phép trừ Vec2
    (dx, dy) = (vecX deltaVec, vecY deltaVec)

    -- 2. (Đã loại bỏ logic xoay)

    -- 3. Co dãn (Scale) từ World-space về Radar-space
    --    TỰ ĐỘNG DÙNG CÁC GIÁ TRỊ MỚI:
    --    scaleFactor = 140.0 / 350.0 (Lớn hơn trước)
    scaleFactor = radarDisplayRadius / radarMaxRange
    scaledX = dx * scaleFactor
    scaledY = dy * scaleFactor -- Y+ của map là Y+ của radar

    -- 4. Kẹp (Clamp) vị trí vào mép radar
    --    TỰ ĐỘNG DÙNG BÁN KÍNH MỚI:
    --    dist > 140.0
    dist = sqrt (scaledX*scaledX + scaledY*scaledY)
    (finalX, finalY) = if dist > radarDisplayRadius && dist > 0
                       then (scaledX * radarDisplayRadius / dist, scaledY * radarDisplayRadius / dist)
                       else (scaledX, scaledY)
  in
    -- Dịch chuyển chấm đỏ (KHÔNG bị scale bởi hàm renderRadar nữa)
    Translate finalX finalY (Color red (circleSolid 3.0))