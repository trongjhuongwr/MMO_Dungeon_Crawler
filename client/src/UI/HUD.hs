{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module UI.HUD (renderHUD) where

import Graphics.Gloss
import Types.Player (PlayerState(..))
import Renderer.Resources (Resources(..))

-- Tọa độ góc trên trái của màn hình
screenWidth, screenHeight :: Float
screenWidth = 800.0
screenHeight = 600.0

topLeftX, topLeftY :: Float
topLeftX = -(screenWidth / 2)
topLeftY = screenHeight / 2

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