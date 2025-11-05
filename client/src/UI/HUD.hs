module UI.HUD (renderHUD) where -- Sửa export

import Graphics.Gloss
import Types.Player (PlayerState(..))

-- Tọa độ góc trên trái của màn hình (ví dụ cửa sổ 800x600)
-- Gloss dùng tọa độ (0,0) ở giữa
screenWidth, screenHeight :: Float
screenWidth = 800.0
screenHeight = 600.0

topLeftX, topLeftY :: Float
topLeftX = -screenWidth / 2
topLeftY = screenHeight / 2

maxHealth :: Int
maxHealth = 100

-- Hàm render chính cho HUD
renderHUD :: PlayerState -> Picture
renderHUD player =
  let
    healthBar = drawHealthBar (psHealth player)
  in
    -- Dịch chuyển thanh máu lên góc trên trái
    Translate (topLeftX + 110) (topLeftY - 30) healthBar

-- Vẽ thanh máu
drawHealthBar :: Int -> Picture
drawHealthBar currentHP =
  let
    barWidth = 200.0
    barHeight = 20.0
    
    -- Tính toán độ rộng của phần máu đỏ
    healthRatio = (fromIntegral currentHP) / (fromIntegral maxHealth)
    currentWidth = barWidth * (max 0 (min 1 healthRatio)) -- Kẹp giá trị từ 0-1
    
    -- Thanh màu đỏ (máu hiện tại)
    healthPic = Color red $ rectangleSolid currentWidth barHeight
    
    -- Khung màu xám (nền)
    backgroundPic = Color (greyN 0.3) $ rectangleSolid barWidth barHeight
    
    -- Khung viền
    borderPic = Color white $ rectangleWire (barWidth + 2) (barHeight + 2)
    
  in
    Pictures
      [ -- Dịch chuyển nền và máu sang phải một chút để chúng căn lề trái
        Translate (barWidth / 2) 0 backgroundPic
      , Translate (currentWidth / 2) 0 healthPic
      , Translate (barWidth / 2) 0 borderPic
      ]