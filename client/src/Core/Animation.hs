module Core.Animation
  ( Animation(..)
  , updateAnimation
  , getCurrentFrame
  , isAnimationFinished
  , startAnimation
  ) where

import Graphics.Gloss (Picture(Blank))

-- Lưu trữ trạng thái của một animation.
data Animation = Animation
  { animFrames       :: [Picture]
  , animFrameTime    :: Float
  , animTimer        :: Float
  , animCurrentFrame :: Int
  , animLoops        :: Bool
  }

-- Reset một animation về frame đầu tiên nếu nó không lặp lại.
startAnimation :: Animation -> Animation
startAnimation anim
  | animLoops anim = anim
  | otherwise      = anim { animCurrentFrame = 0, animTimer = 0.0 }

-- Cập nhật trạng thái của animation dựa trên thời gian đã trôi qua.
updateAnimation :: Float -> Animation -> Animation
updateAnimation dt anim
  | null frames = anim 
  | not (animLoops anim) && animCurrentFrame anim >= frameCount = anim
  | otherwise =
      let
        newTimer = animTimer anim + dt
        framesToAdvance = floor (newTimer / animFrameTime anim) -- Tính số frame cần nhảy qua
      in
        if framesToAdvance == 0
          then anim { animTimer = newTimer }
          else
            let
              newFrameRaw = animCurrentFrame anim + framesToAdvance
              
              finalFrame
                | animLoops anim = newFrameRaw `mod` frameCount
                | otherwise      = min frameCount newFrameRaw
              
              finalTimer = newTimer - (fromIntegral framesToAdvance * animFrameTime anim) -- Tính lại timer, giữ lại phần dư
            in
              anim { animCurrentFrame = finalFrame, animTimer = finalTimer }
  where
    frames = animFrames anim
    frameCount = length frames

-- Lấy ra ảnh của frame hiện tại để vẽ.
getCurrentFrame :: Animation -> Picture
getCurrentFrame anim
  | null frames = Blank
  | not (animLoops anim) && idx >= frameCount = head frames 
  | otherwise = frames !! (idx `mod` frameCount)
  where
    frames = animFrames anim
    idx = animCurrentFrame anim
    frameCount = length frames

-- Kiểm tra xem animation (không lặp) đã chạy xong hay chưa.
isAnimationFinished :: Animation -> Bool
isAnimationFinished anim =
  let
    frameCount = length (animFrames anim)
  in
    frameCount == 0 || (not (animLoops anim) && (animCurrentFrame anim >= frameCount))