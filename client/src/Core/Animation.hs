module Core.Animation
  ( Animation(..)
  , updateAnimation
  , getCurrentFrame
  , isAnimationFinished
  ) where

import Graphics.Gloss (Picture(Blank))

data Animation = Animation
  { animFrames       :: [Picture]
  , animFrameTime    :: Float     -- Thời gian cho mỗi khung hình (giây)
  , animTimer        :: Float     -- Bộ đếm thời gian nội bộ
  , animCurrentFrame :: Int       -- Chỉ số khung hình hiện tại
  , animLoops        :: Bool      -- Có lặp lại không?
  }

-- | Cập nhật trạng thái animation dựa trên delta time
updateAnimation :: Float -> Animation -> Animation
updateAnimation dt anim
  -- Guard 1: Không có frame, không làm gì
  | null frames = anim
  -- Guard 2: Đã xong (không lặp) và ở frame cuối, không làm gì
  | not (animLoops anim) && animCurrentFrame anim >= frameCount = anim
  -- Guard 3: Logic cập nhật chính
  | otherwise =
      let
        newTimer = animTimer anim + dt
        
        -- Tính toán có bao nhiêu frame đã trôi qua (có thể > 1 nếu lag)
        framesToAdvance = floor (newTimer / animFrameTime anim)
      in
        if framesToAdvance == 0
          -- Chưa đủ thời gian, chỉ cập nhật timer
          then anim { animTimer = newTimer }
          -- Đủ thời gian, tính frame và timer mới
          else
            let
              newFrameRaw = animCurrentFrame anim + framesToAdvance
              
              finalFrame
                | animLoops anim = newFrameRaw `mod` frameCount -- Quay vòng nếu lặp
                | otherwise      = min frameCount newFrameRaw -- Dừng ở frame_count (trạng thái "đã xong")
              
              -- Timer mới là số dư thời gian
              finalTimer = newTimer - (fromIntegral framesToAdvance * animFrameTime anim)
            in
              anim { animCurrentFrame = finalFrame, animTimer = finalTimer }
  where
    -- 'where' này áp dụng cho TẤT CẢ các guard ở trên
    frames = animFrames anim
    frameCount = length frames


-- | Lấy hình ảnh (Picture) của khung hình hiện tại
getCurrentFrame :: Animation -> Picture
getCurrentFrame anim
  | null frames = Blank
  | idx >= frameCount = frames !! (frameCount - 1) -- Giữ ở frame cuối nếu đã xong
  | otherwise = frames !! idx
  where
    frames = animFrames anim
    idx = animCurrentFrame anim
    frameCount = length frames

-- | Kiểm tra xem animation (không lặp) đã kết thúc chưa
isAnimationFinished :: Animation -> Bool
isAnimationFinished anim =
  let
    frameCount = length (animFrames anim)
  in
    -- Hoàn thành khi KHÔNG lặp VÀ frame hiện tại VƯỢT QUÁ (>=) frame cuối
    frameCount == 0 || (not (animLoops anim) && (animCurrentFrame anim >= frameCount))