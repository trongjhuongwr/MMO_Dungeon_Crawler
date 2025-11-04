module Core.Effect
  ( Effect(..)
  , updateEffect
  , isEffectFinished
  , makeExplosion
  ) where

import Core.Animation
import Types.Common (Vec2(..))
import Graphics.Gloss (Picture)

-- | Một hiệu ứng hình ảnh đang hoạt động trong game
data Effect = Effect
  { effId        :: Int       -- ID duy nhất (phía client)
  , effPosition  :: Vec2
  , effAnimation :: Animation
  }

-- | Hàm khởi tạo một hiệu ứng nổ
makeExplosion :: Int -> [Picture] -> Vec2 -> Effect
makeExplosion eid frames pos = Effect
  { effId = eid
  , effPosition = pos
  , effAnimation = Animation
      { animFrames = frames
      , animFrameTime = 0.05 -- 50ms mỗi frame (điều chỉnh nếu cần)
      , animTimer = 0
      , animCurrentFrame = 0
      , animLoops = False -- Không lặp lại
      }
  }

-- | Cập nhật animation của hiệu ứng
updateEffect :: Float -> Effect -> Effect
updateEffect dt effect =
  effect { effAnimation = updateAnimation dt (effAnimation effect) }

-- | Kiểm tra xem hiệu ứng đã chạy xong chưa
isEffectFinished :: Effect -> Bool
isEffectFinished = isAnimationFinished . effAnimation