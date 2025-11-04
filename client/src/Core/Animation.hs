module Core.Animation
  ( Animation(..)
  , updateAnimation
  , getCurrentFrame
  , isAnimationFinished
  , startAnimation
  ) where

import Graphics.Gloss (Picture(Blank))

data Animation = Animation
  { animFrames       :: [Picture]
  , animFrameTime    :: Float
  , animTimer        :: Float
  , animCurrentFrame :: Int
  , animLoops        :: Bool
  }

startAnimation :: Animation -> Animation
startAnimation anim
  | animLoops anim = anim
  | otherwise      = anim { animCurrentFrame = 0, animTimer = 0.0 }

updateAnimation :: Float -> Animation -> Animation
updateAnimation dt anim
  | null frames = anim 
  | not (animLoops anim) && animCurrentFrame anim >= frameCount = anim
  | otherwise =
      let
        newTimer = animTimer anim + dt
        framesToAdvance = floor (newTimer / animFrameTime anim)
      in
        if framesToAdvance == 0
          then anim { animTimer = newTimer }
          else
            let
              newFrameRaw = animCurrentFrame anim + framesToAdvance
              
              finalFrame
                | animLoops anim = newFrameRaw `mod` frameCount
                | otherwise      = min frameCount newFrameRaw
              
              finalTimer = newTimer - (fromIntegral framesToAdvance * animFrameTime anim)
            in
              anim { animCurrentFrame = finalFrame, animTimer = finalTimer }
  where
    frames = animFrames anim
    frameCount = length frames

getCurrentFrame :: Animation -> Picture
getCurrentFrame anim
  | null frames = Blank
  | not (animLoops anim) && idx >= frameCount = head frames 
  | otherwise = frames !! (idx `mod` frameCount)
  where
    frames = animFrames anim
    idx = animCurrentFrame anim
    frameCount = length frames

isAnimationFinished :: Animation -> Bool
isAnimationFinished anim =
  let
    frameCount = length (animFrames anim)
  in
    frameCount == 0 || (not (animLoops anim) && (animCurrentFrame anim >= frameCount))