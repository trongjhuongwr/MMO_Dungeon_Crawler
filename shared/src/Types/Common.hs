{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Common where

import Data.Binary (Binary)
import GHC.Generics (Generic)

-- Định nghĩa vector 2D
data Vec2 = Vec2
  { vecX :: Float
  , vecY :: Float
  } deriving (Show, Eq, Generic)

instance Binary Vec2

-- Cung cấp các phép toán cơ bản cho Vec2.
instance Num Vec2 where
  (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)  -- cộng 2 vec
  (-) :: Vec2 -> Vec2 -> Vec2
  (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)  -- trừ 2 vec
  (Vec2 x1 y1) * (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2)  -- nhân 2 vec 
  abs (Vec2 x y) = Vec2 (abs x) (abs y)                   -- trị tuyệt đối
  signum (Vec2 x y) = Vec2 (signum x) (signum y)          -- dấu của vec
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)    -- chuyển từ Integer sang Vec2

(*^) :: Vec2 -> Float -> Vec2                             -- nhân vô hướng 
(Vec2 x y) *^ s = Vec2 (x * s) (y * s)

-- Tính độ dài vector.
vecLength :: Vec2 -> Float
vecLength (Vec2 x y) = sqrt (x*x + y*y)