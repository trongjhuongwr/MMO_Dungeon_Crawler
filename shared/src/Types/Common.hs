{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Common where

import Data.Binary (Binary)
import GHC.Generics (Generic)

data Vec2 = Vec2
  { vecX :: Float
  , vecY :: Float
  } deriving (Show, Eq, Generic)

instance Binary Vec2

-- Cung cấp các phép toán cơ bản cho Vec2.
instance Num Vec2 where
  (Vec2 x1 y1) + (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  (-) :: Vec2 -> Vec2 -> Vec2
  (Vec2 x1 y1) - (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)
  (Vec2 x1 y1) * (Vec2 x2 y2) = Vec2 (x1 * x2) (y1 * y2) -- nhân 2 vec 
  abs (Vec2 x y) = Vec2 (abs x) (abs y)
  signum (Vec2 x y) = Vec2 (signum x) (signum y)
  fromInteger i = Vec2 (fromInteger i) (fromInteger i)

(*^) :: Vec2 -> Float -> Vec2 --Nhân vô hướng 
(Vec2 x y) *^ s = Vec2 (x * s) (y * s)

-- Tính độ dài vector.
vecLength :: Vec2 -> Float
vecLength (Vec2 x y) = sqrt (x*x + y*y)