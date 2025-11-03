{-# LANGUAGE DeriveGeneric #-}

module Types.Enemy where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Types.Common (Vec2)

-- | Trạng thái của một con quái (mục tiêu tĩnh cho Giai đoạn 2)
data EnemyState = EnemyState
  { esId       :: Int     -- ID duy nhất
  , esPosition :: Vec2    -- Vị trí
  , esHealth   :: Int
  } deriving (Show, Eq, Generic)

instance Binary EnemyState