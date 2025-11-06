{-# LANGUAGE DeriveGeneric #-}

module Types.Tank (TankType(..)) where

import GHC.Generics (Generic)
import Data.Binary (Binary)

-- Định nghĩa loại tank (Giữ nguyên)
data TankType
  = Rapid
  | Blast
  deriving (Show, Eq, Generic)

instance Binary TankType