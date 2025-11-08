{-# LANGUAGE DeriveGeneric #-}

module Types.MatchState where

import GHC.Generics (Generic)
import Data.Binary (Binary)
import Data.Maybe (Maybe)

-- | Định nghĩa các trạng thái của một trận đấu PvP
data MatchState
  = Waiting       -- Đang chờ người chơi (dưới 2 người)
  | InProgress    -- Trận đấu đang diễn ra
  | GameOver (Maybe Int) -- Trận đấu kết thúc, (Maybe Int) là ID của người thắng (Nothing nếu hòa)
  deriving (Show, Eq, Generic)

instance Binary MatchState