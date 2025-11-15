{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Types.GameMode where

import GHC.Generics (Generic)
import Data.Binary (Binary)

-- Định nghĩa các chế độ chơi
data GameMode = PvP | PvE
  deriving (Eq, Show, Generic)

instance Binary GameMode