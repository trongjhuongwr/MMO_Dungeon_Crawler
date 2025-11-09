{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Types.GameMode where

import GHC.Generics (Generic)
import Data.Binary (Binary)

data GameMode = PvP | PvE
  deriving (Eq, Show, Generic)

instance Binary GameMode