module Systems.EntitySystem (updatePlayerState) where

import Core.Types (RoomGameState(..)) -- << SỬA DÒNG NÀY
import Types.Player (PlayerState(..))
import qualified Data.Map as Map

-- << SỬA CHỮ KÝ HÀM
updatePlayerState :: PlayerState -> RoomGameState -> RoomGameState
updatePlayerState _newPlayerState gs = gs